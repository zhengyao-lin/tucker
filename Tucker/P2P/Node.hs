module Tucker.P2P.Node where

import Data.List
import Data.Word
import Data.Foldable as FD
import qualified Data.Set as SET
import qualified Data.Set.Ordered as OSET

import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Concurrent
import Control.Monad.Morph
import Control.Monad.Trans.Resource
import qualified Control.Concurrent.Lock as LK

import Network.Socket

import Debug.Trace

import Tucker.DB
import Tucker.Enc
import Tucker.Msg
import Tucker.Conf
import Tucker.Atom
import Tucker.Util
import Tucker.Transport

import Tucker.Chain.Object

-- two parts
-- 1. main old tree, most common chain for all blocks
-- 2. side chains, side chains rooted from main tree

-- an environment shared among a main loop
data MainLoopEnv =
    MainLoopEnv {
        global_conf   :: TCKRConf,

        timeout_s     :: Int, -- timeout in sec
        node_list     :: Atom [Node],

        gc_interv     :: Integer, -- in ms

        io_lock       :: LK.Lock,
        io_buf        :: Atom [String],

        chain_lock    :: LK.Lock,
        block_chain   :: Atom Chain
    }

data RouterAction
    = StopProp -- stop the propagation of the message
    | DumpMe -- dump the current handler
    | UpdateMe NodeAction -- update to a new action

instance Eq RouterAction where
    StopProp == StopProp = True
    DumpMe   == DumpMe   = True
    _        == _        = False

data NodeAction
    = NormalAction { handler :: ActionHandle }
    | NoAction

type ActionHandle = MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]

data Node =
    Node {
        conn_trans     :: Transport,
        incoming       :: Bool,

        thread_id      :: ThreadId,
        sock_addr      :: SockAddr,
        net_addr       :: NetAddr,
        vers_payload   :: VersionPayload,

        recv_buf       :: Atom ByteString,
        msg_list       :: Atom [MsgHead], -- prepend
        last_seen      :: Atom Word64,

        action_list    :: Atom [NodeAction],
        new_action     :: Atom [NodeAction],

        ping_delay     :: Atom Word, -- in ms

        alive          :: Atom Bool
    }

instance Eq Node where
    (Node { thread_id = t1 }) == (Node { thread_id = t2 })
        = t1 == t2

instance Show Node where
    show node =
        "node on " ++ show (sock_addr node)

-- used in spreading actions
-- a task can be anything specified for a action
-- that is going to be spreaded out
-- the task needs to be monoid for task combination
class Monoid t => NodeTask t where

taskFold :: NodeTask t => [t] -> Int -> [t]
taskFold ts n =
    [
        mconcat $ take foldn $ drop (i * foldn) ts
        | i <- [ 0 .. n - 1 ]
    ]
    where
        old_n = length ts
        foldn = ceiling $ fromIntegral old_n / fromIntegral n -- number of tasks to fold together

data NullTask = NullTask

instance Monoid NullTask where
    mempty = NullTask
    mappend _ _ = NullTask

instance NodeTask NullTask

envMsg :: MainLoopEnv -> String -> IO ()
envMsg env msg = do
    -- force eval
    let msg' = BS.pack ("env: " ++ msg)

    LK.acquire (io_lock env)
    BS.putStrLn msg'
    LK.release (io_lock env)

    -- appA (++ [ "env: " ++ msg ]) (io_buf env)
    -- putStrLn' $ "env: " ++ msg

initEnv :: TCKRConf -> ResIO MainLoopEnv
initEnv conf = do
    node_list <- lift $ newA []
    io_lock <- lift $ LK.new
    io_buf <- lift $ newA []

    -- db_block <- openDB def (tckr_db_path conf) (tckr_ks_block conf)
    -- db_tx <- openDB def (tckr_db_path conf) (tckr_ks_tx conf)
    -- db_chain <- openDB def (tckr_db_path conf) (tckr_ks_chain conf)

    chain_lock <- lift $ LK.new
    block_chain <- initChain conf >>= (lift . newA)

    return $ MainLoopEnv {
        global_conf = conf,

        timeout_s = tckr_trans_timeout conf,
        node_list = node_list,
        gc_interv = tckr_gc_interval conf,

        io_lock = io_lock,
        io_buf = io_buf,

        chain_lock = chain_lock,
        block_chain = block_chain

        -- db_block = db_block,
        -- db_tx = db_tx,
        -- db_chain = db_chain
    }

initNode :: SockAddr -> Transport -> IO Node
initNode sock_addr trans = do
    timestamp    <- unixTimestamp

    -- vers_payload <- newA VersionPending -- version placeholder
    recv_buf     <- newA $ BSR.empty
    msg_list     <- newA []
    last_seen    <- newA timestamp
    action_list  <- newA [] -- nodeDefaultActionList
    new_action   <- newA []
    ping_delay   <- newA maxBound -- max time in case the node doesn't reply
    alive        <- newA True

    return $ Node {
        conn_trans   = trans,
        incoming     = False,

        thread_id    = undefined,
        sock_addr    = sock_addr,
        net_addr     = undefined,
        vers_payload = undefined,

        recv_buf     = recv_buf,
        msg_list     = msg_list,
        last_seen    = last_seen,
        action_list  = action_list,
        new_action   = new_action,

        ping_delay   = ping_delay,

        alive        = alive
    }

envConf :: MainLoopEnv -> (TCKRConf -> t) -> t
envConf env field = field $ global_conf env

envAllNode :: MainLoopEnv -> IO [Node]
envAllNode = getA . node_list

nodeMsg :: MainLoopEnv -> Node -> String -> IO ()
nodeMsg env node msg = do
    envMsg env $ (show node) ++ ": " ++ msg
    return ()

nodeLastSeen :: Node -> IO Word64
nodeLastSeen = getA . last_seen

nodePrependActions :: Node -> [NodeAction] -> IO ()
nodePrependActions node new_actions =
    appA (new_actions ++) (new_action node)

nodeNetAddr :: Node -> IO NetAddr
nodeNetAddr = return . net_addr

nodeNetDelay :: Node -> IO Word
nodeNetDelay = getA . ping_delay

-- spread actions to nodes
envSpreadAction :: NodeTask t => MainLoopEnv -> (t -> [NodeAction]) -> [t] -> IO ()
envSpreadAction env gen_action tasks = do
    nodes <- getA (node_list env)

    alive_nodes <- filterM (getA . alive) nodes
    -- filter out dead nodes

    delays <- mapM nodeNetDelay nodes

    let sorted = sortBy (\(d1, _) (d2, _) -> compare d1 d2)
                        (zip delays alive_nodes)
        sorted_nodes = map snd sorted

        taskn = length tasks
        noden = length sorted_nodes

    envMsg env $ show sorted

    let (target_nodes, new_tasks) =
            if noden < taskn then
                -- no enough node
                (sorted_nodes, taskFold tasks noden)
            else
                -- great, we have enough nodes
                -- simply take n nodes
                (take taskn sorted_nodes, tasks)

    -- assume length target_nodes == length new_tasks
    forM (zip target_nodes new_tasks) $ \(node, task) -> do
        nodeMsg env node $ "prepending new action(s)"

        -- append new actions to each node
        nodePrependActions node (gen_action task)
        
    return ()

envSpreadSimpleAction :: MainLoopEnv -> NodeAction -> Int -> IO ()
envSpreadSimpleAction env action n =
    envSpreadAction env (const [action]) [ NullTask | _ <- [ 1 .. n ] ]

envAppendNode :: MainLoopEnv -> Node -> IO ()
envAppendNode env node =
    appA (++ [node]) (node_list env)
