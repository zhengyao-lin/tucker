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

import Tucker.Storage.Chain

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
        block_chain   :: Atom BlockChain
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

-- progress of a download task
-- Progress received total
-- all in bytes
data Progress = Progress Integer Integer

instance Show Progress where
    show (Progress recv'd total) =
        if total >= 0 then
            let perc = fi recv'd / fi total in
            printf "progress %d/%d(%f) received" recv'd total (perc :: Double)
        else
            printf "progress %d received" recv'd

data Node =
    Node {
        conn_trans     :: Transport,
        incoming       :: Bool,

        thread_id      :: ThreadId,
        sock_addr      :: SockAddr,
        net_addr       :: NetAddr,
        vers_payload   :: VersionPayload,

        recv_buf       :: Atom ByteString,
        -- msg_list       :: Atom [MsgHead], -- prepend
        last_seen      :: Atom Word64,
        cur_progress   :: Atom Progress,
        -- download progress on the current msg(when last_seen)

        blacklist_time :: Atom Int, -- how many times is the node blacklisted

        action_list    :: Atom [NodeAction],
        new_action     :: Atom [NodeAction],

        ping_delay     :: Atom Word, -- in ms

        alive          :: Atom Bool
    }

instance Eq Node where
    (Node { thread_id = t1 }) == (Node { thread_id = t2 })
        = t1 == t2

instance Ord Node where
    compare (Node { thread_id = t1 }) (Node { thread_id = t2 })
        = compare t1 t2

instance Show Node where
    show node =
        "node on " ++ show (sock_addr node)

-- used in spreading actions
-- a task can be anything specified for a action
-- that is going to be spreaded out
-- the task needs to be monoid for task combination
class Monoid t => NodeTask t where
    done :: MainLoopEnv -> Node -> t -> IO ()

taskFold :: NodeTask t => [t] -> Int -> [t]
taskFold ts n =
    [
        mconcat $ take maxn $ drop (i * maxn) ts
        | i <- [ 0 .. t - 1 ]
    ] ++ [
        mconcat $ take minn $ drop (t * maxn + i * minn) ts
        | i <- [ 0 .. n - t - 1 ]
    ]
    where
        oldn = length ts
        -- foldn = old_n `divCeiling` n -- number of tasks to fold together
        maxn = oldn `divCeiling` n
        minn = oldn `divFloor` n

        t = oldn - minn * n

data NullTask = NullTask

instance Monoid NullTask where
    mempty = NullTask
    mappend _ _ = NullTask

instance NodeTask NullTask where
    done e n t = return ()

envMsg :: MainLoopEnv -> String -> IO ()
envMsg env msg = do
    -- force eval
    let msg' = BS.pack ("env: " ++ msg)

    LK.with (io_lock env) $ BS.putStrLn msg'

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
    block_chain <- initBlockChain conf >>= (lift . newA)

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
    recv_buf       <- newA $ BSR.empty
    -- msg_list     <- newA []
    last_seen      <- newA timestamp
    cur_progress   <- newA $ Progress 0 (-1)
    blacklist_time <- newA 0
    action_list    <- newA [] -- nodeDefaultActionList
    new_action     <- newA []
    ping_delay     <- newA maxBound -- max time in case the node doesn't reply
    alive          <- newA True

    return $ Node {
        conn_trans     = trans,
        incoming       = False,

        thread_id      = undefined,
        sock_addr      = sock_addr,
        net_addr       = undefined,
        vers_payload   = undefined,

        recv_buf       = recv_buf,
        -- msg_list     = msg_list,
        last_seen      = last_seen,
        cur_progress   = cur_progress,
        blacklist_time = blacklist_time,

        action_list    = action_list,
        new_action     = new_action,

        ping_delay     = ping_delay,

        alive          = alive
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

nodeBlacklistTime :: Node -> IO Int
nodeBlacklistTime = getA . blacklist_time

nodeBlacklistInc :: Node -> IO Int
nodeBlacklistInc = appA (+1) . blacklist_time

nodeBlacklistDec :: Node -> IO Int
nodeBlacklistDec = appA (\c -> if c > 0 then c - 1 else 0) . blacklist_time

-- nodeBlacklistDec :: Node -> IO ()
-- nodeBlacklistDec node =
--     appA (-1) (blacklist_time node)

nodePrependActions :: Node -> [NodeAction] -> IO ()
nodePrependActions node new_actions =
    appA (new_actions ++) (new_action node) >> return ()

nodeNetAddr :: Node -> IO NetAddr
nodeNetAddr = return . net_addr

nodeNetDelay :: Node -> IO Word
nodeNetDelay = getA . ping_delay

-- spread actions to nodes except the ones in the black list
-- return [] if no available node is found
envSpreadActionExcept :: NodeTask t
                      => [Node] -> MainLoopEnv -> (t -> [NodeAction]) -> [t] -> IO [(Node, t)]
envSpreadActionExcept blacklist env gen_action tasks = do
    nodes <- getA (node_list env)

    alive_nodes <- filterM (getA . alive) nodes
    -- filter out dead nodes

    delays <- mapM nodeNetDelay nodes

    -- envMsg env $ "blacklist: " ++ show blacklist

    let sorted = sortBy (\(d1, _) (d2, _) -> compare d1 d2)
                        (zip delays alive_nodes)
        sorted_nodes =
            filter (`notElem` blacklist) $
            map snd sorted

        taskn = length tasks
        noden = length sorted_nodes

    -- envMsg env $ show sorted

    if noden == 0 then return []
    else do
        let (target_nodes, new_tasks) =
                if noden < taskn then
                    -- no enough node
                    (sorted_nodes, taskFold tasks noden)
                else
                    -- great, we have enough nodes
                    -- simply take n nodes
                    (take taskn sorted_nodes, tasks)

            assignment = zip target_nodes new_tasks

        -- assume length target_nodes == length new_tasks
        forM_ assignment $ \(node, task) -> do
            -- nodeMsg env node $ "prepending new action(s)"

            -- append new actions to each node
            nodePrependActions node (gen_action task)
            
        return assignment

envSpreadAction = envSpreadActionExcept []

envSpreadSimpleAction :: MainLoopEnv -> NodeAction -> Int -> IO [(Node, NullTask)]
envSpreadSimpleAction env action n =
    envSpreadAction env (const [action]) (replicate n NullTask)

envAppendNode :: MainLoopEnv -> Node -> IO ()
envAppendNode env node =
    appA (++ [node]) (node_list env) >> return ()

envAddBlock :: MainLoopEnv -> Node -> Block -> IO ()
envAddBlock env node block =
    envAddBlocks env node [block]

-- removing explicit reference to the block list
-- NOTE: may help reduce space leaks?
envAddBlocks :: MainLoopEnv -> Node -> [Block] -> IO ()
envAddBlocks env node =
    (>>= after) .
    (before >>=) .
    (flip (addBlocks proc))

    where
        before = LK.acquire (chain_lock env) >> getA (block_chain env)
        after chain = setA (block_chain env) chain >> LK.release (chain_lock env)
        proc block res =
            case res of
                Left err ->
                    nodeMsg env node $ "error when adding block " ++ show block ++ ": " ++ show err
                
                Right _ ->
                    nodeMsg env node $ "block added: " ++ show block
