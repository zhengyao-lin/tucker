module Tucker.P2P.Node where

import Data.Word
import qualified Data.Set as SET
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.Lock as LK

import Network.Socket

import Tucker.Std
import Tucker.Enc
import Tucker.Msg
import Tucker.Conf
import Tucker.Atom
import Tucker.Util
import Tucker.Transport

import Tucker.Chain.Object

-- an environment shared among a main loop
data MainLoopEnv =
    MainLoopEnv {
        btc_network   :: BTCNetwork,
        timeout_s     :: Int, -- timeout in sec
        node_list     :: Atom [BTCNode],

        gc_interv     :: Integer, -- in ms
    
        global_conf   :: TCKRConf,

        fetched_block :: Atom (SET.Set Hash256),

        io_lock       :: LK.Lock,
        io_buf        :: Atom [String],

        tree_lock     :: LK.Lock,
        block_tree    :: Atom BlockTree,
        idle_block    :: Atom (SET.Set BlockPayloadHashed)
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

type ActionHandle = MainLoopEnv -> BTCNode -> MsgHead -> IO [RouterAction]

data BTCNode =
    BTCNode {
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

        alive          :: Atom Bool
    }

instance Show BTCNode where
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

envCurrentTreeHeight :: MainLoopEnv -> IO Int
envCurrentTreeHeight env = getA (block_tree env) >>= (return . treeHeight)

envDumpIdleBlock :: MainLoopEnv -> IO [Hash256]
envDumpIdleBlock env =
    getA (idle_block env) >>=
    (return . map (\(BlockPayloadHashed hash _) -> hash) . SET.toList)

envDumpReceivedBlock :: MainLoopEnv -> IO [Hash256]
envDumpReceivedBlock env =
    getA (fetched_block env) >>= (return . SET.toList)

envMsg :: MainLoopEnv -> String -> IO ()
envMsg env msg = do
    -- force eval
    let msg' = BS.pack ("env: " ++ msg)

    LK.acquire (io_lock env)
    BS.putStrLn msg'
    LK.release (io_lock env)

    -- appA (++ [ "env: " ++ msg ]) (io_buf env)
    -- putStrLn' $ "env: " ++ msg

initEnv :: BTCNetwork -> TCKRConf -> IO MainLoopEnv
initEnv net conf = do
    node_list <- newA []
    io_lock <- LK.new
    io_buf <- newA []
    fetched <- newA SET.empty
    tree_lock <- LK.new
    block_tree <- newA emptyTree
    idle_block <- newA SET.empty

    let genesis@(BlockPayload {
            header = header
        }) = case decodeLE (genesisRaw net) of
            (Left err, _) ->
                error $ "fatal: genesis block decoding error: " ++ show err
            (Right gen, _) -> gen

        bph = BlockPayloadHashed (hashBlockHeader header) genesis

    case insertToTree emptyTree bph of
        Left err ->
            error $ "fatal: illegal genesis block: " ++ show err
        
        Right new_tree ->
            setA block_tree new_tree

    return $ MainLoopEnv {
        btc_network = net,
        timeout_s = tckr_trans_timeout conf,
        node_list = node_list,
        gc_interv = tckr_gc_interval conf,

        global_conf = conf,

        io_lock = io_lock,
        io_buf = io_buf,

        fetched_block = fetched,
        tree_lock = tree_lock,
        block_tree = block_tree,
        idle_block = idle_block
    }

initNode :: SockAddr -> Transport -> IO BTCNode
initNode sock_addr trans = do
    timestamp    <- unixTimestamp

    -- vers_payload <- newA VersionPending -- version placeholder
    recv_buf     <- newA $ BSR.empty
    msg_list     <- newA []
    last_seen    <- newA timestamp
    action_list  <- newA [] -- nodeDefaultActionList
    new_action   <- newA []
    alive        <- newA True

    return $ BTCNode {
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
        alive        = alive
    }

getEnvConf :: MainLoopEnv -> (TCKRConf -> t) -> IO t
getEnvConf env field = do
    return $ field $ global_conf env

envAddFetchedBlock :: MainLoopEnv -> Hash256 -> IO ()
envAddFetchedBlock env hash = do
    appA (SET.insert hash) (fetched_block env)

envHasFetchedBlock :: MainLoopEnv -> Hash256 -> IO Bool
envHasFetchedBlock env hash = do
    fetched <- getA (fetched_block env)

    return $ case SET.lookupIndex hash fetched of
        Just _ -> True
        Nothing -> False

envAddIdleBlock :: MainLoopEnv -> BTCNode -> BlockPayload -> IO ()
envAddIdleBlock env node payload@(BlockPayload {
    header = header,
    Tucker.Msg.txns = txns
}) = do
    let hash = hashBlockHeader header
        bph = BlockPayloadHashed hash payload

    -- 1. try to insert to the tree
    -- 2. if cannot, push it to the idle block

    has_fetched <- envHasFetchedBlock env hash

    if has_fetched then
        nodeMsg env node $ "block received again: " ++ (show hash)
    else do
        nodeMsg env node $ "block received: " ++ (show hash) -- ++ " " ++ (show payload)
            
        -- add to the set
        appA (SET.insert hash) (fetched_block env)
        appA (SET.insert bph) (idle_block env)

insertAction :: MainLoopEnv -> NodeAction -> IO ()
insertAction env action = do
    nodes <- getA $ node_list env

    forM nodes $ \node -> do
        alive <- getA $ alive node
        if alive then
            appA (action:) (new_action node)
        else
            return ()

    return ()

nodeMsg :: MainLoopEnv -> BTCNode -> String -> IO ()
nodeMsg env node msg = do
    envMsg env $ (show node) ++ ": " ++ msg
    return ()

nodeLastSeen :: BTCNode -> IO Word64
nodeLastSeen = getA . last_seen

nodePrependAction :: BTCNode -> [NodeAction] -> IO ()
nodePrependAction node new_actions =
    appA (new_actions ++) (new_action node)

nodeNetAddr :: BTCNode -> IO NetAddr
nodeNetAddr = return . net_addr

-- spread actions to nodes
envSpreadAction :: NodeTask t => MainLoopEnv -> (t -> [NodeAction]) -> [t] -> IO ()
envSpreadAction env gen_action tasks = do
    nodes <- getA (node_list env)
    alive_nodes <- filterM (getA . alive) nodes
    -- filter out dead nodes

    let taskn = length tasks
        noden = length alive_nodes

    let (target_nodes, new_tasks) =
            if noden < taskn then
                -- no enough node
                (alive_nodes, taskFold tasks noden)
            else
                -- great, we have enough nodes
                -- simply take n nodes
                (take taskn alive_nodes, tasks)

    -- assume length target_nodes == length new_tasks
    forM (zip target_nodes new_tasks) $ \(node, task) -> do
        nodeMsg env node $ "prepending new action(s)"

        -- append new actions to each node
        nodePrependAction node (gen_action task)
        
    return ()

envSpreadSimpleAction :: MainLoopEnv -> NodeAction -> Int -> IO ()
envSpreadSimpleAction env action n =
    envSpreadAction env (const [action]) [ NullTask | _ <- [ 1 .. n ] ]

envAppendNode :: MainLoopEnv -> BTCNode -> IO ()
envAppendNode env node =
    appA (++ [node]) (node_list env)
