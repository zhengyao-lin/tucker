module Tucker.P2P.Node where

import Data.List
import Data.Word
import Data.Hashable
import Data.Foldable as FD

import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Monad.Morph
import Control.Monad.Trans.Resource

import Network.Socket

import Tucker.DB
import Tucker.Enc
import Tucker.Msg
import Tucker.Conf
import Tucker.Atom
import Tucker.Util
import Tucker.Error
import Tucker.Crypto
import Tucker.Thread
import Tucker.Transport
import qualified Tucker.Lock as LK

import Tucker.State.Chain

import Tucker.P2P.Util

import Tucker.Wallet.Wallet

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

        -- io_lock       :: LK.Lock,
        io_buf        :: Atom [String],

        cur_socket    :: Atom Int,

        sync_ready    :: Atom Bool,

        chain_lock    :: LK.Lock,
        block_chain   :: Atom BlockChain,

        thread_state  :: ThreadState,

        wallets       :: [Wallet]
    }

data RouterAction
    = StopProp -- stop the propagation of the message
    | DumpMe -- dump the current handler
    | UpdateMe NodeAction -- update to a new action

instance Eq RouterAction where
    StopProp == StopProp = True
    DumpMe   == DumpMe   = True
    _        == _        = False

newtype NodeAction
    = NormalAction { handler :: ActionHandle }

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

instance Default Progress where
    def = Progress 0 0

data TransferState =
    TransferState {
        ping_delay         :: Word, -- in ms
        cur_progress       :: Progress, -- download progress on the current msg(when last_seen)
        last_seen          :: Timestamp,
        recv_buf           :: ByteString, -- currently downloaded part

        speed_test_begin   :: Timestamp,
        total_download     :: Word64, -- total download from speed_test_begin to now
        max_download_speed :: Word64 -- in bytes/s
    }

-- return if the ts1 is likely faster than ts2(GT) or other wise
compareTransState :: TransferState -> TransferState -> Ordering
compareTransState (TransferState {
    max_download_speed = ds1,
    ping_delay = ping1
}) (TransferState {
    max_download_speed = ds2,
    ping_delay = ping2
}) =
    case compare ds1 ds2 of
        EQ -> compare ping2 ping1 -- the result is reversed
        r -> r

instance Show TransferState where
    show ts = printf "TransferState {ping_delay=%v,cur_progress=%v,max_download_speed=%v}"
        (ping_delay ts) (show (cur_progress ts)) (max_download_speed ts)

instance Default TransferState where
    def = TransferState {
        ping_delay = maxBound,
        cur_progress = def,
        last_seen = 0,
        recv_buf = BSR.empty,

        speed_test_begin = 0,
        total_download = 0,
        max_download_speed = 0
    }

data Node =
    Node {
        conn_trans      :: Transport,
        inbound         :: Bool,

        thread_id       :: ThreadId,
        sock_addr       :: SockAddr,
        net_addr        :: NetAddr,
        vers_payload    :: VersionPayload,

        -- msg_list       :: Atom [MsgHead], -- prepend

        blacklist_time  :: Atom Int, -- how many times is the node blacklisted

        action_list     :: Atom [NodeAction],
        new_action      :: Atom [NodeAction],

        cur_trans_state :: Atom TransferState,

        alive           :: Atom Bool
    } | NullNode

instance Eq Node where
    (Node { thread_id = t1 }) == (Node { thread_id = t2 })
        = t1 == t2

    _ == _ = error "using null nodes"

instance Ord Node where
    compare (Node { thread_id = t1 }) (Node { thread_id = t2 })
        = compare t1 t2

    compare _ _ = error "using null nodes"

instance Hashable Node where
    hashWithSalt _ NullNode = error "using null nodes"
    hashWithSalt salt n = hashWithSalt salt (thread_id n)

instance Show Node where
    show NullNode = "(null node)"
    show node = "node on " ++ show (sock_addr node) -- ++ "(" ++ show (thread_id node) ++ ")"

-- used in spreading actions
-- a task can be anything specified for a action
-- that is going to be spreaded out
-- the task needs to be monoid for task combination
class Monoid t => NodeTask t where
    done :: MainLoopEnv -> Node -> t -> IO ()

taskFold :: NodeTask t => [t] -> Int -> [t]
taskFold ts n = map mconcat (foldList n ts)

data NullTask = NullTask

instance Monoid NullTask where
    mempty = NullTask
    mappend _ _ = NullTask

instance NodeTask NullTask where
    done e n t = return ()

envMsg :: MainLoopEnv -> String -> IO ()
envMsg env msg = do
    tLnM ("env: " ++ msg)

envWarn :: MainLoopEnv -> String -> IO ()
envWarn env msg =
    tLnM (wss (Color Yellow False) ("env: " ++ msg))

envErr :: MainLoopEnv -> String -> IO ()
envErr env msg =
    tLnM (wss (Color Red True) ("env: " ++ msg))

envInfo :: MainLoopEnv -> String -> IO ()
envInfo env msg =
    tLnM (wss (Color Green False) ("env: " ++ msg))

nodeMsg :: MainLoopEnv -> Node -> String -> IO ()
nodeMsg env node msg = envMsg env $ show node ++ ": " ++ msg

nodeWarn :: MainLoopEnv -> Node -> String -> IO ()
nodeWarn env node msg = envWarn env $ show node ++ ": " ++ msg

nodeErr :: MainLoopEnv -> Node -> String -> IO ()
nodeErr env node msg = envErr env $ show node ++ ": " ++ msg

nodeInfo :: MainLoopEnv -> Node -> String -> IO ()
nodeInfo env node msg = envInfo env $ show node ++ ": " ++ msg

envSetSyncReady :: MainLoopEnv -> Bool -> IO ()
envSetSyncReady env = setA (sync_ready env)

envIsSyncReady :: MainLoopEnv -> IO Bool
envIsSyncReady = getA . sync_ready

envWhenSyncReady :: MainLoopEnv -> IO () -> IO ()
envWhenSyncReady env action =
    envIsSyncReady env >>= flip when action

envWhenSyncReady_ :: MainLoopEnv -> IO a -> IO ()
envWhenSyncReady_ env action =
    envIsSyncReady env >>= flip when_ action

initEnv :: TCKRConf -> ResIO MainLoopEnv
initEnv conf = do
    -- tid <- lift myThreadId

    node_list <- lift $ newA []
    -- io_lock <- lift $ LK.new
    io_buf <- lift $ newA []

    cur_socket <- lift $ newA 0

    sync_ready <- lift $ newA False

    -- db_block <- openDB def (tckr_db_path conf) (tckr_ks_block conf)
    -- db_tx <- openDB def (tckr_db_path conf) (tckr_ks_tx conf)
    -- db_chain <- openDB def (tckr_db_path conf) (tckr_ks_chain conf)

    thread_state <- lift $ initThread conf

    wallets <-
        if tckr_enable_wallet conf then
            return <$> initWallet conf
        else
            return []

    chain_lock <- lift $ LK.new
    block_chain <- initBlockChain conf (Just thread_state) wallets >>= (lift . newA)

    return MainLoopEnv {
        global_conf = conf,

        timeout_s = tckr_trans_timeout conf,
        node_list = node_list,
        gc_interv = tckr_gc_interval conf,

        -- io_lock = io_lock,
        io_buf = io_buf,
        cur_socket = cur_socket,

        sync_ready = sync_ready,

        chain_lock = chain_lock,
        block_chain = block_chain,

        thread_state = thread_state,

        wallets = wallets

        -- db_block = db_block,
        -- db_tx = db_tx,
        -- db_chain = db_chain
    }

initNode :: SockAddr -> Transport -> IO Node
initNode sock_addr trans = do
    timestamp    <- unixTimestamp

    -- vers_payload <- newA VersionPending -- version placeholder
    -- recv_buf       <- newA $ BSR.empty
    -- msg_list     <- newA []
    blacklist_time <- newA 0
    action_list    <- newA [] -- nodeDefaultActionList
    new_action     <- newA []

    trans_state    <- newA def

    alive          <- newA True

    return $ Node {
        conn_trans      = trans,
        inbound         = isInbound trans,

        thread_id       = error "node tid not ready",
        sock_addr       = sock_addr,
        net_addr        = error "node net addr not ready",
        vers_payload    = error "version not ready",

        -- recv_buf       = recv_buf,
        -- msg_list     = msg_list,
        -- last_seen      = last_seen,
        -- cur_progress   = cur_progress,
        blacklist_time  = blacklist_time,

        action_list     = action_list,
        new_action      = new_action,

        -- ping_delay     = ping_delay,

        cur_trans_state = trans_state,

        alive           = alive
    }

envConf :: MainLoopEnv -> (TCKRConf -> t) -> t
envConf env field = field $ global_conf env

envFork = forkCap . thread_state
envForkFinally = forkCapFinally . thread_state
envForkMap__ = forkMap__ . thread_state
envForkMap = forkMap . thread_state

envThreadState = thread_state

envCloseTrans :: MainLoopEnv -> Transport -> IO ()
envCloseTrans env trans = do
    cur_s <- appA (+(-1)) (cur_socket env)
    tClose trans

    -- envMsg env ("transport closed, " ++ show cur_s ++ " left")

-- need timeout
envConnect :: MainLoopEnv -> AddrInfo -> IO Transport
envConnect env addr = do
    let sock_addr = addrAddress addr
        limit = envConf env tckr_max_socket

    cur_s <- appA (+1) (cur_socket env)

    if cur_s > limit then do
        appA (+(-1)) (cur_socket env)

        -- wait until an empty place is available and retry
        waitUntilIO ((< limit) <$> getA (cur_socket env))

        full <- envOutNodeFull env

        if not full then
            envConnect env addr
        else
            throwMT "node list is full"

        -- fail "number of sockets has reached the limit"
    else do
        -- envMsg env ("connecting to " ++ show sock_addr ++ "(" ++ show cur_s ++ " sockets)")

        sock <- buildSocketTo addr
        
        let conn = do
                timeoutFailS (timeout_s env) (connect sock sock_addr)
                tFromSocket sock False

        catchT conn $ \e -> do
            appA (+(-1)) (cur_socket env)
            close sock -- close the socket even if it's not connected
            throw e

nodeLastSeen :: Node -> IO Timestamp
nodeLastSeen node = last_seen <$> getA (cur_trans_state node)

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

nodeTransState :: Node -> IO TransferState
nodeTransState = getA . cur_trans_state

nodeChangeTransState :: Node -> (TransferState -> TransferState) -> IO ()
nodeChangeTransState node f = appA f (cur_trans_state node) >> return ()

nodeNetDelay :: Node -> IO Word
nodeNetDelay node = ping_delay <$> getA (cur_trans_state node)

-- this function will filter out uninit nodes
envAllNetDelay :: Integral t => MainLoopEnv -> IO [t]
envAllNetDelay env =
    getA (node_list env) >>=
    mapM nodeNetDelay >>=
    (return . map fi . filter (/= maxBound))

envAliveNodes :: MainLoopEnv -> IO [Node]
envAliveNodes env =
    getA (node_list env) >>= filterM (getA . alive)

envOutNodeFull :: MainLoopEnv -> IO Bool
envOutNodeFull env =
    (envConf env tckr_max_outbound_node <=) <$>
    length <$> filter (not . inbound) <$>
    envAliveNodes env

envInNodeFull :: MainLoopEnv -> IO Bool
envInNodeFull env =
    (envConf env tckr_max_inbound_node <=) <$>
    length <$> filter inbound <$>
    envAliveNodes env

nodeService :: Node -> NodeServiceType
nodeService = vers_serv . vers_payload

nodeHasService :: Node -> NodeServiceType -> Bool
nodeHasService node serv =
    nodeService node `serviceInclude` serv

-- spread actions to nodes except the ones in the black list
-- return [] if no available node is found
envSpreadActionExcept :: NodeTask t
                      => (Node -> Bool)
                      -> MainLoopEnv -> (t -> [NodeAction]) -> [t] -> IO [(Node, t)]
envSpreadActionExcept pred env gen_action tasks = do
    nodes <- getA (node_list env)

    valid_nodes <- flip filterM nodes $ \node -> do
        alive <- getA (alive node)
        tcount <- length <$> getA (action_list node)

        let cond = pred node

        -- let support_flag = nodeService node `serviceInclude` NodeServiceType node_flag

        -- limit the maximum task load on one node
        return $
            alive && cond &&
            tcount <= envConf env tckr_node_max_task + 1 -- base handler

    -- filter out dead nodes

    states <- mapM nodeTransState nodes
    -- delays <- mapM nodeNetDelay nodes

    -- envMsg env $ "blacklist: " ++ show blacklist

    let sorted = sortBy (\(d1, _) (d2, _) -> compareTransState d2 d1)
                        (zip states valid_nodes)
        sorted_nodes = map snd sorted

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
        forM_ assignment $ \(node, task) ->
            -- nodeMsg env node $ "prepending new action(s)"

            -- append new actions to each node
            nodePrependActions node (gen_action task)
            
        return assignment

envSpreadAction = envSpreadActionExcept (const True)

envSpreadSimpleAction :: MainLoopEnv -> NodeAction -> Int -> IO [(Node, NullTask)]
envSpreadSimpleAction env action n =
    envSpreadAction env (const [action]) (replicate n NullTask)

envBroadcastActionExcept :: (Node -> Bool) -> MainLoopEnv -> NodeAction -> IO ()
envBroadcastActionExcept pred env action = do
    nodes <- getA (node_list env)

    valid_nodes <- flip filterM nodes $ \node -> do
        alive <- getA (alive node)
        let cond = pred node
        return (alive && cond)

    forM_ valid_nodes $ \node ->
        nodePrependActions node [action]

envBroadcastAction = envBroadcastActionExcept (const True)

envAppendNode :: MainLoopEnv -> Node -> IO ()
envAppendNode env node =
    appA (++ [node]) (node_list env) >> return ()

envWithChain :: MainLoopEnv -> (BlockChain -> IO a) -> IO a
envWithChain env proc =
    LK.with (chain_lock env) (getA (block_chain env) >>= proc)

envSyncChain :: MainLoopEnv -> IO ()
envSyncChain env = envWithChain env syncBlockChain

envHasBlock :: MainLoopEnv -> Hash256 -> IO Bool
envHasBlock env hash = envWithChain env (`hasBlock` hash)

envFilterExistingBlock :: MainLoopEnv -> [Hash256] -> IO [Hash256]
envFilterExistingBlock env hashes =
    filterM ((not <$>) . envHasBlock env) hashes

envAddBlock :: MainLoopEnv -> Node -> Block -> IO ()
envAddBlock env node block =
    envAddBlocks env node [block]

envAddBlockIfNotExist :: MainLoopEnv -> Node -> Block -> IO Bool
envAddBlockIfNotExist env node block =
    LK.with (chain_lock env) $ do
        has <- envHasBlock env (block_hash block)

        if not has then do
            envAddBlock env node block
            return True
        else
            return False

nodeReject :: MainLoopEnv -> Node -> Command -> ByteString -> Rejection -> IO ()
nodeReject env node cmd dat rej = do
    rej <- encodeMsg (global_conf env) BTC_CMD_REJECT $
           encodeRejectPayload cmd dat rej

    tSend (conn_trans node) rej

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
                Left rej -> do
                    -- error ("error when adding block " ++ show block ++ ": " ++ show err)
                    nodeErr env node ("block rejected: " ++ show block ++ ": " ++ show rej)
                    nodeReject env node BTC_CMD_BLOCK (encodeLE (block_hash block)) rej

                    -- TODO: this is for test only
                    error (show rej)
                
                Right bc -> do
                    nodeMsg env node $
                        "added " ++ show block ++
                        "[" ++ show (mainBranchHeight bc) ++ "]"

envHasTx :: MainLoopEnv -> Hash256 -> IO Bool
envHasTx env hash = envWithChain env (`hasTx` hash)

envFilterExistingTx :: MainLoopEnv -> [Hash256] -> IO [Hash256]
envFilterExistingTx env hashes =
    filterM ((not <$>) . envHasTx env) hashes

envAddPoolTxIfNotExist :: MainLoopEnv -> Node -> TxPayload -> IO Bool
envAddPoolTxIfNotExist env node tx =
    envWithChain env $ \chain -> do
        has <- envHasTx env (txid tx)

        if not has then do
            res <- addPoolTx chain tx
            case res of
                Nothing -> do
                    nodeMsg env node ("added tx " ++ show (txid tx))
                    return True

                Just rej -> do
                    nodeErr env node ("tx verificaton failed: " ++ show (txid tx) ++ ": " ++ show rej)
                    nodeReject env node BTC_CMD_TX (encodeLE (txid tx)) rej
                    return False
        else
            return False

-- check if a specific soft fork is enabled
envForkEnabled :: MainLoopEnv -> String -> IO Bool
envForkEnabled env name =
    getA (block_chain env) >>= (`shouldEnableFork` name)

-- get node client info in string
nodeClientInfo :: Node -> String
nodeClientInfo node =
    let VersionPayload {
            cli_vers = vers,
            user_agent = user_agent,
            vers_serv = vers_serv
        } = vers_payload node

    in
        -- /<user-agent>/<vers>/<service>
        vstrToString user_agent ++ show vers ++ "/" ++ show vers_serv

nodeStartHeight :: Node -> Height
nodeStartHeight = fi . start_height . vers_payload

-- check if the client has a greater or equal chain height than
-- other clients on the network
-- NOTE: this can be only used in startup because tree height is not updated
envIsLongestChain :: MainLoopEnv -> IO Bool
envIsLongestChain env = do
    nodes <- getA (node_list env)
    bc <- getA (block_chain env)
    let height = mainBranchHeight bc

    return (all (<= height) (map nodeStartHeight nodes))

envMainBranchHeight :: MainLoopEnv -> IO Height
envMainBranchHeight env =
    mainBranchHeight <$> getA (block_chain env)

envTxPoolFull :: MainLoopEnv -> IO Bool
envTxPoolFull env = envWithChain env txPoolFull
