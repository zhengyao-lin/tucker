module Tucker.P2P.Server where

import qualified Data.ByteString as BSR

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Monad.Loops

import Network.Socket

import Tucker.Msg
import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Atom
import Tucker.Error
import Tucker.Thread
import Tucker.Transport

import Tucker.P2P.Msg
import Tucker.P2P.Node
import Tucker.P2P.Util
import qualified Tucker.P2P.Action as A

import Tucker.Storage.Chain
import Tucker.Storage.Block

-- NOTE:
-- when trying to sync block chain,
-- exec fetchBlock on at least <a> nodes
-- then wait until a certain span(or the tree has reached a certain height)
-- then re-exec the fetchBlock on other <a> nodes

defaultHandler :: MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
defaultHandler env node (LackData _) = do
    msDelay 200
    return []

defaultHandler env node msg@(MsgHead {
        command = command,
        payload = payload
    }) = h command
    where
        trans = conn_trans node
        conf = global_conf env

        nBlocksFrom :: Int -> [Hash256] -> Hash256 -> IO [Block]
        nBlocksFrom n locators stop_hash =
            envWithChain env $ \bc -> do
                res <- mapM (lookupMainBranchBlock bc . WithHash) locators
                
                let cur_height = mainBranchHeight bc
                    begin_height =
                        case first isJust res of
                            Just (Just (height, _)) -> height + 1
                            Nothing -> 1 -- start right after the genesis

                    range = take n [ begin_height .. cur_height ]

                res <- mapM (lookupMainBranchBlock bc . AtHeight) range

                let hashes = map snd (maybeCat res)
                    final = takeWhile ((/= stop_hash) . block_hash) hashes

                return final

        d :: MsgPayload t => (t -> IO [RouterAction]) -> IO [RouterAction]
        d = decodePayload env node command payload (pure [])

        h BTC_CMD_PING = do
            d $ \ping@(PingPongPayload {}) -> do
                pong <- encodeMsg conf BTC_CMD_PONG $ pure $ encodeLE ping
                -- nodeMsg env node $ "pinging back: " ++ (show pong)
                timeoutS (timeout_s env) $ tSend trans pong
                return []

        h BTC_CMD_GETADDR = do
            nodes       <- getA (node_list env)
            net_addrs   <- forM nodes nodeNetAddr
            addr        <- encodeMsg conf BTC_CMD_ADDR $ encodeAddrPayload net_addrs

            -- nodeMsg env node $ "return addresses"

            timeoutS (timeout_s env) $ tSend trans addr

            return []

        -- h BTC_CMD_GETHEADERS = return []
        
        h BTC_CMD_ALERT = return []

        h BTC_CMD_ADDR =
            d $ \addrmsg@(AddrPayload {
                addrs = net_addrs
            }) -> do
                nodes <- getA $ node_list env
                full  <- envNodeFull env

                new_list <- forM net_addrs netAddrToAddrInfo
                filted   <- filterProbingList env new_list

                -- tLnM (show (new_list, filted))

                unless (full || null filted) $ do
                    -- try to probe new nodes
                    envFork env THREAD_OTHER (probe env filted)
                    return ()

                return []

        h BTC_CMD_BLOCK =
            d $ \(BlockPayload block) -> do
                ready <- envIsSyncReady env

                if ready then do
                    added <- envAddBlockIfNotExist env node block

                    when added $ do
                        inv <- encodeMsg conf BTC_CMD_INV $
                               encodeInvPayload [InvVector INV_TYPE_BLOCK (block_hash block)]

                        -- propagate inv to nodes other than the current one
                        envBroadcastActionExcept (/= node) env (A.sendMsgA inv)
                else
                    nodeInfo env node "an unsolicited block received"

                return []

        h BTC_CMD_TX =
            d $ \tx -> do
                envWhenSyncReady env $ do
                    added <- envAddPoolTxIfNotExist env node tx

                    -- broadcast tx
                    when added $ do
                        nodeInfo env node ("broadcasting tx " ++ show (txid tx))

                        inv <- encodeMsg conf BTC_CMD_INV $
                               encodeInvPayload [InvVector INV_TYPE_TX (txid tx)]

                        -- propagate inv to nodes other than the current one
                        envBroadcastActionExcept (/= node) env (A.sendMsgA inv)

                return []

        h BTC_CMD_GETDATA =
            d $ \(InvPayload invs) -> do
                let -- return Just hash for blocks that have not been found
                    lookupBlockAndSend :: (Block -> Block) -> Hash256 -> IO (Maybe Hash256)
                    lookupBlockAndSend preproc hash =
                        envWithChain env $ \bc -> do
                            res <- lookupMainBranchBlock bc (WithHash hash)
                            case res of
                                Just (_, block) -> do
                                    res <- getFullBlock bc block
                                    case res of
                                        Nothing -> return (Just hash)
                                        Just block -> do
                                            -- send back the block
                                            msg <- encodeMsg conf BTC_CMD_BLOCK $
                                                   encodeBlockPayload (preproc block)
                                            tSend trans msg

                                            return Nothing

                                Nothing -> return (Just hash)

                    lookupTxAndSend :: (TxPayload -> TxPayload) -> Hash256 -> IO (Maybe Hash256)
                    lookupTxAndSend preproc txid =
                        envWithChain env $ \bc -> do
                            res <- lookupMemPool bc txid

                            case res of
                                Nothing -> return (Just txid)
                                Just tx -> do
                                    msg <- encodeMsg conf BTC_CMD_TX $
                                           encodeTxPayload tx
                                    tSend trans msg

                                    return Nothing

                notfounds' <-
                    forM invs $ \(InvVector htype hash) -> do
                        res <- case htype of
                            INV_TYPE_BLOCK -> lookupBlockAndSend stripBlockWitness hash
                            INV_TYPE_WITNESS_BLOCK -> lookupBlockAndSend id hash
                            INV_TYPE_TX -> lookupTxAndSend stripWitness hash
                            INV_TYPE_WITNESS_TX -> lookupTxAndSend id hash

                        return (InvVector htype <$> res)

                let notfounds = maybeCat notfounds'

                unless (null notfounds) $ do
                    -- inform node about not-found objects
                    notfound <- encodeMsg conf BTC_CMD_NOTFOUND $
                                encodeNotfoundPayload notfounds

                    tSend trans notfound

                return []

        h BTC_CMD_GETBLOCKS =
            d $ \(GetblocksPayload {
                locator = locators,
                stop_hash = stop_hash
            }) -> do
                envWhenSyncReady_ env $
                    envFork env THREAD_OTHER $ do
                        nodeInfo env node ("node requested getblocks with locators: " ++ show locators)

                        hashes <-
                            map block_hash <$>
                            nBlocksFrom (tckr_max_getblocks_batch conf) locators stop_hash

                        -- reply with hashes
                        unless (null hashes) $ do
                            inv <- encodeMsg conf BTC_CMD_INV $
                                   encodeInvPayload (map (InvVector INV_TYPE_BLOCK) hashes)
                            tSend trans inv

                return []

        h BTC_CMD_GETHEADERS =
            d $ \(GetblocksPayload {
                locator = locators,
                stop_hash = stop_hash
            }) -> do
                -- nodeInfo env node ("node requested getheaders with locators: " ++ show locators)
                envWhenSyncReady_ env $ do
                    envFork env THREAD_OTHER $ do
                        headers <-
                            map BlockHeader <$>
                            nBlocksFrom (tckr_max_getheaders_batch conf) locators stop_hash

                        nodeInfo env node ("returning headers of size " ++ show (length headers))

                        headers <- encodeMsg conf BTC_CMD_HEADERS $
                                   encodeHeadersPayload headers
                        tSend trans headers

                return []

        -- h BTC_CMD_HEADERS =

        h BTC_CMD_MEMPOOL =
            envWithChain env $ \bc -> do
                txids <- map txid <$> allMemPoolTxns bc

                inv <- encodeMsg conf BTC_CMD_INV $
                       encodeInvPayload (map (InvVector INV_TYPE_TX) txids)

                tSend trans inv

                return []

        h BTC_CMD_INV =
            d $ \(InvPayload inv_vect) -> do
                if null inv_vect then
                    nodeWarn env node "peer sending empty inventory"
                else do
                    let hashes = map invToHash256 inv_vect
                        trans = conn_trans node
                        first@(InvVector itype _) = head inv_vect
                    
                    case itype of
                        INV_TYPE_BLOCK -> do -- new block received
                            ready <- envIsSyncReady env

                            if ready then do
                                nodeInfo env node ("new block(s) received: " ++ show first ++ ", ...")

                                -- filter out existing blocks
                                hashes <- envFilterExistingBlock env hashes

                                -- request for the block
                                A.getFullDataMsg env INV_TYPE_BLOCK hashes >>= tSend trans
                            else
                                nodeMsg env node "ignoring new block(s) due to unfinished sync process"

                        INV_TYPE_TX ->
                            envWhenSyncReady env $ do
                                nodeInfo env node ("new txns received: " ++ show first ++ ", ...")

                                -- filter out existing txns
                                hashes <- envFilterExistingTx env hashes

                                -- request for full txns
                                A.getFullDataMsg env INV_TYPE_TX hashes >>= tSend trans

                        _ ->
                            nodeWarn env node ("unknown inventory received: " ++ show first ++ ", ...")

                return []

        h BTC_CMD_REJECT =
            d $ \rej@(RejectPayload {
                message = message,
                ccode = ccode,
                reason = reason,
                rdata = rdata
            }) -> do
                case message of
                    "tx" -> do
                        let dres = decodeAllLE rdata
                            txid =
                                case dres :: Either TCKRError Hash256 of
                                    Right hash -> show hash
                                    Left err -> "(unparsed data)"

                        nodeWarn env node ("rejected tx " ++ txid ++ ": " ++ show ccode ++ " " ++ show reason)

                    _ ->
                        nodeWarn env node ("unknown rejection: " ++ show rej)

                return []

        h _ = do
            nodeMsg env node $ "unhandled message: " ++ (show command)
            return []

nodeDefaultActionList = [
        -- NormalAction fetchBlock, -- for test
        NormalAction A.pingDelay, -- measure delay
        NormalAction A.seekNode,
        NormalAction defaultHandler
    ]

-- passes the new message along the action handler list
-- and adjust the new action list by need(e.g. delete certain action)
nodeProcMsg :: MainLoopEnv -> Node -> MsgHead -> IO ()
nodeProcMsg env node msg = do
    -- prepend new actions
    new_alist <- getA $ new_action node
    setA (new_action node) []
    
    current_alist <- appA (new_alist ++) (action_list node)

    -- processed action result is appended to exec_res
    -- continue indicates whether the execution is to be continued
    let proc = \orig@(exec_res, continue) action -> do
            if continue then
                case action of
                    NormalAction handler -> do
                        res <- handler env node msg

                        let update = [ a | UpdateMe a <- res ]
                            continue = StopProp `notElem` res
                            new_action =
                                if DumpMe `elem` res then
                                    Nothing
                                else if null update then 
                                    Just action
                                else
                                    Just $ last update

                        return (exec_res ++ [new_action], continue)

            else return orig

    (exec_res, _) <- foldM proc ([], True) current_alist

    let exec_count = length exec_res
        exec_alist = maybeCat exec_res
        new_alist = exec_alist ++ drop exec_count current_alist

    setA (action_list node) new_alist

    return ()

nodeExec :: MainLoopEnv -> Node -> IO ()
nodeExec env unready_node = do
    -- nodeMsg env unready_node "spawn"

    -- update thread id
    tid <- myThreadId
    let node = unready_node { thread_id = tid }
    
    -- update after handshake
    node <- timeoutFailS (timeout_s env) $ handshake env node

    -- set default action
    nodePrependActions node nodeDefaultActionList

    full <- envNodeFull env

    unless full $ do
        -- officially inserting the node
        envAppendNode env node

        -- enter receiving loop
        whileM_ (pure True) $ do
            -- nodeMsg env node "node loop"
            nodeRecvOneMsgNonBlocking env node >>= nodeProcMsg env node

nodeFinal :: MainLoopEnv -> Node -> Either SomeException () -> IO ()
nodeFinal env node res = do
    case res of
        Right _ -> nodeMsg env node "normal exit"
        Left err -> nodeMsg env node ("exiting in error: " ++ show err)

    setA (alive node) False
    envCloseTrans env (conn_trans node)

handshake :: MainLoopEnv -> Node -> IO Node
handshake env node = do
    let conf = global_conf env
        trans = conn_trans node

    net_addr <- ip4ToNetAddr "127.0.0.1"
                (tckr_listen_port conf)
                (tckr_node_service conf)

    cur_height <- envMainBranchHeight env

    version <- encodeMsg conf BTC_CMD_VERSION $
               encodeVersionPayload conf cur_height net_addr

    -- send version
    tSend trans version

    -- waiting for peer's version
    MsgHead {
        command = BTC_CMD_VERSION,
        payload = payload
    } <- nodeExpectOneMsg env node
    
    -- decode peer's version and check
    {-
    
    7D110100 version
    4D00000000000000 service
    A849B45A00000000 timestamp
    0100000000000000 00000000000000000000FFFF7011F705 6DEB
    4D00000000000000 00000000000000000000000000000000 0000
    431386780C502A6B nonce
    1B 2F627463776972653A302E352E302F627463643A302E31322E302F
    E1AA1300 start height

    -}

    case decodeLE payload of
        (Left err, _) ->
            throw $ TCKRError $ "version decode failed: " ++ show err ++ ", " ++ show payload

        (Right vp@(VersionPayload {
            cli_vers = vers,
            user_agent = user_agent,
            vers_serv = vers_serv
        }), _) -> do
            net_addr <- sockAddrToNetAddr (sock_addr node) vers_serv

            -- update vp
            let new_node = node {
                vers_payload = vp,
                net_addr = net_addr
            }

            -- send verack
            ack <- encodeMsg conf BTC_CMD_VERACK $ encodeVerackPayload
            tSend trans ack
            
            -- receive verack
            MsgHead {
                command = BTC_CMD_VERACK
            } <- nodeExpectOneMsg env new_node

            -- handshake finished
            -- nodeMsg env node "handshaking succeeded"

            nodeMsg env node $ "handshaked: " ++ nodeClientInfo new_node

            return new_node

-- start a server to accept incoming nodes
server :: MainLoopEnv -> IO ()
server env = do
    let conf = global_conf env
        port = tckr_listen_port conf

    addr <- ipToAddrInfo (tckr_listen_addr conf) (fi port)
    sock <- buildSocketTo addr

    bind sock (addrAddress addr)
    listen sock (fi port)

    let -- main server loop
        loop sock = do
            -- wait until there are spare places for incoming nodes
            waitUntilIO $
                (< tckr_max_incoming_conn conf) <$> envCountIncomingNodes env

            (conn, addr) <- accept sock
            trans <- tFromSocket conn True
            node <- initNode addr trans

            envInfo env ("incoming node: " ++ show addr)

            envForkFinally env THREAD_NODE (nodeExec env node) (nodeFinal env node)

            loop sock

    envInfo env "server started"
    loop sock

-- return alive address
-- timeout in seconds
probe :: MainLoopEnv -> [AddrInfo] -> IO ()
probe env addrs =
    flip (envForkMap__ env THREAD_OTHER) addrs $ \addr -> do
        trans <- envConnect env addr
        node <- initNode (addrAddress addr) trans

        envForkFinally env THREAD_NODE (nodeExec env node) (nodeFinal env node)

        return ()
