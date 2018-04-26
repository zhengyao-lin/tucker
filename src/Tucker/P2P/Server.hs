module Tucker.P2P.Server where

import qualified Data.ByteString as BSR

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Monad.Loops
import Control.Concurrent.Thread.Delay

import Network.Socket

import Tucker.Msg
import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Atom
import Tucker.Error
import Tucker.Transport

import Tucker.P2P.Msg
import Tucker.P2P.Node
import Tucker.P2P.Util
import qualified Tucker.P2P.Action as A

-- NOTE:
-- when trying to sync block chain,
-- exec fetchBlock on at least <a> nodes
-- then wait until a certain span(or the tree has reached a certain height)
-- then re-exec the fetchBlock on other <a> nodes

defaultHandler :: MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
defaultHandler env node (LackData _) = do
    delay 100000 -- 100ms
    return []

defaultHandler env node msg@(MsgHead {
        command = command,
        payload = payload
    }) = h command
    where
        trans = conn_trans node
        conf = global_conf env

        d :: MsgPayload t => (t -> IO [RouterAction]) -> IO [RouterAction]
        d = decodePayload env node payload (pure [])

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

        h BTC_CMD_GETHEADERS = do
            return []
        
        h BTC_CMD_ALERT = do
            return []

        h BTC_CMD_ADDR = do
            d $ \addrmsg@(AddrPayload {
                addrs = net_addrs
            }) -> do
                nodes <- getA $ node_list env
                full  <- envNodeFull env

                new_list <- forM net_addrs netAddrToAddrInfo
                filted   <- filterProbingList env new_list

                if full || null filted then
                    return ()
                else do
                    -- try to probe new nodes
                    forkIO $ probe env filted
                    return ()

                return []

        h BTC_CMD_BLOCK = do
            d $ \(BlockPayload block) -> do
                error "new block?"
                envAddBlock env node block
                return []

        h BTC_CMD_INV = do
            d $ \(InvPayload inv_vect) ->
                let first@(InvVector itype _) = head inv_vect
                in case itype of
                    INV_TYPE_BLOCK -> do -- new block received
                        ready <- envIsSyncReady env

                        if ready then do
                            envInfo env ("new block(s) received: " ++ show first ++ ", ...")
                            error "not yet implemented"
                        else
                            envMsg env "ignoring new block(s) due to unfinished sync process"

                        return []

                    _ -> do
                        envWarn env ("unknown inventory received: " ++ show first ++ ", ...")
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

    if not full then do
        -- officially inserting the node
        envAppendNode env node

        -- enter receiving loop
        whileM_ (pure True) $ do
            -- nodeMsg env node "node loop"
            nodeRecvOneMsgNonBlocking env node >>= nodeProcMsg env node
    else
        return ()

nodeFinal :: MainLoopEnv -> Node -> Either SomeException () -> IO ()
nodeFinal env node res = do
    case res of
        Right _ -> nodeMsg env node "exiting normally"
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
    version <- encodeMsg conf BTC_CMD_VERSION $
               encodeVersionPayload conf net_addr

    -- send version
    tSend trans version

    -- waiting for peer's version
    MsgHead {
        command = BTC_CMD_VERSION,
        payload = payload
    } <- nodeExpectOneMsg env node
    
    -- decode peer's version and check

    let decode = doDecode (decoder :: Decoder VersionPayload) LittleEndian

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

    case decode payload of
        (Left err, _) ->
            throw $ TCKRError $ "version decode failed: " ++ show err ++ ", " ++ show payload

        (Right vp@(VersionPayload {
            cli_vers = vers,
            user_agent = VStr user_agent,
            vers_serv = vers_serv
        }), _) -> do
            nodeMsg env node $ "version received: " ++ (show vers) ++ user_agent

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

            return new_node

-- return alive address
-- timeout in seconds
probe :: MainLoopEnv -> [AddrInfo] -> IO ()
probe env addrs =
    flip forkMapM__ addrs $ \addr -> do
        trans <- envConnect env addr
        node <- initNode (addrAddress addr) trans

        forkFinally (nodeExec env node) (nodeFinal env node)

        return ()
