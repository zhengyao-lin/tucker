{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.P2P.Server where

import qualified Data.ByteString as BSR

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Monad.Loops
import Control.Concurrent.Thread.Delay

import Network.Socket

import Tucker.Std
import Tucker.Msg
import Tucker.Enc
import Tucker.Conf
import Tucker.Atom
import Tucker.Error
import Tucker.Transport

import Tucker.P2P.Msg
import Tucker.P2P.Node
import Tucker.P2P.Util
import Tucker.P2P.Action

nodeDefaultActionHandler :: MainLoopEnv -> BTCNode -> MsgHead -> IO [RouterAction]
nodeDefaultActionHandler env node LackData = do
    delay 100000 -- 100ms
    return []

nodeDefaultActionHandler env node msg@(MsgHead {
        command = command,
        payload = payload
    }) = do
        h command where
            trans = conn_trans node
            net = btc_network env

            d :: MsgPayload t => (t -> IO [RouterAction]) -> IO [RouterAction]
            d = decodePayload env node payload (pure [])

            h BTC_CMD_PING = do
                d $ \ping@(PingPongPayload {}) -> do
                    pong <- encodeMsg net BTC_CMD_PONG $ pure $ encodeLE ping
                    nodeMsg env node $ "pinging back: " ++ (show pong)
                    timeoutS (timeout_s env) $ tSend trans pong
                    return []

            h BTC_CMD_GETADDR = do
                nodes       <- getA (node_list env)
                netaddrs    <- forM nodes nodeToNetAddr
                addr        <- encodeMsg net BTC_CMD_ADDR $ encodeAddrPayload [ v | Just v <- netaddrs ]

                nodeMsg env node $ "return addresses"

                timeoutS (timeout_s env) $ tSend trans addr

                return []

            h BTC_CMD_ADDR = do
                d $ \addrmsg@(AddrPayload {
                    addrs = netaddrs
                }) -> do
                    nodes       <- getA $ node_list env
                    seek_max    <- getEnvConf env tckr_seek_max

                    nodeMsg env node (show addrmsg)

                    if length nodes < seek_max then do
                        new_list <- forM netaddrs netAddrToAddrInfo
                        filted   <- filterProbingList env new_list

                        -- nodeMsg env node $ "filted: " ++ (show $ map addrAddress filted)

                        if not $ null filted then do
                            nodeMsg env node $ "probing " ++ (show $ length filted) ++ " new nodes"
                            forkIO $ probe env filted
                            return ()
                        else
                            return ()
                    else
                        nodeMsg env node "max number of nodes reached, no more probing"

                    return []

            h _ = do
                nodeMsg env node $ "unhandled message: " ++ (show command)
                return []

nodeDefaultAction = NormalAction nodeDefaultActionHandler

nodeDefaultActionList = [
        nodeDefaultAction
    ]

-- upon receiving a new message
-- nodeProcMsg route the message through the action_list
nodeProcMsg :: MainLoopEnv -> BTCNode -> MsgHead -> IO ()
nodeProcMsg env node msg = do
    -- prepend new actions
    new_alist <- getA $ new_action node
    appA (new_alist ++) (action_list node)
    
    current_alist <- getA $ action_list node

    -- processed action result is appended to exec_res
    -- continue indicates whether the execution is to be continued
    let proc = \orig@(exec_res, continue) action -> do
        if continue then do
            case action of
                NormalAction handler -> do
                    res <- handler env node msg

                    let update = [ a | UpdateMe a <- res ]
                        continue = StopProp `elem` res
                        new_action =
                            if DumpMe `elem` res then
                                Nothing
                            else if null update then 
                                Just action
                            else
                                Just $ last update

                    return (exec_res ++ [new_action], continue)

                _ -> error "unsupported action"
        else return orig

    (exec_res, _) <- foldM proc ([], True) current_alist

    let exec_count = length exec_res
        exec_alist = [ v | Just v <- exec_res ]
        new_alist = exec_alist ++ drop exec_count current_alist

    setA (action_list node) new_alist

    return ()

nodeExec :: MainLoopEnv -> BTCNode -> IO ()
nodeExec env node = do
    myThreadId >>= setA (thread_id node)

    nodeMsg env node "forked"

    timeoutFailS (timeout_s env) $ handshake env node

    whileM_ (pure True) $
        nodeRecvOneMsgNonBlocking env node >>=
        nodeProcMsg env node

nodeFinal :: MainLoopEnv -> BTCNode -> Either SomeException () -> IO ()
nodeFinal env node res = do
    case res of
        Right _ -> nodeMsg env node "exiting normally"
        Left err -> nodeMsg env node ("exiting in error: " ++ show err)

    setA (alive node) False
    tClose $ conn_trans node

nodeRecvOneMsg :: MainLoopEnv
               -> BTCNode
               -> (Transport -> ByteString -> IO (Either TCKRError MsgHead, ByteString))
               -> IO MsgHead
               -> IO MsgHead
nodeRecvOneMsg env node recv_proc timeout_proc = do
    buf         <- getA $ recv_buf node
    (msg, buf)  <- recv_proc (conn_trans node) buf

    case msg of
        Right msg@(MsgHead {}) ->
            if magicno msg /= magicNo (btc_network env) then
                throw $ TCKRError "network magic number not match"
            else do
                updateBuf buf
                appA (msg:) (msg_list node)
                return msg

        Right LackData -> updateBuf buf >> timeout_proc
        Left err -> throw err

    where
        updateBuf = setA (recv_buf node)

nodeRecvOneMsgTimeout :: MainLoopEnv -> BTCNode -> IO MsgHead -> IO MsgHead
nodeRecvOneMsgTimeout env node timeout_proc = do
    nodeRecvOneMsg env node ((flip tRecvOneMsg) (timeout_s env)) timeout_proc

nodeRecvOneMsgNonBlocking :: MainLoopEnv -> BTCNode -> IO MsgHead
nodeRecvOneMsgNonBlocking env node = do
    nodeRecvOneMsg env node tRecvOneMsgNonBlocking (return LackData)

-- throw an exception when timeout
nodeExpectOneMsg :: MainLoopEnv -> BTCNode -> IO MsgHead
nodeExpectOneMsg env node =
    nodeRecvOneMsgTimeout env node (throw $ TCKRError "recv timeout")

handshake :: MainLoopEnv -> BTCNode -> IO ()
handshake env node = do
    let net = btc_network env
        trans = conn_trans node

    netaddr <- ip4ToNetAddr "127.0.0.1" (listenPort net) btc_cli_service
    version <- encodeMsg net BTC_CMD_VERSION $
               encodeVersionPayload net netaddr

    -- send version
    tSend trans version

    -- waiting for peer's version
    MsgHead {
        command = BTC_CMD_VERSION,
        payload = payload
    } <- nodeExpectOneMsg env node
    
    -- decode peer's version and check
    decodePayload env node payload
        (throw $ TCKRError "version decode failed") $
        \vp@(VersionPayload {
            vers = vers,
            user_agent = VStr user_agent
        }) -> do
            nodeMsg env node $ "version received: " ++ (show vers) ++ user_agent
            setA (vers_payload node) vp

    -- Left err -> throw $ TCKRError $ "version decode failed: " ++ (show err)

    -- send verack
    ack <- encodeMsg net BTC_CMD_VERACK $ encodeVerackPayload
    tSend trans ack
    
    -- receive verack
    MsgHead {
        command = BTC_CMD_VERACK
    } <- nodeExpectOneMsg env node

    -- handshake finished
    
    nodeMsg env node "handshaking succeeded"

-- return alive address
-- timeout in seconds
probe :: MainLoopEnv -> [AddrInfo] -> IO ()
probe env addrs = do
    let net = btc_network env

    res <- (flip mapM) addrs $ \addr -> do
        envMsg env ("probing " ++ (show $ addrAddress addr))

        sock         <- buildSocketTo addr
        res          <- timeoutFailS (timeout_s env) $ connect sock (addrAddress addr)
        
        vers_payload <- newA VersionPending -- version placeholder
        trans        <- tFromSocket sock
        recv_buf     <- newA $ BSR.empty
        msg_list     <- newA []
        thread_id    <- myThreadId >>= newA
        action_list  <- newA nodeDefaultActionList
        new_action   <- newA []
        alive        <- newA True

        let node = BTCNode {
            conn_trans   = trans,
            addr         = addr,
            incoming     = False,

            vers_payload = vers_payload,

            recv_buf     = recv_buf,
            msg_list     = msg_list,
            thread_id    = thread_id,
            action_list  = action_list,
            new_action   = new_action,
            alive        = alive
        }

        forkFinally (nodeExec env node) (nodeFinal env node)

        appA (++ [node]) (node_list env)

        return $ Just node

    return ()
