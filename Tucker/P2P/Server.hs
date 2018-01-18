{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.P2P.Server where

import qualified Data.ByteString as BSR

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Monad.Loops
import Control.Concurrent.Thread.Delay

import Network.Socket
import System.Timeout

import Tucker.Std
import Tucker.Msg
import Tucker.Enc
import Tucker.Conf
import Tucker.Atom
import Tucker.Error
import Tucker.P2P.Msg
import Tucker.P2P.Node
import Tucker.P2P.Util
import Tucker.Transport

nodeDefaultActionHandler :: MainLoopEnv -> BTCNode -> MsgHead -> IO [RouterAction]
nodeDefaultActionHandler env node LackData = do
    delay 100000 -- 100ms
    return []

nodeDefaultActionHandler env node msg@(MsgHead {
        command = command,
        payload = payload
    }) = do
        h command where
            sock = conn_trans node
            net = btc_network env
            timeout_us = timeout_s env * 1000000

            d :: MsgPayload t => (t -> IO [RouterAction]) -> IO [RouterAction]
            d = decodePayload env node payload

            h BTC_CMD_PING = do
                d $ \ping@(PingPongPayload {}) -> do
                    pong <- encodeMsg net BTC_CMD_PONG $ pure $ encodeLE ping

                    nodeMsg env node $ "pinging back: " ++ (show pong)

                    timeout timeout_us $ tSend sock pong
                    
                    return []

            h BTC_CMD_GETADDR = do
                nodes <- getA (node_list env)

                netaddrs <- forM nodes nodeToNetAddr
                let tmp = map (\(Just v) -> v) . filter (/= Nothing) $ netaddrs

                addr <- encodeMsg net BTC_CMD_ADDR $ encodeAddrPayload tmp

                nodeMsg env node $ "return addresses"

                timeout timeout_us $ tSend sock addr

                return []

            h BTC_CMD_ADDR = do
                d $ \addrmsg@(AddrPayload {
                    addrs = netaddrs
                }) -> do
                    nodes <- getA $ node_list env
                    seek_max <- getEnvConf env tckr_seek_max

                    nodeMsg env node (show addrmsg)

                    if length nodes < seek_max then do
                        new_list <- forM netaddrs netAddrToAddrInfo

                        -- nodeMsg env node $ "new probing: " ++ (show $ map addrAddress addrinfo)
                        -- nodeMsg env node $ "already has: " ++ (show $ map addrAddress exist_addrinfo)

                        filted <- filterProbingList env new_list

                        -- nodeMsg env node $ "filted: " ++ (show $ map addrAddress filted)

                        if length filted /= 0 then do
                            nodeMsg env node $ "probing " ++ (show $ length filted) ++ " new nodes"
                            forkIO $ probe env filted

                            return ()
                        else
                            return ()
                    else do
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
    -- case command msg of
    --     BTC_CMD_GETADDR ->

    -- prepend new actions
    new_alist <- getA $ new_action node
    appA (new_alist ++) (action_list node)
    
    current_alist <- getA $ action_list node

    -- tmp_res :: [m (Maybe NodeAction, Bool)]
    let tmp_res = (flip map) current_alist $ \action -> do
        case action of
            NormalAction handler -> do
                res <- handler env node msg

                new_action <- newA $ Just action
                continue <- newA True

                forM res $ \res -> do
                    case res of
                        StopProp -> do
                            setA continue False -- stop propagation

                        DumpMe -> do
                            setA new_action Nothing

                        UpdateMe new_a -> do
                            setA new_action $ Just new_a

                new_action <- getA new_action
                continue <- getA continue

                return (new_action, continue)

            _ -> error "unsupported action"

    (exec_res, _) <- concatM (map (\m -> \orig@(exec_res, continue) -> do
        if continue then do
            res@(_, continue) <- m
            return (exec_res ++ [res], continue)
        else return orig) tmp_res) ([], True)

    -- exec_res <- (flip takeWhileM) tmp_res $ \m -> do
    --     (new_action, continue) <- m
    --     return continue

    let exec_count = length exec_res
        exec_alist =
            map (\(Just v) -> v) .
            filter (\v -> case v of
                Nothing -> False
                _ -> True) .
            map fst $
            exec_res
    
    -- new list = old updated action list + rest action list

    let new_alist = exec_alist ++ drop exec_count current_alist

    setA (action_list node) new_alist

    return ()

nodeExec :: MainLoopEnv -> BTCNode -> IO ()
nodeExec env node = do
    let net = btc_network env
        sock = conn_trans node

    myThreadId >>= setA (thread_id node)

    nodeMsg env node "forked"

    handshake env node

    whileM_ (pure True) $ do
        msg <- nodeRecvOneMsgNonBlocking env node

        -- if msg /= LackData then do
        --     nodeProcMsg env node msg
        -- else return ()

        nodeProcMsg env node msg

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
    let net = btc_network env
        trans = conn_trans node

    buf <- getA $ recv_buf node
    (msg, buf) <- recv_proc trans buf

    case msg of
        Right msg@(MsgHead {}) ->
            if magicno msg /= magicNo net then
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

    tSend trans version

    let recv_version = do
            msg <- nodeExpectOneMsg env node

            case msg of
                MsgHead {
                    command = BTC_CMD_VERSION,
                    payload = payload
                } -> do
                    ack <- encodeMsg net BTC_CMD_VERACK $ encodeVerackPayload
                    
                    let (res, _) = doDecode (decoder :: Decoder VersionPayload) LittleEndian payload

                    case res of
                        Right vp@(VersionPayload {
                            vers = vers,
                            user_agent = VStr user_agent
                        }) -> do
                            nodeMsg env node $ "version received: " ++ (show vers) ++ user_agent
                            setA (vers_payload node) vp
                        
                        Left err -> throw $ TCKRError $ "version decode failed: " ++ (show err)

                    tSend trans ack
                    recv_verack

                    return ()

        recv_verack = do
            msg <- nodeExpectOneMsg env node
            case msg of
                MsgHead {
                    command = BTC_CMD_VERACK
                } -> do
                    nodeMsg env node "handshaking succeeded"
                    return ()
    
    recv_version

-- return alive address
-- timeout in seconds
probe :: MainLoopEnv -> [AddrInfo] -> IO ()
probe env addrs = do
    let timeout_us = timeout_s env * 1000000
        net = btc_network env

    res <- (flip mapM) addrs $ \addr -> do

        envMsg env ("probing " ++ (show $ addrAddress addr))

        sock <- buildSocketTo addr

        -- print (isSupportedSocketOption RecvTimeOut)
        -- setSocketTimeouts sock timeout_us timeout_us
        -- setSocketOption sock RecvTimeOut 1

        res <- timeout timeout_us $ connect sock (addrAddress addr)

        case res of
            Nothing -> do
                envMsg env $ "connection timeout on " ++ (show $ addrAddress addr)
                return Nothing

            Just _ -> do
                vers_payload <- newA VersionPending -- placeholder
                
                trans <- tFromSocket sock
                recv_buf <- newA $ BSR.empty
                msg_list <- newA []
                thread_id <- myThreadId >>= newA
                action_list <- newA nodeDefaultActionList
                new_action <- newA []
                alive <- newA True

                let node = BTCNode {
                    conn_trans = trans,
                    addr = addr,
                    incoming = False,

                    vers_payload = vers_payload,

                    recv_buf = recv_buf,
                    msg_list = msg_list,
                    thread_id = thread_id,
                    action_list = action_list,
                    new_action = new_action,
                    alive = alive
                }

                forkFinally (nodeExec env node) (nodeFinal env node)

                appA (++ [node]) (node_list env)

                return $ Just node
            
    -- return $ map (\(Just x) -> x) $ filter filt res
    
    return ()

    where
        filt (Just res) = True
        filt Nothing = False
