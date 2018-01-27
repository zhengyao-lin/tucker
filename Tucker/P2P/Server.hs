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
import Tucker.Util
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
                    -- nodeMsg env node $ "pinging back: " ++ (show pong)
                    timeoutS (timeout_s env) $ tSend trans pong
                    return []

            h BTC_CMD_GETADDR = do
                nodes       <- getA (node_list env)
                net_addrs   <- forM nodes nodeNetAddr
                addr        <- encodeMsg net BTC_CMD_ADDR $ encodeAddrPayload net_addrs

                -- nodeMsg env node $ "return addresses"

                timeoutS (timeout_s env) $ tSend trans addr

                return []

            h BTC_CMD_ADDR = do
                d $ \addrmsg@(AddrPayload {
                    addrs = net_addrs
                }) -> do
                    nodes       <- getA $ node_list env
                    seek_max    <- getEnvConf env tckr_seek_max

                    -- nodeMsg env node (show addrmsg)

                    if length nodes < seek_max then do
                        new_list <- forM net_addrs netAddrToAddrInfo
                        filted   <- filterProbingList env new_list

                        -- nodeMsg env node $ "filted: " ++ (show $ map addrAddress filted)

                        if not $ null filted then do
                            nodeMsg env node $ "probing " ++ (show $ length filted) ++ " new node(s)"
                            forkIO $ probe env filted
                            return ()
                        else
                            return ()
                    else
                        nodeMsg env node "max number of nodes reached, no more probing"

                    return []

            h BTC_CMD_BLOCK = do
                d $ \msg -> do
                    envAddIdleBlock env node msg
                    return []

            h _ = do
                nodeMsg env node $ "unhandled message: " ++ (show command)
                return []

nodeDefaultAction = NormalAction nodeDefaultActionHandler

-- NOTE:
-- when trying to sync block chain,
-- exec fetchBlock on at least <a> nodes
-- then wait until a certain span(or the tree has reached a certain height)
-- then re-exec the fetchBlock on other <a> nodes

nodeDefaultActionList = [
        -- NormalAction fetchBlock, -- for test
        nodeDefaultAction
    ]

-- upon receiving a new message
-- nodeProcMsg route the message through the action_list
nodeProcMsg :: MainLoopEnv -> BTCNode -> MsgHead -> IO ()
nodeProcMsg env node msg = do
    -- prepend new actions
    new_alist <- getA $ new_action node
    setA (new_action node) []
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
nodeExec env unready_node = do
    nodeMsg env unready_node "spawn"

    -- update thread id
    tid <- myThreadId
    let node = unready_node { thread_id = tid }
    
    -- update after handshake
    node <- timeoutFailS (timeout_s env) $ handshake env node

    -- set default action
    nodePrependAction node nodeDefaultActionList

    -- officially inserting the node
    envAppendNode env node

    -- enter event loop
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

handshake :: MainLoopEnv -> BTCNode -> IO BTCNode
handshake env node = do
    let net = btc_network env
        trans = conn_trans node

    net_addr <- ip4ToNetAddr "127.0.0.1" (listenPort net) btc_cli_service
    version <- encodeMsg net BTC_CMD_VERSION $
               encodeVersionPayload net net_addr

    -- send version
    tSend trans version

    -- waiting for peer's version
    MsgHead {
        command = BTC_CMD_VERSION,
        payload = payload
    } <- nodeExpectOneMsg env node
    
    -- decode peer's version and check

    let decode = doDecode (decoder :: Decoder VersionPayload) LittleEndian

    case decode payload of
        (Left err, _) ->
            throw $ TCKRError $ "version decode failed: " ++ show err

        (Right vp@(VersionPayload {
            vers = vers,
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
            ack <- encodeMsg net BTC_CMD_VERACK $ encodeVerackPayload
            tSend trans ack
            
            -- receive verack
            MsgHead {
                command = BTC_CMD_VERACK
            } <- nodeExpectOneMsg env new_node

            -- handshake finished
            nodeMsg env node "handshaking succeeded"

            return new_node

-- return alive address
-- timeout in seconds
probe :: MainLoopEnv -> [AddrInfo] -> IO ()
probe env addrs = do
    let net = btc_network env

    res <- (flip mapM) addrs $ \addr -> (try $ do
        let sock_addr = addrAddress addr

        envMsg env ("probing " ++ show sock_addr)

        sock         <- buildSocketTo addr
        _            <- timeoutFailS (timeout_s env) $ connect sock sock_addr

        trans <- tFromSocket sock
        node <- initNode sock_addr trans

        forkFinally (nodeExec env node) (nodeFinal env node)

        -- appA (++ [node]) (node_list env)
        -- return $ Just node

        return ()) :: IO (Either SomeException ())

    return ()
