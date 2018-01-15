{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.Loop where

import qualified Data.ByteString as BSR

import Control.Monad
import Control.Monad.Loops
import Control.Exception

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Concurrent.Lock as LK

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString

import System.Timeout

import Tucker.Enc
import Tucker.Std
import Tucker.Msg
import Tucker.Atom
import Tucker.Conf
import Tucker.Error
import Tucker.Socket

-- an environment shared by a main loop
data MainLoopEnv =
    MainLoopEnv {
        btc_network :: BTCNetwork,
        timeout_s   :: Int, -- timeout in sec
        node_list   :: Atom [BTCNode],
        io_lock     :: LK.Lock
    }

data NodeAction = NoAction deriving (Show, Eq)

data BTCNode =
    BTCNode {
        conn_sock      :: Socket,
        addr           :: AddrInfo,
        incoming       :: Bool,

        vers_payload   :: Atom VersionPayload,

        sock_buf       :: Atom ByteString,
        msg_list       :: Atom [MsgHead], -- prepend

        thread_id      :: Atom ThreadId,
        action_list    :: Atom [NodeAction],

        alive          :: Atom Bool
    } deriving (Eq)

instance Show BTCNode where
    show node =
        "node on " ++ (show $ addrAddress $ addr node)

envMsg :: MainLoopEnv -> String -> IO ()
envMsg env msg = do
    LK.acquire (io_lock env)
    putStrLn $ "env: " ++ msg
    LK.release (io_lock env)
    return ()

node2netaddr :: BTCNode -> IO NetAddr
node2netaddr node = do
    let sock = conn_sock node
    sockaddr <- getSocketName sock

    vers <- getA (vers_payload node)

    case vers of
        VersionPayload {
            vers_serv = vers_serv
        } -> sockaddr2netaddr sockaddr vers_serv

        _ -> throw $ TCKRError "version message not ready"

nodeMsg :: MainLoopEnv -> BTCNode -> String -> IO ()
nodeMsg env node msg = do
    envMsg env $ (show node) ++ ": " ++ msg
    return ()

-- default actions
-- addr -> do nothing
-- getaddr -> return addr
-- version -> do nothing
-- verack -> do nothing
-- ping -> return pong

{-

things to implement:
1. address propagation & discovery(addr, getaddr)
2. inventory exchange(getblocks & inv)
    A: me, B: other

    for full nodes(x):
    A: getblocks
    B: inv
    A: getdata
    B: many notfound/block

    for SPV:
    A: getheaders
    B: headers

3. bloom filter for SPV
    A: filterload
    B: inv(maybe??)
(1) A: getdata
    B: merkleblock/tx

    A(if not satisfied): filteradd
    B: inv(maybe??)
    .. repeat from (1)

4. memory pool(x)

5. alert

6. tx broadcast
    A: inv + tx

-}

nodeProcMsg :: MainLoopEnv -> BTCNode -> MsgHead -> IO ()
nodeProcMsg env node msg = do
    -- case command msg of
    --     BTC_CMD_GETADDR ->
    return ()

nodeExec :: MainLoopEnv -> BTCNode -> IO ()
nodeExec env node = do
    let net = btc_network env
        sock = conn_sock node

    myThreadId >>= setA (thread_id node)

    nodeMsg env node "forked"

    handshake env node

    whileM_ (pure True) $ do
        msg <- nodeRecvOneMsg env node (return LackData)

        if msg /= LackData then do
            nodeMsg env node $ show msg
            nodeProcMsg env node msg
        else return ()

    return ()

nodeFinal :: MainLoopEnv -> BTCNode -> Either SomeException () -> IO ()
nodeFinal env node res = do
    case res of
        Right _ -> nodeMsg env node "exiting normally"
        Left err -> nodeMsg env node ("exiting in error: " ++ show err)

    setA (alive node) False
    close $ conn_sock node

nodeRecvOneMsg :: MainLoopEnv -> BTCNode -> IO MsgHead -> IO MsgHead
nodeRecvOneMsg env node timeout_proc = do
    let net = btc_network env
        sock = conn_sock node
        timeout_ms = timeout_s env * 1000000

    buf <- getA $ sock_buf node
    res <- timeout timeout_ms $ recvOneMsg sock buf

    case res of
        Nothing -> timeout_proc

        Just (msg, buf) ->
            case msg of
                Right msg@(MsgHead {}) ->
                    if magicno msg /= magicNo net then
                        throw $ TCKRError "magic no not match"
                    else do
                        -- update buffer
                        appA (msg:) (msg_list node)
                        setA (sock_buf node) buf
                        return msg

                Left err -> throw err
                _ -> error "impossible"

-- throw an exception when timeout
nodeExpectOneMsg :: MainLoopEnv -> BTCNode -> IO MsgHead
nodeExpectOneMsg env node =
    nodeRecvOneMsg env node (throw $ TCKRError "socket timeout")

handshake :: MainLoopEnv -> BTCNode -> IO ()
handshake env node = do
    let net = btc_network env
        sock = conn_sock node

    version <- encodeMsg net BTC_CMD_VERSION $ encodeVersionPayload net
    send sock version

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

                    send sock ack
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

---------------------------------------------------

bootstrap :: MainLoopEnv -> [String] -> IO [BTCNode]
bootstrap env hostnames = do
    addrs <- (mapM (seedLookup $ btc_network env) hostnames) >>= (pure . concat)
    probe env addrs

-- return alive address
-- timeout in seconds
probe :: MainLoopEnv -> [AddrInfo] -> IO [BTCNode]
probe env addrs = do
    let timeout_ms = timeout_s env * 1000000
        net = btc_network env

    res <- (flip mapM) addrs $ \addr -> do

        envMsg env ("probing " ++ (show $ addrAddress addr))

        sock <- buildSocketTo addr

        -- print (isSupportedSocketOption RecvTimeOut)
        -- setSocketTimeouts sock timeout_ms timeout_ms
        -- setSocketOption sock RecvTimeOut 1

        res <- timeout timeout_ms $ connect sock (addrAddress addr)

        case res of
            Nothing -> do
                envMsg env $ "connection timeout on " ++ (show $ addrAddress addr)
                return Nothing

            Just _ -> do
                vers_payload <- newA VersionPending -- placeholder
                
                sock_buf <- newA $ BSR.pack []
                msg_list <- newA []
                thread_id <- myThreadId >>= newA
                action_list <- newA []
                alive <- newA True

                let node = BTCNode {
                    conn_sock = sock,
                    addr = addr,
                    incoming = False,

                    vers_payload = vers_payload,

                    sock_buf = sock_buf,
                    msg_list = msg_list,
                    thread_id = thread_id,
                    action_list = action_list,
                    alive = alive
                }

                forkFinally (nodeExec env node) (nodeFinal env node)

                return $ Just node
            
    return $ map (\(Just x) -> x) $ filter filt res
    where
        filt (Just res) = True
        filt Nothing = False

initEnv :: BTCNetwork -> TCKRConf -> IO MainLoopEnv
initEnv net conf = do
    node_list <- newA []
    lock <- LK.new

    return $ MainLoopEnv {
        btc_network = net,
        timeout_s = tckr_socket_timeout conf,
        node_list = node_list,
        io_lock = lock
    }

mainLoop :: BTCNetwork -> TCKRConf -> IO ()
mainLoop net conf = do
    env <- initEnv net conf

    init_nodes <- bootstrap env (tckr_bootstrap_host conf)
    setA (node_list env) init_nodes

    -- fork to 3 sub-loops
    -- 1. seekerLoop: maintain the number of nodes
    -- 2. serverLoop: accept new incoming nodes
    -- 3. actionLoop: interact with user and send actions to nodes

    return ()
