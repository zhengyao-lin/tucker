-- utilities

module Tucker.P2P.Util where

import Data.Word
import qualified Data.ByteString as BSR

import Network.Socket
import System.Timeout

import Control.Exception
import Control.Monad.Loops

import Tucker.Enc
import Tucker.Msg
import Tucker.Util
import Tucker.Atom
import Tucker.Conf
import Tucker.Error
import Tucker.Transport

import Tucker.P2P.Msg
import Tucker.P2P.Node

ip4ToIP6 :: ByteString -> ByteString
ip4ToIP6 addrv4 =
    BSR.append (BSR.pack pref) addrv4
    where
        pref = [ 0x00, 0x00, 0x00, 0x00,
                 0x00, 0x00, 0x00, 0x00,
                 0x00, 0x00, 0xff, 0xff ]

ip4ToNetAddr :: String -> Word16 -> NodeServiceType -> IO NetAddr
ip4ToNetAddr addr port serv = do
    time <- unixTimestamp
    enc <- inet_addr addr
    let addrv6 = ip4ToIP6 $ encodeBE $ ntohl enc
    pure $ NetAddr {
        time = time,
        net_serv = serv,
        ipv6o4 = addrv6,
        port = port
    }

ipToAddr :: String -> Word16 -> IO AddrInfo
ipToAddr ip port =
    (getAddrInfo (Just defaultHints {
        addrSocketType = Stream
        -- addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
    }) (Just ip) (Just $ show port)) >>= (pure . head)

-- AF_INET SOCK_STREAM (addrSocketType addr) 

buildSocketTo :: AddrInfo -> IO Socket
buildSocketTo addr =
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

-- look up node seeds using a domain
seedLookup :: TCKRConf -> String -> IO [AddrInfo]
seedLookup conf host =
    getAddrInfo (Just defaultHints {
        addrSocketType = Stream
    }) (Just host) (Just $ show $ tckr_listen_port conf)

-- try to trim a message from the received data
-- if a complete message is found, return (Right MsgHead {}, rest of data)
-- if the message is incomplete, return (Right LackData, original data)
-- if the message if incorrect, return (Left err, original data)
-- trimMsg :: ByteString -> (Either TCKRError MsgHead, ByteString)
-- trimMsg = decodeLE

sockAddrToNetAddr :: SockAddr -> NodeServiceType -> IO NetAddr

sockAddrToNetAddr (SockAddrInet port host) serv = do
    time <- unixTimestamp
    pure $ NetAddr {
        time = time,
        net_serv = serv,
        ipv6o4 = ip4ToIP6 $ encodeBE (fromIntegral host :: Word32),
        port = fromIntegral port
    }

sockAddrToNetAddr (SockAddrInet6 port _ (h1, h2, h3, h4) _) serv = do
    time <- unixTimestamp
    pure $ NetAddr {
        time = time,
        net_serv = serv,
        ipv6o4 = BSR.concat $ [
            encodeBE (fromIntegral h1 :: Word32),
            encodeBE (fromIntegral h2 :: Word32),
            encodeBE (fromIntegral h3 :: Word32),
            encodeBE (fromIntegral h4 :: Word32)
        ],
        port = fromIntegral port
    }

netAddrToAddrInfo :: NetAddr -> IO AddrInfo
netAddrToAddrInfo netaddr = do
    let sockaddr6 =
            SockAddrInet6 (fromIntegral $ port netaddr) 0 (
                ntohl $ fromIntegral $ decodeInt 4 BigEndian h1,
                ntohl $ fromIntegral $ decodeInt 4 BigEndian h2,
                ntohl $ fromIntegral $ decodeInt 4 BigEndian h3,
                ntohl $ fromIntegral $ decodeInt 4 BigEndian h4
            ) 0
        
        sockaddr4 =
            SockAddrInet (fromIntegral $ port netaddr) (ntohl $ fromIntegral $ decodeInt 4 BigEndian h4)

    if is_ip4 || not (isSupportedSockAddr sockaddr6) then
        return $ defaultHints {
            addrSocketType = Stream,
            addrFamily = AF_INET,
            addrAddress = sockaddr4
        }
    else -- use ip6
        return $ defaultHints {
            addrSocketType = Stream,
            addrFamily = AF_INET6,
            addrAddress = sockaddr6
        }

    where
        ip = ipv6o4 netaddr
        h1 = BSR.take 4 $ BSR.drop 0 ip
        h2 = BSR.take 4 $ BSR.drop 4 ip
        h3 = BSR.take 4 $ BSR.drop 8 ip
        h4 = BSR.take 4 $ BSR.drop 12 ip

        is_ip4 = ip4ToIP6 h4 == ip

-- nodeNetAddr :: Node -> IO (Maybe NetAddr)
-- nodeNetAddr node = do
--     let sockaddr = addrAddress $ addr node
--     vers <- getA $ vers_payload node
--     alive <- getA $ alive node

--     if vers == VersionPending || not alive then
--         -- error "version not ready(no handshake?)"
--         return Nothing
--     else do
--         netaddr <- sockAddrToNetAddr sockaddr (vers_serv vers)
--         return $ Just $ netaddr

isSameIP :: SockAddr -> SockAddr -> Bool
isSameIP (SockAddrInet _ h1) (SockAddrInet _ h2) = h1 == h2
isSameIP (SockAddrInet6 _ _ h1 _) (SockAddrInet6 _ _ h2 _) = h1 == h2
isSameIP _ _ = False

filterProbingList :: MainLoopEnv -> [AddrInfo] -> IO [AddrInfo]
filterProbingList env new_list = do
    nodes <- getA $ node_list env

    let bl = envConf env tckr_node_blacklist
        exist_sockaddr = map sock_addr nodes
        blacklist = exist_sockaddr ++ bl

    return $ filter (\a -> all (not . isSameIP (addrAddress a)) blacklist) new_list

decodePayload :: MsgPayload t
              => MainLoopEnv
              -> Node
              -> ByteString
              -> IO a
              -> (t -> IO a)
              -> IO a

decodePayload env node payload fail proc = do
    case decodeAllLE payload of
        Left err -> do
            nodeMsg env node $ "payload decoding error: " ++ (show err)
            fail

        Right v -> proc v

secondToMicrosecond :: (Integral a, Integral b) => a -> b
secondToMicrosecond = fromIntegral . (* 1000000)

timeoutS :: Int -> IO a -> IO (Maybe a)
timeoutS sec = timeout (secondToMicrosecond sec)

timeoutFailS :: Int -> IO a -> IO a
timeoutFailS sec action = do
    res <- timeoutS sec action

    case res of
        Nothing -> throw $ TCKRError "action timeout"
        Just v -> return v

timeoutRetryS :: Int -> IO a -> IO a
timeoutRetryS sec action = untilJust $ timeoutS sec action

nodeRecvOneMsg :: MainLoopEnv
               -> Node
               -> (Transport -> ByteString -> IO (Either TCKRError MsgHead, ByteString))
               -> IO MsgHead
               -> IO MsgHead
nodeRecvOneMsg env node recv_proc timeout_proc = do
    buf         <- getA $ recv_buf node
    (msg, buf)  <- recv_proc (conn_trans node) buf

    case msg of
        Right msg@(MsgHead {}) ->
            if magicno msg /= envConf env tckr_magic_no then
                throw $ TCKRError "network magic number not match"
            else do
                updateBuf buf
                appA (msg:) (msg_list node)

                timestamp <- unixTimestamp
                setA (last_seen node) timestamp

                return msg

        Right LackData -> updateBuf buf >> timeout_proc
        Left err -> throw err

    where
        updateBuf = setA (recv_buf node)

nodeRecvOneMsgTimeout :: MainLoopEnv -> Node -> IO MsgHead -> IO MsgHead
nodeRecvOneMsgTimeout env node timeout_proc = do
    nodeRecvOneMsg env node ((flip tRecvOneMsg) (timeout_s env)) timeout_proc

nodeRecvOneMsgNonBlocking :: MainLoopEnv -> Node -> IO MsgHead
nodeRecvOneMsgNonBlocking env node = do
    nodeRecvOneMsg env node tRecvOneMsgNonBlocking (return LackData)

-- throw an exception when timeout
nodeExpectOneMsg :: MainLoopEnv -> Node -> IO MsgHead
nodeExpectOneMsg env node =
    nodeRecvOneMsgTimeout env node (throw $ TCKRError "recv timeout")
