-- utilities

module Tucker.P2P.Util where

import Data.IP
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

ip4ToIP6Pref :: ByteString
ip4ToIP6Pref =
    BSR.pack [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff ]

ip4ToIP6 :: ByteString -> ByteString
ip4ToIP6 addrv4 = ip4ToIP6Pref <> addrv4

ip4ToNetAddr :: String -> Word16 -> NodeServiceType -> IO NetAddr
ip4ToNetAddr addr port serv = do
    time <- unixTimestamp
    enc <- inet_addr addr
    let addrv6 = ip4ToIP6 $ encodeBE $ ntohl enc
    return NetAddr {
        time = time,
        net_serv = serv,
        ipv6o4 = addrv6,
        port = port
    }

ipToAddrInfo :: String -> Word16 -> IO AddrInfo
ipToAddrInfo ip port =
    head <$>
    getAddrInfo (Just defaultHints {
        addrSocketType = Stream
        -- addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
    }) (Just ip) (Just (show port))

-- AF_INET SOCK_STREAM (addrSocketType addr) 

buildSocketTo :: AddrInfo -> IO Socket
buildSocketTo addr =
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

-- look up node seeds using a domain
seedLookup :: TCKRConf -> String -> IO [AddrInfo]
seedLookup conf host =
    getAddrInfo (Just defaultHints {
        addrSocketType = Stream
    }) (Just host) (Just (show (tckr_listen_port conf)))

    `catch` \e -> (e :: SomeException) `seq` return []

-- try to trim a message from the received data
-- if a complete message is found, return (Right MsgHead {}, rest of data)
-- if the message is incomplete, return (Right LackData, original data)
-- if the message if incorrect, return (Left err, original data)
-- trimMsg :: ByteString -> (Either TCKRError MsgHead, ByteString)
-- trimMsg = decodeLE

-- SockAddr is a part of AddrInfo
-- NetAddr is a component of bitcoin protocol
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
        ipv6o4 = BSR.concat [
            encodeBE (fromIntegral h1 :: Word32),
            encodeBE (fromIntegral h2 :: Word32),
            encodeBE (fromIntegral h3 :: Word32),
            encodeBE (fromIntegral h4 :: Word32)
        ],
        port = fromIntegral port
    }

netAddrToAddrInfo :: NetAddr -> IO AddrInfo
netAddrToAddrInfo netaddr = do
    let ip6 = show $ fromHostAddress6 (
                ntohl (decodeFailBE h1),
                ntohl (decodeFailBE h2),
                ntohl (decodeFailBE h3),
                ntohl (decodeFailBE h4)
            )
        
        ip4 = show $ fromHostAddress (ntohl (decodeFailBE h4))

    -- return $ tLn (show (isSupportedFamily AF_INET6)) $
    -- return $
    head <$>
        if is_ip4 || not (isSupportedFamily AF_INET6) then do
            getAddrInfo (Just defaultHints {
                addrSocketType = Stream,
                addrFamily = AF_INET
            }) (Just ip4) (Just (show (port netaddr)))
        else -- use ip6
            getAddrInfo (Just $ defaultHints {
                addrSocketType = Stream,
                addrFamily = AF_INET6
            }) (Just ip6) (Just (show (port netaddr)))

    where
        ip = ipv6o4 netaddr
        h1 = BSR.take 4 $ BSR.drop 0 ip
        h2 = BSR.take 4 $ BSR.drop 4 ip
        h3 = BSR.take 4 $ BSR.drop 8 ip
        h4 = BSR.take 4 $ BSR.drop 12 ip

        is_ip4 = BSR.isPrefixOf ip4ToIP6Pref ip

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

secondToMicrosecond :: (Integral a, Integral b) => a -> b
secondToMicrosecond = fromIntegral . (* 1000000)

timeoutS :: Int -> IO a -> IO (Maybe a)
timeoutS sec = timeout (secondToMicrosecond sec)

timeoutFailS :: Int -> IO a -> IO a
timeoutFailS sec action = do
    res <- timeoutS sec action

    case res of
        Nothing -> throwMT "action timeout"
        Just v -> return v

timeoutRetryS :: Int -> IO a -> IO a
timeoutRetryS sec action = untilJust $ timeoutS sec action
