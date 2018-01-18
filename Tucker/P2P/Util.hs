-- utilities

module Tucker.P2P.Util where

import Data.Word
import qualified Data.ByteString as BSR

import Network.Socket

import Tucker.Enc
import Tucker.Msg
import Tucker.Std
import Tucker.Atom
import Tucker.P2P.Node

ip4ToIp6 :: ByteString -> ByteString
ip4ToIp6 addrv4 =
    BSR.append (BSR.pack pref) addrv4
    where
        pref = [ 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0xff, 0xff ]

ip4ToNetAddr :: String -> Word16 -> BTCServiceType -> IO NetAddr
ip4ToNetAddr addr port serv = do
    time <- unixTimestamp
    enc <- inet_addr addr
    let addrv6 = ip4ToIp6 $ encodeBE enc
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
seedLookup :: BTCNetwork -> String -> IO [AddrInfo]
seedLookup net host =
    getAddrInfo (Just defaultHints {
        addrSocketType = Stream
    }) (Just host) (Just $ show $ listenPort net)

-- try to trim a message from the received data
-- if a complete message is found, return (Right MsgHead {}, rest of data)
-- if the message is incomplete, return (Right LackData, original data)
-- if the message if incorrect, return (Left err, original data)
-- trimMsg :: ByteString -> (Either TCKRError MsgHead, ByteString)
-- trimMsg = decodeLE

sockAddrToNetAddr :: SockAddr -> BTCServiceType -> IO NetAddr

sockAddrToNetAddr (SockAddrInet port host) serv = do
    time <- unixTimestamp
    pure $ NetAddr {
        time = time,
        net_serv = serv,
        ipv6o4 = ip4ToIp6 $ encodeBE (fromIntegral host :: Word32),
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

netAddrToSockAddr :: NetAddr -> IO SockAddr
netAddrToSockAddr netaddr = do
    return $ if isSupportedSockAddr (SockAddrInet6 0 0 (0, 0, 0, 0) 0) then
        -- support ipv6
        SockAddrInet6 (fromIntegral $ port netaddr) 0 (
            fromIntegral $ decodeInt 4 BigEndian h1,
            fromIntegral $ decodeInt 4 BigEndian h2,
            fromIntegral $ decodeInt 4 BigEndian h3,
            fromIntegral $ decodeInt 4 BigEndian h4
        ) 0
    else
        SockAddrInet (fromIntegral $ port netaddr) (fromIntegral $ decodeInt 4 BigEndian h4)

    where
        ip = ipv6o4 netaddr
        h1 = BSR.take 4 $ BSR.drop 0 ip
        h2 = BSR.take 4 $ BSR.drop 4 ip
        h3 = BSR.take 4 $ BSR.drop 8 ip
        h4 = BSR.take 4 $ BSR.drop 12 ip

nodeToNetAddr :: BTCNode -> IO (Maybe NetAddr)
nodeToNetAddr node = do
    let sockaddr = addrAddress $ addr node
    vers <- getA $ vers_payload node
    alive <- getA $ alive node

    if vers == VersionPending || not alive then
        -- error "version not ready(no handshake?)"
        return Nothing
    else do
        netaddr <- sockAddrToNetAddr sockaddr (vers_serv vers)
        return $ Just $ netaddr
