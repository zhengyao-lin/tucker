-- utilities

module Tucker.P2P.Util where

import Data.Word
import qualified Data.ByteString as BSR

import Network.Socket

import Tucker.Enc
import Tucker.Msg
import Tucker.Std
import Tucker.Atom
import Tucker.Conf
import Tucker.P2P.Node

ip4ToIP6 :: ByteString -> ByteString
ip4ToIP6 addrv4 =
    BSR.append (BSR.pack pref) addrv4
    where
        pref = [ 0x00, 0x00, 0x00, 0x00,
                 0x00, 0x00, 0x00, 0x00,
                 0x00, 0x00, 0xff, 0xff ]

ip4ToNetAddr :: String -> Word16 -> BTCServiceType -> IO NetAddr
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

isSameIP :: SockAddr -> SockAddr -> Bool
isSameIP (SockAddrInet _ h1) (SockAddrInet _ h2) = h1 == h2
isSameIP (SockAddrInet6 _ _ h1 _) (SockAddrInet6 _ _ h2 _) = h1 == h2
isSameIP _ _ = False

filterProbingList :: MainLoopEnv -> [AddrInfo] -> IO [AddrInfo]
filterProbingList env new_list = do
    nodes <- getA $ node_list env

    bl <- getEnvConf env tckr_node_blacklist

    let exist_sockaddr = map (addrAddress . addr) nodes
        blacklist = exist_sockaddr ++ bl

    return $ filter (\a -> all (not . isSameIP (addrAddress a)) blacklist) new_list

decodePayload :: MsgPayload t
              => MainLoopEnv
              -> BTCNode
              -> ByteString
              -> (t -> IO [RouterAction])
              -> IO [RouterAction]

decodePayload env node payload proc = do
    case decodeAllLE payload of
        Left err -> do
            nodeMsg env node $ "uncrucial decoding error: " ++ (show err)
            return []

        Right v -> proc v
