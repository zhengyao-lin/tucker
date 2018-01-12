-- trivial BTC protocol messages

module Tucker.Msg.Other where

import Data.Int
import Data.Word
import Data.Bits
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BSR

import Network.Socket
import System.Random

import Tucker.Enc
import Tucker.Std

import Tucker.Msg.Common

unixTimestamp :: Integral a => IO a
unixTimestamp = round `fmap` getPOSIXTime

-- defined in Tucker.Std
instance Encodable BTCServiceType where
    encode end (BTCServiceType serv) =
        encode end $
        ((foldr (.|.) 0) $
        map (\s -> case s of
            BTC_NODE_NETWORK -> 0x01
            BTC_NODE_GETUTXO -> 0x02
            BTC_NODE_BLOOM -> 0x04) serv :: Word64)

data NetAddr =
    NetAddr {
        time         :: Word32,
        neta_serv    :: BTCServiceType,
        ipv6o4       :: ByteString, 
        port         :: Word16
    } deriving (Show, Eq)

instance Encodable NetAddr where
    encode end (NetAddr {
        time = time,
        neta_serv = neta_serv,
        ipv6o4 = ipv6o4,
        port = port
    }) =
        BSR.concat [
            e time,
            e neta_serv,
            e ipv6o4,
            encodeBE port -- port here is big-endian
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

ip42ip6 :: ByteString -> ByteString
ip42ip6 addrv4 =
    BSR.append (BSR.pack pref) addrv4
    where pref = [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff ]

ip42netaddr :: String -> Word16 -> BTCServiceType -> IO NetAddr
ip42netaddr addr port serv = do
    time <- unixTimestamp
    enc <- inet_addr addr
    let addrv6 = ip42ip6 $ encodeBE enc
    pure $ NetAddr {
        time = time,
        neta_serv = serv,
        ipv6o4 = addrv6,
        port = port
    }

data VersionPayload =
    VersionPayload {
        vers         :: Int32, -- 60002 for modern protocol
        vers_serv    :: BTCServiceType,
        timestamp    :: Int64,

        addr_recv    :: NetAddr,
        addr_from    :: NetAddr,

        nonce        :: Word64,

        user_agent   :: VStr,
        start_height :: Int32,
        relay        :: Bool
    } deriving (Show, Eq)

instance Encodable VersionPayload where
    encode end (VersionPayload {
        vers = vers,
        vers_serv = vers_serv,
        timestamp = timestamp,

        addr_recv = addr_recv,
        addr_from = addr_from,

        nonce = nonce,
        user_agent = user_agent,
        start_height = start_height,
        relay = relay
    }) =
        BSR.concat [
            e vers,
            e vers_serv,
            e timestamp,
            
            BSR.drop 4 $ e addr_recv, -- drop 4 bytes from the head(timestamp field)
            BSR.drop 4 $ e addr_from,

            e nonce,
            e user_agent,
            e start_height,
            e relay
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

buildVersionPayload :: BTCNetwork -> IO VersionPayload
buildVersionPayload net = do
    timestamp <- unixTimestamp
    nonce <- getStdRandom (randomR (0, maxBound :: Word64))

    addr1 <- ip42netaddr "127.0.0.1" (listenPort net) btc_cli_service

    return $ VersionPayload {
        vers = 60002,
        vers_serv = btc_cli_service,
        timestamp = timestamp,

        addr_recv = addr1,
        addr_from = addr1,

        nonce = nonce,

        user_agent = VStr "",
        start_height = 0,
        relay = False
    }

encodeVersionPayload :: BTCNetwork -> IO ByteString
encodeVersionPayload net =
    buildVersionPayload net >>= (pure . encodeLE)
