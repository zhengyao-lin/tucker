-- trivial BTC protocol messages

module Tucker.Msg.Other where

import Data.Int
import Data.Word
import Data.Bits
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BSR

import Network.Socket

import Tucker.Enc

unixTimestamp :: Integral a => IO a
unixTimestamp = round `fmap` getPOSIXTime

data ServiceTypeSingle = NODE_NETWORK | NODE_GETUTXO | NODE_BLOOM deriving (Show, Eq)
data ServiceType = ServiceType [ServiceTypeSingle] deriving (Show, Eq)

instance Encodable ServiceType where
    encode end (ServiceType serv) =
        encode end $
        ((foldr (.|.) 0) $
        map (\s -> case s of
            NODE_NETWORK -> 0x01
            NODE_GETUTXO -> 0x02
            NODE_BLOOM -> 0x04) serv :: Word64)

data NetAddr =
    NetAddr {
        time         :: Word32,
        services     :: ServiceType,
        ipv6o4       :: ByteString, 
        port         :: Word16
    } deriving (Show, Eq)

instance Encodable NetAddr where
    encode end (NetAddr {
        time = time,
        services = services,
        ipv6o4 = ipv6o4,
        port = port
    }) =
        BSR.concat [
            e time,
            e services,
            e ipv6o4,
            e port
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

ip42ip6 :: ByteString -> ByteString
ip42ip6 addrv4 =
    BSR.append (BSR.pack pref) addrv4
    where pref = [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff ]

ip42netaddr :: String -> Word16 -> ServiceType -> IO NetAddr
ip42netaddr addr port serv = do
    time <- unixTimestamp
    enc <- inet_addr addr
    let addrv6 = ip42ip6 $ encodeLE enc
    pure $ NetAddr {
        time = time,
        services = serv,
        ipv6o4 = addrv6,
        port = port
    }

-- data VersionPayload =
--     VersionPayload {
--         vers         :: Int32, -- 60002 for modern protocol
--         services     :: ServiceType,
--         timestamp    :: Int64,

--         addr_recv    :: NetAddr,
--         addr_from    :: NetAddr,

--         nonce        :: Word64,

--         user_agent   :: VStr,

--         start_height :: Int32,

--         relay        :: Bool
--     }

-- buildVersionPayload :: IO ByteString
-- buildVersionPayload = do
--     timestamp <- unixTimestamp

--     pure $ VersionPayload {
--         vers = 60002,
--         services = [ NODE_NETWORK ],
--         timestamp = timestamp,

--     }
