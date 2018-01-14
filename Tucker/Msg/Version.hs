module Tucker.Msg.Version where

import Data.Int
import Data.Word
import qualified Data.ByteString as BSR

import Debug.Trace
import System.Random

import Tucker.Enc
import Tucker.Std

import Tucker.Msg.Common

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
    } | VersionPending deriving (Show, Eq)

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

instance Decodable VersionPayload where
    decoder = do
        vers <- decoder
        vers_serv <- decoder
        timestamp <- decoder

        -- net_addr in version payload does not have timestamp field
        -- so fill in a temporary timestamp field
        let decoderAddr = (appendD $ encodeLE (fromIntegral timestamp :: Word32)) >> decoder

        addr_recv <- decoderAddr

        addr_from <- ifD (vers < 106) addr_recv decoderAddr

        nonce <- ifD (vers < 106) 0 decoder
        user_agent <- ifD (vers < 106) (VStr "") decoder

        start_height <- ifD (vers < 106) 0 decoder
        relay <- ifD (vers < 70001) False decoder

        return $ VersionPayload {
            vers = vers,
            vers_serv = vers_serv,
            timestamp = timestamp,
            addr_recv = addr_recv,
            addr_from = addr_from,
    
            nonce = nonce,
            user_agent = user_agent,
            start_height = start_height,
            relay = relay
        }

buildVersionPayload :: BTCNetwork -> IO VersionPayload
buildVersionPayload net = do
    timestamp <- unixTimestamp
    nonce <- getStdRandom (randomR (0, maxBound :: Word64))

    addr1 <- ip42netaddr "127.0.0.1" (listenPort net) btc_cli_service

    return $ VersionPayload {
        vers = btc_version,
        vers_serv = btc_cli_service,
        timestamp = timestamp,

        addr_recv = addr1,
        addr_from = addr1,

        nonce = nonce,

        user_agent = VStr btc_user_agent,
        start_height = 0,
        relay = False
    }

encodeVersionPayload :: BTCNetwork -> IO ByteString
encodeVersionPayload net =
    buildVersionPayload net >>= (pure . encodeLE)

encodeVerackPayload :: IO ByteString
encodeVerackPayload = return $ BSR.pack []
