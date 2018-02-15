module Tucker.Msg.Version where

import Data.Int
import Data.Word
import qualified Data.ByteString as BSR

import Debug.Trace
import System.Random

import Tucker.Enc
import Tucker.Conf
import Tucker.Util

import Tucker.Msg.Common

data VersionPayload =
    VersionPayload {
        vers         :: Int32, -- 60002 for modern protocol
        vers_serv    :: NodeServiceType,
        timestamp    :: Int64,

        addr_recv    :: NetAddr,
        addr_from    :: NetAddr,

        nonce        :: Word64,

        user_agent   :: VStr,
        start_height :: Int32,
        relay        :: Bool
    } deriving (Show, Eq)

instance MsgPayload VersionPayload

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
        mconcat [
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

buildVersionPayload :: TCKRConf -> NetAddr -> IO VersionPayload
buildVersionPayload conf addr = do
    timestamp <- unixTimestamp
    nonce <- getStdRandom (randomR (0, maxBound :: Word64))

    -- ip4ToNetAddr "127.0.0.1" (tckr_listen_port conf) btc_cli_service

    return $ VersionPayload {
        vers = fi $ tckr_net_version conf,
        vers_serv = tckr_node_service conf,
        timestamp = timestamp,

        addr_recv = addr,
        addr_from = addr,

        nonce = nonce,

        user_agent = VStr $ tckr_user_agent conf,
        start_height = 0,
        relay = False
    }

encodeVersionPayload :: TCKRConf -> NetAddr -> IO ByteString
encodeVersionPayload conf addr =
    buildVersionPayload conf addr >>= (pure . encodeLE)

encodeVerackPayload :: IO ByteString
encodeVerackPayload = return $ BSR.empty
