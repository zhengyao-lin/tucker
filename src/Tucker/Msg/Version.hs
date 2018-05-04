module Tucker.Msg.Version where

import Data.Int
import Data.Word
import qualified Data.ByteString as BSR

import System.Random

import Control.Applicative

import Tucker.Enc
import Tucker.Conf
import Tucker.Util

import Tucker.Msg.Common

data VersionPayload =
    VersionPayload {
        cli_vers     :: Int32, -- 60002 for modern protocol
        vers_serv    :: NodeServiceType,
        timestamp    :: Int64,

        addr_recv    :: NetAddr,
        addr_from    :: NetAddr,

        vers_nonce   :: Word64,

        user_agent   :: VStr,
        start_height :: Int32,
        relay        :: Bool
    } deriving (Show, Eq)

instance MsgPayload VersionPayload

instance Encodable VersionPayload where
    encodeB end (VersionPayload {
        cli_vers = cli_vers,
        vers_serv = vers_serv,
        timestamp = timestamp,

        addr_recv = addr_recv,
        addr_from = addr_from,

        vers_nonce = vers_nonce,
        user_agent = user_agent,
        start_height = start_height,
        relay = relay
    }) =
        mconcat [
            e cli_vers,
            e vers_serv,
            e timestamp,
            
            encodeB end $ BSR.drop 4 $ encode end addr_recv, -- drop 4 bytes from the head(timestamp field)
            encodeB end $ BSR.drop 4 $ encode end addr_from,

            e vers_nonce,
            e user_agent,
            e start_height,
            e relay
        ]
        where
            e :: Encodable t => t -> Builder
            e = encodeB end

instance Decodable VersionPayload where
    decoder = do
        cli_vers <- decoder
        vers_serv <- decoder
        timestamp <- decoder

        -- net_addr in version payload does not have timestamp field
        -- so fill in a temporary timestamp field
        let decoderAddr =
                (appendD $ encodeLE (fi timestamp :: Word32)) >> decoder

        addr_recv <- decoderAddr

        addr_from <- ifD (cli_vers < 106) addr_recv decoderAddr

        vers_nonce <- ifD (cli_vers < 106) 0 decoder
        user_agent <- ifD (cli_vers < 106) (vstr "") decoder

        start_height <- ifD (cli_vers < 106) 0 decoder

        relay <- ifD (cli_vers < 70001) True (decoder <|> return True)

        return $ VersionPayload {
            cli_vers = cli_vers,
            vers_serv = vers_serv,
            timestamp = timestamp,
            addr_recv = addr_recv,
            addr_from = addr_from,
    
            vers_nonce = vers_nonce,
            user_agent = user_agent,
            start_height = start_height,
            relay = relay
        }

buildVersionPayload :: TCKRConf -> Height -> NetAddr -> IO VersionPayload
buildVersionPayload conf height addr = do
    timestamp <- unixTimestamp
    vers_nonce <- getStdRandom (randomR (0, maxBound :: Word64))

    return $ VersionPayload {
        cli_vers = fi $ tckr_net_version conf,
        vers_serv = tckr_node_service conf,
        timestamp = timestamp,

        addr_recv = addr,
        addr_from = addr,

        vers_nonce = vers_nonce,

        user_agent = vstr (tckr_user_agent conf),
        start_height = fi height,
        relay = False
    }

encodeVersionPayload :: TCKRConf -> Height -> NetAddr -> IO ByteString
encodeVersionPayload conf height addr =
    encodeLE <$> buildVersionPayload conf height addr

encodeVerackPayload :: IO ByteString
encodeVerackPayload = return $ BSR.empty
