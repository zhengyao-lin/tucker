{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.Msg.Inv where

import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Msg.Common
import Tucker.Msg.Hash256

data InvType
    = INV_TYPE_ERROR
    | INV_TYPE_TX
    | INV_TYPE_BLOCK
    | INV_TYPE_FILTERED_BLOCK
    | INV_TYPE_CMPCT_BLOCK
    | INV_TYPE_WITNESS_TX
    | INV_TYPE_WITNESS_BLOCK deriving (Show, Eq)

inv_type_map = [
        (INV_TYPE_ERROR,                0x0),
        (INV_TYPE_TX,                   0x1),
        (INV_TYPE_BLOCK,                0x2),
        (INV_TYPE_FILTERED_BLOCK,       0x3),
        (INV_TYPE_CMPCT_BLOCK,          0x4),
        (INV_TYPE_WITNESS_TX,    0x40000001),
        (INV_TYPE_WITNESS_BLOCK, 0x40000002)
    ]

inv_type_map_r = map (\(a, b) -> (b, a)) inv_type_map

instance Encodable InvType where
    encode end t =
        case lookup INV_TYPE_CMPCT_BLOCK inv_type_map of
            Just i -> encode end (i :: Word32)
            Nothing -> error "impossible"

instance Decodable InvType where
    decoder = do
        itype <- decoder :: Decoder Word32

        case lookup itype inv_type_map_r of
            Just htype -> return htype
            Nothing -> fail "illegal inventory type"

-- type and hash
data InvVector = InvVector InvType Hash256 deriving (Show, Eq)

invToHash256 (InvVector _ hash) = hash

instance Encodable InvVector where
    encode end (InvVector htype hash) =
        encode end htype <> encode end hash

instance Decodable InvVector where
    decoder = do
        htype <- decoder
        hash <- decoder
        return $ InvVector htype hash

data InvPayload =
    InvPayload {
        inv_vect :: [InvVector]
    }

instance MsgPayload InvPayload

instance Encodable InvPayload where
    encode end (InvPayload inv_vect) = encodeVList end inv_vect

instance Decodable InvPayload where
    decoder = vlistD decoder >>= (return . InvPayload)

encodeInvPayload :: [InvVector] -> IO ByteString
encodeInvPayload = return . encodeLE . InvPayload

encodeGetdataPayload = encodeInvPayload
encodeNotfoundPayload = encodeInvPayload

data GetblocksPayload =
    GetblocksPayload {
        gb_vers   :: Word32,
        locator   :: [Hash256],
        stop_hash :: Hash256
    }

instance MsgPayload GetblocksPayload

instance Encodable GetblocksPayload where
    encode end (GetblocksPayload gb_vers locator stop_hash) =
        mconcat [
            encode end gb_vers,
            encodeVList end locator,
            encode end stop_hash
        ]

instance Decodable GetblocksPayload where
    decoder = do
        gb_vers <- decoder
        locator <- vlistD decoder
        stop_hash <- decoder

        return $ GetblocksPayload {
            gb_vers = gb_vers,
            locator = locator,
            stop_hash = stop_hash
        }

encodeGetblocksPayload :: TCKRConf -> [Hash256] -> Hash256 -> IO ByteString
encodeGetblocksPayload conf locator stop_hash =
    return $ encodeLE $ GetblocksPayload {
        gb_vers = fi $ tckr_net_version conf,
        locator = locator,
        stop_hash = stop_hash
    }

encodeGetheadersPayload = encodeGetblocksPayload

encodeMempoolPayload :: IO ByteString
encodeMempoolPayload = return $ BSR.empty
