{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.Msg.Inv where

import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.Std
import Tucker.Enc
import Tucker.Msg.Common

data InvType
    = INV_TYPE_ERROR
    | INV_TYPE_TX
    | INV_TYPE_BLOCK
    | INV_TYPE_FILTERED_BLOCK
    | INV_TYPE_CMPCT_BLOCK deriving (Show, Eq)

inv_type_map = [
        (INV_TYPE_ERROR,            0),
        (INV_TYPE_TX,               1),
        (INV_TYPE_BLOCK,            2),
        (INV_TYPE_FILTERED_BLOCK,   3),
        (INV_TYPE_CMPCT_BLOCK,      4)
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

instance Encodable InvVector where
    encode end (InvVector htype hash) =
        BSR.append (encode end htype) (encode end hash)

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
    encode end (InvPayload inv_vect) =
        BSR.append
            (encode end (VInt (fromIntegral $ length inv_vect)))
            (encode end inv_vect)

instance Decodable InvPayload where
    decoder = do
        (VInt count) <- decoder
        inv_vect <- listD (fromInteger count) decoder
        return $ InvPayload inv_vect

encodeInvPayload :: [InvVector] -> IO ByteString
encodeInvPayload = return . encodeLE . InvPayload

encodeGetdataPayload = encodeInvPayload
encodeNotfoundPayload = encodeInvPayload

data GetblocksPayload =
    GetblocksPayload {
        vers      :: Word32,
        locator   :: [Hash256],
        stop_hash :: Hash256
    }

instance MsgPayload GetblocksPayload

instance Encodable GetblocksPayload where
    encode end (GetblocksPayload vers locator stop_hash) =
        BSR.concat [
            encode end vers,
            encode end (VInt $ fromIntegral $ length locator),
            encode end locator,
            encode end stop_hash
        ]

instance Decodable GetblocksPayload where
    decoder = do
        vers <- decoder
        (VInt count) <- decoder
        locator <- listD (fromIntegral count) decoder
        stop_hash <- decoder

        return $ GetblocksPayload {
            vers = vers,
            locator = locator,
            stop_hash = stop_hash
        }

encodeGetblocksPayload :: [Hash256] -> Hash256 -> IO ByteString
encodeGetblocksPayload locator stop_hash =
    return $ encodeLE $ GetblocksPayload {
        vers = btc_version,
        locator = locator,
        stop_hash = stop_hash
    }

encodeGetheadersPayload = encodeGetblocksPayload

encodeMempoolPayload :: IO ByteString
encodeMempoolPayload = return $ BSR.pack []
