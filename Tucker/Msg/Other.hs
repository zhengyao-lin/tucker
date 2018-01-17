module Tucker.Msg.Other where

import Data.Word
import qualified Data.ByteString as BSR

import Tucker.Enc
import Tucker.Msg.Common

data PingPongPayload = PingPongPayload Word64 deriving (Show, Eq)

instance MsgPayload PingPongPayload

instance Encodable PingPongPayload where
    encode end (PingPongPayload nonce) =
        encode end nonce

instance Decodable PingPongPayload where
    decoder = decoder >>= return . PingPongPayload

data RejectType
    = REJECT_MALFORMED
    | REJECT_INVALID
    | REJECT_OBSOLETE
    | REJECT_DUPLICATE
    | REJECT_NONSTANDARD
    | REJECT_DUST
    | REJECT_INSUFFICIENTFEE
    | REJECT_CHECKPOINT deriving (Show, Eq)
        
reject_type_map = [
        (REJECT_MALFORMED, 0x01),
        (REJECT_INVALID, 0x10),
        (REJECT_OBSOLETE, 0x11),
        (REJECT_DUPLICATE, 0x12),
        (REJECT_NONSTANDARD, 0x40),
        (REJECT_DUST, 0x41),
        (REJECT_INSUFFICIENTFEE, 0x42),
        (REJECT_CHECKPOINT, 0x43)
    ]

reject_type_map_r = map (\(a, b) -> (b, a)) reject_type_map

instance Encodable RejectType where
    encode end t =
        case lookup t reject_type_map of
            Just i -> (encode end (i :: Word8))
            Nothing -> error "reject type not exist"

instance Decodable RejectType where
    decoder = do
        i <- byteD
        case lookup i reject_type_map_r of
            Just t -> return t
            Nothing -> fail $ "reject type " ++ (show i) ++ " not exist"

data RejectPayload =
    RejectPayload {
        message :: VStr,
        ccode   :: RejectType,
        reason  :: VStr,
        rdata   :: ByteString
    } deriving (Show, Eq)

instance Encodable RejectPayload where
    encode end (RejectPayload {
        message = message,
        ccode = ccode,
        reason = reason,
        rdata = rdata
    }) =
        BSR.concat [
            e message,
            e ccode,
            e reason,
            e rdata
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable RejectPayload where
    decoder = do
        message <- decoder
        ccode <- decoder
        reason <- decoder
        rdata <- allD -- eat the rest
        return $ RejectPayload {
            message = message,
            ccode = ccode,
            reason = reason,
            rdata = rdata
        }

data AlertPayload = AlertPayload ByteString deriving (Show, Eq)

-- instance Encodable AlertPayload where

instance Decodable AlertPayload where
    decoder = allD >>= return . AlertPayload
