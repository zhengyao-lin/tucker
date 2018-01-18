{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.Msg.Block where

import Data.Int
import Data.Word
import qualified Data.ByteString as BSR

import Tucker.Enc
import Tucker.Msg.Tx
import Tucker.Msg.Inv
import Tucker.Msg.Common

data BlockHeader =
    BlockHeader {
        vers        :: Int32,
        prev_block  :: Hash256,
        merkle_root :: Hash256,

        timestamp   :: Word32,
        diff_bits   :: Word32,
        nonce       :: Word32,

        txn_count   :: VInt
    } deriving (Show, Eq)

data BlockPayload =
    BlockPayload {
        header      :: BlockHeader,
        txns        :: [TxPayload]
    } deriving (Show, Eq)

data HeadersPayload = HeadersPayload [BlockHeader] deriving (Show, Eq)

instance MsgPayload BlockPayload
instance MsgPayload HeadersPayload

instance Encodable HeadersPayload where
    encode end (HeadersPayload headers) =
        BSR.append
            (encode end (VInt $ fromIntegral $ length headers))
            (encode end headers)

instance Decodable HeadersPayload where
    decoder = do
        (VInt count) <- decoder
        headers <- listD (fromIntegral count) decoder
        return $ HeadersPayload headers

instance Encodable BlockHeader where
    encode end (BlockHeader {
        vers = vers,
        prev_block = prev_block,
        merkle_root = merkle_root,

        timestamp = timestamp,
        diff_bits = diff_bits,
        nonce = nonce,

        txn_count = txn_count
    }) =
        BSR.concat [
            e vers,
            e prev_block,
            e merkle_root,

            e timestamp,
            e diff_bits,
            e nonce,

            e txn_count
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable BlockHeader where
    decoder = do
        vers <- decoder
        prev_block <- decoder
        merkle_root <- decoder

        timestamp <- decoder
        diff_bits <- decoder
        nonce <- decoder

        txn_count <- decoder

        return $ BlockHeader {
            vers = vers,
            prev_block = prev_block,
            merkle_root = merkle_root,
    
            timestamp = timestamp,
            diff_bits = diff_bits,
            nonce = nonce,
    
            txn_count = txn_count
        }

instance Encodable BlockPayload where
    encode end (BlockPayload {
        header = header,
        txns = txns
    }) =
        BSR.append (e header) (e txns)
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable BlockPayload where
    decoder = do
        header@(BlockHeader {
            txn_count = (VInt count)
        }) <- decoder

        txns <- listD (fromIntegral count) decoder

        return $ BlockPayload {
            header = header,
            txns = txns
        }

encodeHeadersPayload :: [BlockHeader] -> IO ByteString -- HeadersPayload
encodeHeadersPayload = return . encodeLE . HeadersPayload

-- encodeBlockPayload