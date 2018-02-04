{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.Msg.Block where

import Data.Int
import Data.Word
import qualified Data.ByteString as BSR

import Tucker.Enc
import Tucker.Auth
import Tucker.Msg.Tx
import Tucker.Msg.Inv
import Tucker.Msg.Common

data Block =
    Block {
        block_hash  :: Hash256,

        vers        :: Int32,
        
        prev_hash   :: Hash256,
        merkle_root :: Hash256,

        timestamp   :: Word32,
        diff_bits   :: Word32,
        nonce       :: Word32,

        txns        :: [TxPayload]
    }

instance Eq Block where
    (Block { block_hash = h1 }) == (Block { block_hash = h2 })
        = h1 == h2

instance Ord Block where
    (Block { block_hash = h1 }) `compare` (Block { block_hash = h2 })
        = h1 `compare` h2

instance Show Block where
    show (Block { block_hash = hash }) = "Block " ++ show hash

data BlockPayload = BlockPayload Block deriving (Eq, Show)
data BlockHeader = BlockHeader Block deriving (Eq, Show)

data HeadersPayload = HeadersPayload [BlockHeader] deriving (Show, Eq)

instance MsgPayload Block
instance MsgPayload HeadersPayload

instance Encodable HeadersPayload where
    encode end (HeadersPayload headers) =
        encodeVList end headers

instance Decodable HeadersPayload where
    decoder = vlistD decoder >>= (return . HeadersPayload)

instance Encodable BlockHeader where
    encode end (BlockHeader (Block {
        vers = vers,
        prev_hash = prev_hash,
        merkle_root = merkle_root,

        timestamp = timestamp,
        diff_bits = diff_bits,
        nonce = nonce,

        txns = txns
    })) =
        mconcat [
            e vers,
            e prev_hash,
            e merkle_root,

            e timestamp,
            e diff_bits,
            e nonce,

            e $ VInt $ fromIntegral $ length txns
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable BlockHeader where
    decoder = do
        vers <- decoder
        prev_hash <- decoder
        merkle_root <- decoder

        timestamp <- decoder
        diff_bits <- decoder
        nonce <- decoder

        VInt txn_count <- decoder

        let tmp = Block {
            block_hash = nullHash256,

            vers = vers,
            prev_hash = prev_hash,
            merkle_root = merkle_root,

            timestamp = timestamp,
            diff_bits = diff_bits,
            nonce = nonce,

            -- fill with undefined
            txns = [ undefined | _ <- [ 1 .. txn_count ] ]
        }

        return $ BlockHeader $ tmp { block_hash = hashBlock tmp }

instance Encodable Block where
    encode end block@(Block {
        txns = txns
    }) =
        encode end (BlockHeader block) <>
        encode end txns

instance Decodable Block where
    decoder = do
        (BlockHeader block@(Block {
            txns = txns
        })) <- decoder

        -- read real txns
        txns <- listD (length txns) decoder

        return $ block { txns = txns }

instance Encodable BlockPayload where
    encode end (BlockPayload block) = encode end block

instance Decodable BlockPayload where
    decoder = BlockPayload <$> decoder

-- encodeHeadersPayload :: [BlockHeader] -> IO ByteString -- HeadersPayload
-- encodeHeadersPayload = return . encodeLE . HeadersPayload

-- encodeBlock

hashBlock :: Block -> Hash256
hashBlock (Block {
    vers = vers,
    prev_hash = prev_hash,
    merkle_root = merkle_root,
    timestamp = timestamp,
    diff_bits = bits,
    nonce = nonce
}) =
    -- sha256^2(vers + prev_hash + merkle_root + time + diff + nonce)
    Hash256FromBS . ba2bs . sha256 . sha256 $ mconcat [
        encodeLE vers,
        hash256ToBS prev_hash,
        hash256ToBS merkle_root,

        encodeLE timestamp,
        encodeLE bits,
        encodeLE nonce
    ]
