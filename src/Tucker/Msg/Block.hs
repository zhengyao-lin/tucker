{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, DeriveAnyClass #-}

module Tucker.Msg.Block where

import Data.Int
import Data.Word
import qualified Data.Foldable as FD
import qualified Data.ByteString as BSR

import Control.DeepSeq

import GHC.Generics (Generic)

import Debug.Trace

import Tucker.Enc
import Tucker.Auth
import Tucker.Conf
import Tucker.Util
import Tucker.Msg.Tx
import Tucker.Msg.Inv
import Tucker.Msg.Common
import Tucker.Msg.Hash256

type Difficulty = Double

data Block =
    Block {
        block_hash  :: Hash256,

        vers        :: Int32,
        
        prev_hash   :: Hash256,
        merkle_root :: Hash256,

        timestamp   :: Word32,
        -- diff_bits   :: Word32,
        hash_target :: Hash256,

        nonce       :: Word32,

        txns        :: PartialList TxPayload,

        enc_cache   :: Maybe ByteString
    } deriving (Generic, NFData)

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

-- instance MsgPayload Block
-- instance MsgPayload BlockHeader
instance MsgPayload BlockPayload
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
        hash_target = hash_target,
        nonce = nonce,

        txns = txns
    })) =
        mconcat [
            e vers,
            e prev_hash,
            e merkle_root,

            e timestamp,
            e $ packHash256 hash_target,
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
        packed_target <- decoder
        nonce <- decoder

        VInt txn_count <- decoder

        let tmp = Block {
            block_hash = nullHash256,

            vers = vers,
            prev_hash = prev_hash,
            merkle_root = merkle_root,

            timestamp = timestamp,
            hash_target = unpackHash256 packed_target,
            nonce = nonce,

            -- fill with undefined
            txns = PartialList (fi txn_count),

            enc_cache = Nothing
        }

        return $ BlockHeader $ tmp { block_hash = hashBlock tmp }

instance Encodable Block where
    encode end block@(Block {
        enc_cache = Just cache
    }) = cache -- use cache if possible

    encode end block@(Block {
        txns = txns
    }) =
        encode end (BlockHeader block) <>
        encode end txns

instance Decodable Block where
    decoder = do
        buf <- allD'
        init_len <- lenD

        (BlockHeader block@(Block {
            txns = txns
        })) <- decoder

        -- traceM "decoding block"

        -- read real txns
        txns <- listD (length txns) decoder

        -- traceM "decoding block finished"

        final_len <- lenD

        return $ block {
            txns = FullList txns,
            enc_cache = Just $ BSR.take (init_len - final_len) buf
        }

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
    hash_target = hash_target,
    nonce = nonce
}) =
    -- sha256^2(vers + prev_hash + merkle_root + time + diff + nonce)
    bsToHash256 . ba2bs . sha256 . sha256 $ mconcat [
        encodeLE vers,
        encodeLE prev_hash,
        encodeLE merkle_root,

        encodeLE timestamp,

        encodeLE $ packHash256 hash_target,

        encodeLE nonce
    ]

-- target to approximate difficulty(truncated by bitcoin floating point)
targetBDiff :: Hash256 -> Difficulty
targetBDiff hash =
    fromIntegral tucker_bdiff_diff1 / fromIntegral hash

isHashOf :: Hash256 -> Block -> Bool
isHashOf hash = (== hash) . block_hash

blockHeader :: Block -> Block
blockHeader block = block {
        txns = toPartial (txns block)
    }

isFullBlock :: Block -> Bool
isFullBlock block = isFull (txns block)
