{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.Msg.Block where

import Data.Int
import Data.Word
import Data.Bits
import qualified Data.Foldable as FD
import qualified Data.ByteString as BSR

import Tucker.Enc
import Tucker.Auth
import Tucker.Conf
import Tucker.Util
import Tucker.DeepSeq

import Tucker.Msg.Tx
import Tucker.Msg.Inv
import Tucker.Msg.Common
import Tucker.Msg.Hash256

type Difficulty = Double

data Block =
    Block {
        block_hash  :: Hash256,

        vers        :: Word32,
        
        prev_hash   :: Hash256,
        merkle_root :: Hash256,

        btimestamp  :: Timestamp,
        -- diff_bits   :: Word32,
        hash_target :: Hash256,

        nonce       :: Word32,

        txns        :: PartialList TxPayload,

        enc_cache   :: Maybe ByteString
    }

instance NFData Block where
    rnf (Block {
        block_hash = block_hash,
        vers = vers,
        prev_hash = prev_hash,
        merkle_root = merkle_root,
        btimestamp = btimestamp,
        hash_target = hash_target,
        nonce = nonce,
        txns = txns,
        enc_cache = enc_cache
    }) =
        rnf block_hash `seq`
        rnf vers `seq`
        rnf prev_hash `seq`
        rnf merkle_root `seq`
        rnf btimestamp `seq`
        rnf hash_target `seq`
        rnf nonce `seq`
        rnf txns `seq`
        rnf enc_cache

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

        btimestamp = btimestamp,
        hash_target = hash_target,
        nonce = nonce,

        txns = txns
    })) =
        mconcat [
            e vers,
            e prev_hash,
            e merkle_root,

            e btimestamp,
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

        btimestamp <- decoder
        packed_target <- decoder
        nonce <- decoder

        VInt txn_count <- decoder

        let tmp = Block {
            block_hash = nullHash256,

            vers = vers,
            prev_hash = prev_hash,
            merkle_root = merkle_root,

            btimestamp = btimestamp,
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

        -- read real txns
        txns <- listD (length txns) decoder

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

-- fixed part of a block in mining
blockFixedHeader :: Block -> ByteString
blockFixedHeader (Block {
    vers = vers,
    prev_hash = prev_hash,
    merkle_root = merkle_root,
    btimestamp = btimestamp,
    hash_target = hash_target
}) =
    mconcat [
        encodeLE vers,
        encodeLE prev_hash,
        encodeLE merkle_root,

        encodeLE btimestamp,

        encodeLE $ packHash256 hash_target
    ]

hashBlock :: Block -> Hash256
hashBlock block =
    -- sha256^2(vers + prev_hash + merkle_root + time + diff + nonce)
    stdHash256 $
    blockFixedHeader block <> encodeLE (nonce block)

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

-- SoftFork defined in Conf.hs

instance Encodable SoftForkStatus where
    encode end FORK_STATUS_DEFINED = bchar 0
    encode end FORK_STATUS_STARTED = bchar 1
    encode end FORK_STATUS_LOCKED_IN = bchar 2
    encode end FORK_STATUS_FAILED = bchar 3
    encode end FORK_STATUS_ACTIVE = bchar 4

instance Decodable SoftForkStatus where
    decoder = do
        b <- byteD
        case b of
            0 -> return FORK_STATUS_DEFINED
            1 -> return FORK_STATUS_STARTED
            2 -> return FORK_STATUS_LOCKED_IN
            3 -> return FORK_STATUS_FAILED
            4 -> return FORK_STATUS_ACTIVE
            _ -> fail "illegal deployment status"

instance Encodable SoftFork where
    encode end d =
        mconcat [
            encode end (VStr (fork_name d)),
            encode end (fork_bit d),
            encode end (fork_start d),
            encode end (fork_timeout d),
            encode end (fork_status d)
        ]

instance Decodable SoftFork where
    decoder = do
        VStr name <- decoder
        bit <- decoder
        start <- decoder
        timeout <- decoder
        status <- decoder

        return $ SoftFork {
            fork_name = name,
            fork_bit = bit,
            fork_start = start,
            fork_timeout = timeout,
            fork_status = status
        }

-- get deployment ids(bit position)
-- for which the corresponding bit
-- in version is set
getSoftForkIds :: Block -> [SoftForkId]
getSoftForkIds (Block { vers = vers }) =
    if vers .&. 0xE0000000 == 0x20000000 then
        filter (\n -> (vers `shift` fi n) .&. 1 == 1) [ 0 .. 28 ]
    else []

isFinalSoftFork :: SoftFork -> Bool
isFinalSoftFork d =
    status == FORK_STATUS_ACTIVE ||
    status == FORK_STATUS_FAILED
    where status = fork_status d

merkleParents :: [Hash256] -> [Hash256]
merkleParents [] = []
merkleParents [a] =
    [ stdHash256 $ hash256ToBS a <> hash256ToBS a ]

merkleParents (l:r:leaves) =
    (stdHash256 $ hash256ToBS l <> hash256ToBS r) :
    merkleParents leaves

merkleRoot' :: [Hash256] -> [Hash256]
merkleRoot' [] = [nullHash256]
merkleRoot' [single] = [single]
merkleRoot' leaves = merkleRoot' $ merkleParents leaves

merkleRoot :: Block -> Hash256
merkleRoot (Block {
    txns = txns
}) = head $ merkleRoot' (map txid (FD.toList txns))

updateBlockHashes :: Block -> Block
updateBlockHashes block =
    let block1 = block {
                merkle_root = merkleRoot block
            }

        block2 = block1 {
                block_hash = hashBlock block1
            }
    in block2

appendTx :: TxPayload -> Block -> Block
appendTx tx block =
    let new_txns = (FD.toList (txns block) ++ [ tx ]) in
    updateBlockHashes $ block {
        txns = FullList new_txns
    }
