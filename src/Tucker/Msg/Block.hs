{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Tucker.Msg.Block where

import Data.Int
import Data.Word
import Data.Bits
import qualified Data.Foldable as FD
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Builder as BSB

import Control.Monad

import Tucker.Enc
import Tucker.Auth
import Tucker.Conf
import Tucker.Util
import Tucker.DeepSeq

import Tucker.Msg.Tx
import Tucker.Msg.Inv
import Tucker.Msg.Common
import Tucker.Msg.Hash256
import Tucker.Msg.ScriptOp

type Difficulty = Double
-- height starts from 0(genesis)

type BlockVersion = Int32
-- use signed version because we should reject blocks with negative versions(after BIP34)

data Block =
    Block {
        block_hash  :: Hash256,

        vers        :: BlockVersion,
        
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

newtype BlockPayload = BlockPayload Block deriving (Eq, Show)
newtype BlockHeader = BlockHeader Block deriving (Eq, Show)

newtype HeadersPayload = HeadersPayload [BlockHeader] deriving (Show, Eq)

-- instance MsgPayload Block
-- instance MsgPayload BlockHeader
instance MsgPayload BlockPayload
instance MsgPayload HeadersPayload

instance Encodable HeadersPayload where
    encodeB end (HeadersPayload headers) =
        encodeVListB end (map (\(BlockHeader b) -> networkBlockHeader b) headers)

instance Decodable HeadersPayload where
    decoder = HeadersPayload <$> vlistD decoder

instance Encodable BlockHeader where
    encodeB end (BlockHeader (Block {
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
            e :: Encodable t => t -> Builder
            e = encodeB end

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

instance Sizeable BlockHeader where
    sizeOf (BlockHeader (Block {
        vers = vers,
        prev_hash = prev_hash,
        merkle_root = merkle_root,

        btimestamp = btimestamp,
        hash_target = hash_target,
        nonce = nonce,

        txns = txns
    })) =
        sizeOf vers +
        sizeOf prev_hash +
        sizeOf merkle_root +
        sizeOf btimestamp +
        sizeOf (packHash256 hash_target) +
        sizeOf nonce +
        sizeOf (VInt (fi (length txns)))

instance Encodable Block where
    encodeB end block@(Block {
        enc_cache = Just cache
    }) = BSB.byteString cache -- use cache if possible

    encodeB end block@(Block {
        txns = txns
    }) =
        encodeB end (BlockHeader block) <>
        encodeB end txns

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

instance Sizeable Block where
    sizeOf (Block {
        enc_cache = Just cache
    }) = BSR.length cache

    sizeOf block@(Block {
        txns = txns
    }) =
        sizeOf (BlockHeader block) +
        sizeOf txns

instance Encodable BlockPayload where
    encodeB end (BlockPayload block) = encodeB end block

instance Decodable BlockPayload where
    decoder = BlockPayload <$> decoder

encodeHeadersPayload :: [BlockHeader] -> IO ByteString -- HeadersPayload
encodeHeadersPayload = return . encodeLE . HeadersPayload

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
targetBDiff :: TCKRConf -> Hash256 -> Difficulty
targetBDiff conf hash =
    fi (tckr_bdiff_diff1_target conf) / fi hash

isHashOf :: Hash256 -> Block -> Bool
isHashOf hash = (== hash) . block_hash

-- NOTE that the block header here is not
-- extactly the block header structure defined in
-- the bitcoin protocol, in which block headers always
-- have their txn_count field to be 0
blockHeader :: Block -> Block
blockHeader block =
    block {
        txns = toPartial (txns block)
    }

removeTxns :: Block -> Block
removeTxns block =
    block {
        txns = mempty
    }

-- block with all txns removed
networkBlockHeader :: Block -> Block
networkBlockHeader = removeTxns

isFullBlock :: Block -> Bool
isFullBlock block = isFull (txns block)

-- SoftFork defined in Conf.hs

instance Encodable SoftForkStatus where
    encodeB end FORK_STATUS_DEFINED = bcharB 0
    encodeB end FORK_STATUS_STARTED = bcharB 1
    encodeB end FORK_STATUS_LOCKED_IN = bcharB 2
    encodeB end FORK_STATUS_FAILED = bcharB 3
    encodeB end FORK_STATUS_ACTIVE = bcharB 4

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
    encodeB end d =
        mconcat [
            encodeB end (vstr (fork_name d)),
            encodeB end (fork_bit d),
            encodeB end (fork_start d),
            encodeB end (fork_timeout d),
            encodeB end (fork_status d)
        ]

instance Decodable SoftFork where
    decoder = do
        name <- decoder
        bit <- decoder
        start <- decoder
        timeout <- decoder
        status <- decoder

        return $ SoftFork {
            fork_name = vstrToString name,
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
        -- top 3 bits are 001
        filter (\n -> (vers `shiftR` fi n) .&. 1 == 1) [ 0 .. 28 ]
    else []

genDeployVersion :: [SoftForkId] -> BlockVersion
genDeployVersion forks =
    foldl (\v id -> v .|. (1 `shift` fi id)) 0x20000000 forks

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
}) = head (merkleRoot' (map txid (FD.toList txns)))

witnessMerkleRoot :: Block -> Hash256
witnessMerkleRoot (Block {
    txns = txns'
}) = head (merkleRoot' wtxids)
    where txns = FD.toList txns'
          wtxids =
            if null txns then []
            else
                nullHash256 : map wtxid (tail txns)

clearEncCache :: Block -> Block
clearEncCache block =
    block {
        enc_cache = Nothing
    }

updateBlock :: Block -> Block
updateBlock block =
    let block1 = block {
                merkle_root = merkleRoot block
            }

        block2 = block1 {
                block_hash = hashBlock block1
            }
    in clearEncCache block2

appendTx :: TxPayload -> Block -> Block
appendTx tx block =
    let new_txns = (FD.toList (txns block) ++ [ tx ]) in
    updateBlock block {
        txns = FullList new_txns
    }

updateCoinbase :: TxPayload -> Block -> Block
updateCoinbase coinbase block =
    let all_txns = FD.toList (txns block) in
    if null all_txns then appendTx coinbase block
    else
        updateBlock block {
            txns = FullList (coinbase : tail all_txns)
        }

stripBlockWitness :: Block -> Block
stripBlockWitness block@(Block {
    txns = txns
}) =
    updateBlock block {
        txns = FullList (map stripWitness (FD.toList txns))
    }

-- specification see https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki
-- assuming full block access
blockWeight :: Block -> Int
blockWeight block =
    3 * sizeOf (clearEncCache (stripBlockWitness block))
    + sizeOf block

encodeBlockPayload :: Block -> IO ByteString
encodeBlockPayload = return . encodeLE

-- (commitment hash, reserved value)
getWitnessCommitment :: TCKRConf -> Block -> Maybe (Hash256, ByteString)
getWitnessCommitment conf block =
    if null all_txns || null outputs then Nothing
    else do
        wit_resv <- (\(TxWitness (w:_)) -> w) <$> getWitness coinbase 0
        com_hash <- firstMaybe (map extract (reverse outputs))

        return (com_hash, wit_resv)

    where
        all_txns = FD.toList (txns block)
        coinbase = head all_txns
        outputs = tx_out coinbase
        header = tckr_wit_commit_header conf

        extract out =
            case decodeAllLE (pk_script out) of
                Right [ OP_RETURN, OP_PUSHDATA dat _ ] ->
                    if header `BSR.isPrefixOf` dat then
                        case decodeAllLE (BSR.drop (BSR.length header) dat) of
                            Right hash -> Just hash
                            _ -> Nothing
                    else
                        Nothing
                _ -> Nothing
