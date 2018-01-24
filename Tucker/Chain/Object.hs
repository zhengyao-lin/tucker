{-# LANGUAGE DuplicateRecordFields #-}

-- block chain implementation

module Tucker.Chain.Object where

import Data.Int -- hiding (map, findIndex, null)
import Data.Word
import Data.List hiding (map)
import qualified Data.Set as SET

import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Auth
import Tucker.Error

data BlockTree = BlockTree [[Block]] deriving (Eq, Show, Read) -- possible multiple roots
data BlockChain = BlockChain [Block] deriving (Eq, Show, Read)

data Block =
    Block {
        block_hash  :: Hash256,

        vers        :: Int32,

        prev_block  :: Maybe Block,
        next_block  :: [Block],
        -- merkle_root :: Hash256,

        timestamp   :: Word32,
        difficulty  :: Word32,
        nonce       :: Word32,

        txns        :: [TxPayload]
    } deriving (Show, Read)

instance Eq Block where
    (Block { block_hash = h1 }) == (Block { block_hash = h2 })
        = h1 == h2

instance Ord Block where
    (Block { block_hash = h1 }) `compare` (Block { block_hash = h2 })
        = h1 `compare` h2

merkleParents :: [Hash256] -> [Hash256]
merkleParents [] = []
merkleParents [a] =
    [ Hash256FromBS . ba2bs . sha256 . sha256 $ hash256ToBS a <> hash256ToBS a ]

merkleParents (l:r:leaves) =
    (Hash256FromBS . ba2bs . sha256 . sha256 $ hash256ToBS l <> hash256ToBS r):merkleParents leaves

merkleRoot' :: [Hash256] -> [Hash256]
merkleRoot' [single] = [single]
merkleRoot' leaves = merkleRoot' $ merkleParents leaves

merkleRoot :: Block -> Hash256
merkleRoot (Block {
    txns = txns
}) = head $ merkleRoot' (map (Hash256FromBS . ba2bs . sha256 . sha256 . encodeLE) txns)

-- get hash of a block(in internal byte order)
-- sha256^2(vers + prev_block + merkle_root + time + diff + nonce)
hashBlock :: Block -> Hash256
hashBlock block@(Block {
    vers = vers,
    prev_block = prev_block,
    timestamp = timestamp,
    difficulty = bits,
    nonce = nonce
}) =
    Hash256FromBS . ba2bs . sha256 . sha256 $ mconcat [
        encodeLE vers,
        
        hash256ToBS $ case prev_block of
            Nothing -> nullHash256
            Just (Block { block_hash = block_hash }) -> block_hash,

        hash256ToBS $ merkleRoot block,
        encodeLE timestamp,
        encodeLE bits,
        encodeLE nonce
    ]

-- get blocks in (probably)random order from the P2P network
-- organize them into the block chain

unique :: Ord a => [a] -> [a]
unique lst = SET.toList (SET.fromList lst)

zipLayers :: [[[Block]]] -> [[Block]]
zipLayers = map (unique . concat) . transpose

rootToLayers :: Block -> [[Block]]
rootToLayers b@(Block { next_block = [] }) = []
rootToLayers (Block { next_block = next }) =
    next:(zipLayers $ map rootToLayers next)

rootsToLayers :: [Block] -> [[Block]]
rootsToLayers = zipLayers . map rootToLayers

-- trace back from a leaf node
traceChain' :: Block -> [Block]
traceChain' leaf@(Block { prev_block = Nothing }) = [leaf]
traceChain' leaf@(Block { prev_block = Just prev }) =
    traceChain' prev ++ [leaf]

-- fix a block tree into a single block chain(by selecting the longest route)
-- return Nothing if multiple chain is possible(with the same height)
fixTree :: BlockTree -> [BlockChain]
fixTree (BlockTree layers) =
    map (BlockChain . traceChain') $ last layers
    -- where layers = rootsToLayers roots

-- assuming the previous block is valid
isValidBlock :: Block -> Bool
isValidBlock block =
    hashBlock block == block_hash block

-- in reversed order, i.e., last chain == genesis block
isValidChain' :: [Block] -> Bool
isValidChain' ([genesis]) = -- genesis block
    Tucker.Chain.Object.prev_block genesis == Nothing &&
    isValidBlock genesis

isValidChain' (newest:next:rest) =
    Tucker.Chain.Object.prev_block newest == Just next &&
    isValidBlock newest

isValidChain :: BlockChain -> Bool
isValidChain (BlockChain chain) = isValidChain' $ reverse chain

-- blockPayloadToBlock payload previous_block hash
-- -> (updated_previous_block, new_block)
-- return Nothing if the previous block does not match

layerAt' :: [[Block]] -> Int -> [Block]
layerAt' layers i =
    if i >= length layers || i < 0 then []
    else layers !! i

rootsOf' :: [[Block]] -> [Block]
rootsOf' layers = layerAt' layers 0

rootsOf :: BlockTree -> [Block]
rootsOf (BlockTree layers) = rootsOf' layers

-- assuming the given previous block is valid
linkToBlock :: BlockPayloadHashed -> Maybe Block -> Maybe (Maybe Block, Block)
linkToBlock (BlockPayloadHashed hash (BlockPayload {
    header = BlockHeader {
        vers = vers,

        prev_block = prev_block,
        timestamp = timestamp,
        diff_bits = bits,
        nonce = nonce
    },
    txns = txns
})) prev =
    let
        new_block = Block {
            block_hash = hash,

            vers = vers,

            prev_block = prev,
            next_block = [],

            timestamp = timestamp,
            difficulty = bits,
            nonce = nonce,

            txns = txns
        }

        new_prev = case prev of
            Nothing -> Nothing
            Just prev@(Block { next_block = prev_next_block }) ->
                Just $ prev {
                    next_block = prev_next_block ++ [new_block]
                }
    in
        if isValidBlock new_block then
            Just (new_prev, new_block)
        else
            Nothing

isBlockInTree :: BlockTree -> Hash256 -> Bool
isBlockInTree (BlockTree layers) hash =
    findIndex ((== hash) . block_hash) (reverse $ concat layers) /= Nothing

-- try to find right place for the new block payload
-- return Just block to return new block if succeeds
-- return Nothing if no suitable prev_block is found
insertToTree :: BlockTree -> BlockPayloadHashed -> Either TCKRError BlockTree
insertToTree tree@(BlockTree layers) bph@(BlockPayloadHashed hash payload) =
    let -- layers = rootsToLayers roots
        prev = Tucker.Msg.prev_block $ header payload
        rlayers = reverse layers
        found = dropWhile (not . elem prev . map block_hash) $ rlayers
        found_i = length layers - length found
    in
        if isBlockInTree tree hash then
            Left $ TCKRError "blcok exists"
        else if null found then
            -- try using it as a genesis
            let res = linkToBlock bph Nothing in

            case res of
                Nothing -> Left $ TCKRError "parent not found"
                Just (Nothing, new) ->
                    Right $ BlockTree $ (rootsOf' layers ++ [new]) : drop 1 layers

        else let
            layer = head found
            Just i = findIndex ((== prev) . block_hash) layer
            res = linkToBlock bph (Just $ layer !! i)
        in case res of
            Nothing -> Left $ TCKRError "illegal block"
            Just (Just new_prev_block, new) ->
                Right $ BlockTree $ reverse $ concat [
                    take (found_i - 1) rlayers,                  -- layers above (found - 1) layer
                    [ layerAt' rlayers (found_i - 1) ++ [new] ], -- the new layer inserting to
                    [ replace i new_prev_block layer ],          -- the prev layer updating
                    drop 1 found                                 -- layers below the found later
                ]

treeToSet :: BlockTree -> SET.Set Hash256
treeToSet (BlockTree layers) = SET.map block_hash $ SET.fromList $ concat layers

chainToSet :: BlockChain -> SET.Set Hash256
chainToSet (BlockChain chain) = SET.map block_hash $ SET.fromList chain

emptyTree = BlockTree []
emptyChain = BlockChain []

treeHeight :: BlockTree -> Int
treeHeight (BlockTree layers) = length layers
