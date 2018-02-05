{-# LANGUAGE DuplicateRecordFields #-}

-- block chain implementation

module Tucker.Chain.Object where

import Data.Int -- hiding (map, findIndex, null)
import Data.Word
import Data.Maybe
import Data.List hiding (map)
import qualified Data.Set as SET

import Control.Monad
import Control.Monad.Loops

import Control.Exception

import Debug.Trace

import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Auth
import Tucker.Error

-- data BlockTree = BlockTree [[Block]] deriving (Eq, Show)
-- data Block

-- type TreeHeight = Int

-- simple in-mem representation of block chain
-- Chain root blocks

-- a main chain is a Chain with root == nullHash256
-- a branch is a Chain with root /= nullHash256

data Chain = Chain {
        root_hash :: Hash256,
        chain     :: [Block]
    } deriving (Eq, Show)

-- monoid
-- encode/decode
-- split
-- insert

instance Monoid Chain where
    mempty = Chain nullHash256 []
    mappend c1 c2 =
        case linkChain c1 c2 of
            Just c -> c
            Nothing -> throw $ TCKRError "chain not match"

instance Encodable Chain where
    encode end (Chain root chain) =
        encode end root <>
        encodeVList end chain

instance Decodable Chain where
    decoder = Chain <$> decoder <*> vlistD decoder

lastHash :: Chain -> Hash256
lastHash (Chain root c1) =
    if null c1 then root
    else block_hash $ last c1

-- 1. Chain hash (c1 ++ c2)
--    -> Chain hash c1, Chain (last c1) c2
-- 2. Chain hash ([] ++ c2)
--    -> Chain hash [], Chain hash c2
-- 3. Chain hash (c1 ++ [])
--    -> Chain hash c1, Chain (last c1) []
-- 4. Chain hash ([] ++ [])
--    -> Chain hash [], Chain hash []
splitChain :: Int -> Chain -> (Chain, Chain)
splitChain at (Chain root chain) =
    (chain1, chain2)
    where
        (c1, c2) = splitAt at chain
        chain1 = Chain root c1
        chain2 = Chain (lastHash chain1) c2

linkChain :: Chain -> Chain -> Maybe Chain
linkChain prev@(Chain r1 c1) (Chain r2 c2) =
    if lastHash prev == r2 then
        -- root_hash match
        Just $ Chain r1 (c1 ++ c2)
    else
        Nothing

-- forkChain original_chain on -> (common ancestor chain, main branch, new empty branch)
forkChain :: Int -> Chain -> (Chain, Chain, Chain)
forkChain on chain =
    (common, main, Chain root [])
    where
        (common, main@(Chain root _)) = splitChain on chain

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
}) = head $ merkleRoot' (map (stdHash256 . encodeLE) txns)

{-

what do we need to check with a block

1. duplication
2. non-empty transaction
3. block hash satisfy diff_bits
4. timestamp < 2 hours in the future
5. first transaction is coinbase
6. partial transaction check (x)
7. total sig_script op count < MAX_BLOCK_SIGOPS
8. check merkle hash

if orphan then
    put into the orphan pool, query peer, done
else
    9. check diff_bits match the difficulty rules
    10. timestamp > median time of last 11 blocks
    11. check old block hash (x)

    if block in main branch then
        
    else if block in side branch then
        if side branch <= main branch then
        else

-}

--------------------------------------------------------

-- data BlockTreePartInfo =
--     BlockTreePartInfo {
--         tree_height :: TreeHeight
--     } deriving (Show)

-- -- BlockTreePart prev_hash 
-- data BlockTreePart = BlockTreePart {
--         prev_hashes :: [Hash256], -- if prev_hashes == [], the layers starts from genesis
--         layers      :: [[Block]]
--     } deriving (Eq, Show)

-- instance Monoid BlockTreePart where
--     mempty = emptyTreePart
--     mappend a b =
--         case linkTreePart a b of
--             -- hopefully somebody will catch it
--             Nothing -> throw $ TCKRError "tree part not connectable"
--             Just c -> c

-- instance Encodable BlockTreePart where
--     encode end (BlockTreePart hashes layers) =
--         encodeVList end hashes <>
--         encodeVList end (map (encodeVList end) layers)

-- instance Decodable BlockTreePart where
--     decoder = do
--         hashes <- vlistD decoder
--         layers <- vlistD (vlistD decoder)

--         return $ BlockTreePart hashes layers

-- -- break from genesis
-- splitTreePart :: Int -> BlockTreePart -> (BlockTreePart, BlockTreePart)
-- splitTreePart n (BlockTreePart hashes layers) =
--     (BlockTreePart hashes prev,
--      BlockTreePart (lastHash prev) (next))
--     where
--         (prev, next) = splitAt n layers

--         lastHash layers =
--             if null layers then []
--             else map block_hash $ last prev

-- linkTreePart :: BlockTreePart -> BlockTreePart -> Maybe BlockTreePart
-- linkTreePart (BlockTreePart h1 l1) (BlockTreePart h2 l2) =
--     if null l1 then
--         Just $ BlockTreePart h2 l2
--     else if null l2 then
--         Just $ BlockTreePart h1 l1
--     else if sort h2 == sort last_l1_hash then
--         -- check if hash matches
--         Just $ BlockTreePart h1 (l1 ++ l2)
--     else
--         Nothing
--     where
--         last_l1_hash = map block_hash $ last l1

-- emptyTreePart = BlockTreePart [] []

-- treePartHeight :: BlockTreePart -> TreeHeight
-- treePartHeight (BlockTreePart { layers = layers }) = fromIntegral $ length layers

-- treePartInfo :: BlockTreePart -> BlockTreePartInfo
-- treePartInfo part =
--     BlockTreePartInfo {
--         tree_height = treePartHeight part
--     }

-- treePartLatestHash :: BlockTreePart -> [Hash256]
-- treePartLatestHash (BlockTreePart prev_hashes layers) =
--     if null layers then prev_hashes
--     else map (block_hash) $ last layers
