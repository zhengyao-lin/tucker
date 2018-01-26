{-# LANGUAGE DuplicateRecordFields #-}

-- block chain implementation

module Tucker.Chain.Object where

import Data.Int -- hiding (map, findIndex, null)
import Data.Word
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

data BlockTree = BlockTree [[Block]] deriving (Eq, Show) -- possible multiple roots
data BlockChain = BlockChain [Block] deriving (Eq, Show)

-- BlockTreePart prev_hash 
data BlockTreePart = BlockTreePart {
        prev_hashes :: [Hash256], -- if prev_hashes == [], the layers starts from genesis
        layers      :: [[Block]]
    } deriving (Eq, Show)

instance Monoid BlockTreePart where
    mempty = emptyTreePart
    mappend a b =
        case linkTreePart a b of
            -- hopefully somebody will catch it
            Nothing -> throw $ TCKRError "tree part not connectable"
            Just c -> c

instance Encodable BlockTreePart where
    encode end (BlockTreePart hashes layers) =
        mconcat [
            encodeVList end hashes,

            e $ vintlen layers,

            mconcat $ map
                (encodeVList end . map blockToBlockPayloadHashed) layers    
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

            vintlen = VInt . fromIntegral . length

instance Decodable BlockTreePart where
    decoder = do
        hashes <- vlistD decoder
        VInt layer_count <- decoder

        layers <- forM [ 1 .. layer_count ] $ const $ vlistD decoder

        let all_blocks = concat layers
            init_part = BlockTreePart hashes []

        if null all_blocks then
            return init_part
        else
            -- foldM insertToTreePart init_part all_blocks
            case foldM insertToTreePart init_part all_blocks of
                Left err -> fail $ "inserting failed: " ++ show err
                Right new_part -> return new_part

data Block =
    Block {
        block_hash  :: Hash256,

        vers        :: Int32,

        prev_hash   :: Hash256,
        prev_block  :: Maybe Block,
        next_block  :: [Block],
        -- merkle_root :: Hash256,

        timestamp   :: Word32,
        difficulty  :: Word32,
        nonce       :: Word32,

        txns        :: [TxPayload]
    }

self_prev_block = Tucker.Chain.Object.prev_block

instance Show Block where
    show (Block { block_hash = hash }) = "Block " ++ show hash

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
merkleRoot' [] = [nullHash256]
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
    and [
        hashBlock block == block_hash block,
        case self_prev_block block of
            Nothing -> True -- if prev_block == Nothing, prev_hash is not checked
            Just prev ->
                prev_hash block == block_hash prev &&
                block `elem` next_block prev
    ]
    -- TODO: add more here

-- in reversed order, i.e., last chain == genesis block
-- isValidChain' :: [Block] -> Bool
-- isValidChain' ([genesis]) = -- genesis block
--     Tucker.Chain.Object.prev_block genesis == Nothing &&
--     isValidBlock genesis

-- isValidChain' (newest:next:rest) =
--     Tucker.Chain.Object.prev_block newest == Just next &&
--     isValidBlock newest

-- layers in reversed order
isValidTree' :: [[Block]] -> Maybe [Hash256] -> Bool

isValidTree' [] mhashes = mhashes == Just [] || mhashes == Nothing

isValidTree' [roots] (Just prev_hashes) =
    all (\block ->
        prev_hash block `elem` prev_hashes &&
        self_prev_block block == Nothing &&
        isValidBlock block) roots

isValidTree' [roots] Nothing =
    all (\block -> self_prev_block block == Nothing && isValidBlock block) roots

isValidTree' (child:parent:rest) h =
    all (\block -> isValidBlock block && check_prev_block block) child &&
    isValidTree' (parent:rest) h
    where
        check_prev_block block =
            case self_prev_block block of
                Nothing -> False
                Just prev -> prev `elem` parent

isValidChain :: BlockChain -> Bool
isValidChain (BlockChain chain) = isValidTree' (map (:[]) $ reverse chain) Nothing

isValidTree :: BlockTree -> Bool
isValidTree (BlockTree layers) = isValidTree' (reverse layers) Nothing

isValidTreePart :: BlockTreePart -> Bool
isValidTreePart (BlockTreePart hashes layers) =
    isValidTree' (reverse layers) (Just hashes)

-- break/link tree parts
-- breakTree

treeToTreePart :: BlockTree -> BlockTreePart
treeToTreePart (BlockTree layers) = BlockTreePart [] layers

-- break from genesis
splitTreePart :: Int -> BlockTreePart -> (BlockTreePart, BlockTreePart)
splitTreePart n (BlockTreePart hashes layers) =
    (BlockTreePart hashes (removeNext prev),
     BlockTreePart (lastHash prev) (removePrev next))
    where
        (prev, next) = splitAt n layers

        lastHash layers =
            if null layers then []
            else map block_hash $ last prev

        removePrev layers =
            if null layers then []
            else map (\b -> b { Tucker.Chain.Object.prev_block = Nothing }) (head layers) : tail layers
        
        removeNext layers =
            if null layers then []
            else init layers ++ [ map (\b -> b { next_block = [] }) (last layers) ]

linkTreePart :: BlockTreePart -> BlockTreePart -> Maybe BlockTreePart
linkTreePart (BlockTreePart h1 l1) (BlockTreePart h2 l2) =
    if null l1 then
        Just $ BlockTreePart h2 l2
    else if null l2 then
        Just $ BlockTreePart h1 l1
    else if sort h2 == sort last_l1_hash then
        -- check if hash matches
        Just $ BlockTreePart h1 (conn l1 l2)
    else
        Nothing
    where
        last_l1_hash = map block_hash $ last l1

        conn l1 l2 =
            let head_l2_hash = head l2 in
            init l1 ++
            [ map (\b -> b {
                    next_block = filter ((== block_hash b) . prev_hash) head_l2_hash
                }) (last l1) ] ++ l2

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

blockToBlockPayloadHashed :: Block -> BlockPayloadHashed
blockToBlockPayloadHashed block@(Block {
    block_hash = block_hash,
    vers = vers,

    prev_hash = prev_hash,

    timestamp = timestamp,
    difficulty = difficulty,
    nonce = nonce,

    txns = txns
}) =
    BlockPayloadHashed
        block_hash
        BlockPayload {
            header = BlockHeader {
                vers = vers,
            
                prev_block = prev_hash,
                merkle_root = merkleRoot block,

                timestamp = timestamp,
                diff_bits = difficulty,
                nonce = nonce,

                txn_count = VInt $ fromIntegral $ length txns
            },
        
            txns = txns
        }

-- blockPaylodHashedToBlock bph prev_block prev_block_hash
blockPaylodHashedToBlock :: BlockPayloadHashed -> Maybe Block -> Hash256 -> Block
blockPaylodHashedToBlock (BlockPayloadHashed hash (BlockPayload {
    header = BlockHeader {
        vers = vers,

        prev_block = prev_block,
        timestamp = timestamp,
        diff_bits = bits,
        nonce = nonce
    },
    txns = txns
})) mprev prev_hash =
    Block {
        block_hash = hash,

        vers = vers,

        prev_hash = prev_hash,

        prev_block = mprev,
        next_block = [],

        timestamp = timestamp,
        difficulty = bits,
        nonce = nonce,

        txns = txns
    }

-- assuming the given previous block is valid
linkToBlock :: BlockPayloadHashed -> Maybe Block -> Maybe (Maybe Block, Block)
linkToBlock bph mprev =
    let
        prev_hash = case mprev of
            Nothing -> nullHash256
            Just prev -> block_hash prev

        new_mprev = mprev >>= \prev@(Block { next_block = prev_next_block }) ->
            return $ prev {
                next_block = prev_next_block ++ [new_block]
            }

        new_block = blockPaylodHashedToBlock bph new_mprev prev_hash
    in
        if isValidBlock new_block then
            Just (new_mprev, new_block)
        else
            Nothing

isBlockInTree' :: [[Block]] -> Hash256 -> Bool
isBlockInTree' layers hash =
    findIndex ((== hash) . block_hash) (reverse $ concat layers) /= Nothing

isBlockInTree :: BlockTree -> Hash256 -> Bool
isBlockInTree (BlockTree layers) hash = isBlockInTree' layers hash

-- try to find right place for the new block payload
-- return Right block to return new block if succeeds
-- return Left error if no suitable prev_block is found

-- mroots is used for tree part to allow non-genesis blocks to be inserted
-- given a set of authoritive prev_blocks
insertToTree' :: [[Block]] -> BlockPayloadHashed -> Maybe [Hash256] -> Either TCKRError BlockTree
insertToTree' layers bph@(BlockPayloadHashed hash payload) mroots =
    let -- layers = rootsToLayers roots
        prev = Tucker.Msg.prev_block $ header payload
        rlayers = reverse layers
        found = dropWhile (not . elem prev . map block_hash) $ rlayers
        found_i = length layers - length found
    in
        if isBlockInTree' layers hash then
            Left $ TCKRError "blcok exists"
        else if null found then
            -- try using it as a genesis/first layer of a tree part

            let insertToRoot layers new =
                    (rootsOf' layers ++ [new]) : drop 1 layers

            in case mroots of
                -- if mroots is given, search mroots to see if prev exists
                Just roots ->
                    let prev_hash = Tucker.Msg.prev_block (header payload) in

                    if prev_hash `elem` roots then
                        let new = blockPaylodHashedToBlock bph Nothing prev_hash in
                        Right $ BlockTree (insertToRoot layers new)
                    else
                        Left $ TCKRError "parent not found"

                -- mroots not given, try to insert as a genesis
                Nothing ->
                    let res = linkToBlock bph Nothing in

                    case res of
                        Nothing -> Left $ TCKRError "parent not found"
                        Just (Nothing, new) ->
                            Right $ BlockTree (insertToRoot layers new)

        else -- previous block found
            let
                prev_layer = head found
                Just i = findIndex ((== prev) . block_hash) prev_layer
                res = linkToBlock bph (Just $ prev_layer !! i)
            in case res of
                Nothing -> Left $ TCKRError "illegal block"
                Just (Just new_prev_block, new) ->
                    Right $ BlockTree $ reverse $ concat [
                        take (found_i - 1) rlayers,                  -- layers above (found - 1) layer
                        [ layerAt' rlayers (found_i - 1) ++ [new] ], -- the new layer inserting to
                        [ replace i new_prev_block prev_layer ],     -- the prev layer updating
                        drop 1 found                                 -- layers below the found later
                    ]

insertToTree :: BlockTree -> BlockPayloadHashed -> Either TCKRError BlockTree
insertToTree (BlockTree layers) bph = insertToTree' layers bph Nothing

insertToTreePart :: BlockTreePart -> BlockPayloadHashed -> Either TCKRError BlockTreePart
insertToTreePart (BlockTreePart hashes layers) bph = do
    (BlockTree new_layers) <- insertToTree' layers bph (Just hashes)
    return $ BlockTreePart hashes new_layers

treeToSet :: BlockTree -> SET.Set Hash256
treeToSet (BlockTree layers) = SET.map block_hash $ SET.fromList $ concat layers

chainToSet :: BlockChain -> SET.Set Hash256
chainToSet (BlockChain chain) = SET.map block_hash $ SET.fromList chain

emptyTree = BlockTree []
emptyChain = BlockChain []
emptyTreePart = BlockTreePart [] []

treeHeight :: BlockTree -> Int
treeHeight (BlockTree layers) = length layers

treeLatest :: BlockTree -> [Block]
treeLatest (BlockTree layers) =
    if null layers then [] else last layers

chainHeight :: BlockChain -> Int
chainHeight (BlockChain chain) = length chain

chainToTree :: BlockChain -> BlockTree
chainToTree (BlockChain chain) = BlockTree $ map (:[]) chain

treePartToTree :: BlockTreePart -> Maybe BlockTree
treePartToTree (BlockTreePart [] layers) = Just $ BlockTree layers
treePartToTree _ = Nothing -- previous roots not empty
