-- this module is in charge of the storage of block chain
-- without any validation

module Tucker.Storage.Block (
    Height,
    Branch(..),
    Chain(..),

    initChain,
    branchHeights,
    
    blocksAtHeight,
    blockAtHeight,
    blockWithHash,

    hasFullBlock,
    toFullBlockNode,

    insertBlock,
    tryFixBranch,

    allBranches,
    topNBlocks,

    saveBlock,

    addOrphan,
    removeOrphan,
    orphanList,

    hasBlockInChain,
    hasBlockInOrphan,
    hasBlock
    -- searchBranch,
    -- searchBranchHash,
    -- searchBranchHeight
) where

import Data.Int
import Data.Word
import Data.List hiding (map)
import qualified Data.Set.Ordered as OSET

import Control.Monad
import Control.Applicative

import Debug.Trace

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.Error
import Tucker.IOMap
import Tucker.DeepSeq

type Height = Int64
-- height starts from 0(genesis)

data Branch
    = BlockNode {
        prev_node  :: Maybe Branch,
        block_data :: Block, -- NOTE: block_data only contains a block header
        cur_height :: Height,
        acc_diff   :: Difficulty
    } -- deriving (Show)

instance Show Branch where
    show node = "BlockNode " ++ show (block_data node)

instance NFData Branch where
    rnf (BlockNode prev_node block_data cur_height acc_diff) =
        rnf prev_node `seq`
        rnf block_data `seq`
        rnf cur_height `seq`
        rnf acc_diff

instance Eq Branch where
    (BlockNode { block_data = b1 }) == (BlockNode { block_data = b2 })
        = b1 == b2

instance Ord Branch where
    compare (BlockNode { cur_height = h1 }) (BlockNode { cur_height = h2 })
        = compare h1 h2

data Chain =
    Chain {
        chain_conf    :: TCKRConf,

        bucket_block  :: DBBucket Hash256 (Height, Block),
        bucket_chain  :: DBBucket Height Hash256,

        saved_height  :: Height, -- hight of the lowest block stored in memory

        orphan_pool   :: OSET.OSet Block,
        buffer_chain  :: Maybe Branch, -- NEVER use buffer_chain alone
        -- replace the old buffer chain when a new buffer chain is formed
        -- the length of the buffer chain should >= tckr_max_tree_insert_depth
        edge_branches :: [Branch]
    }

-- structure
-- -----
-- |
-- |
-- | block nodes in disk(recorded by bucket_block and bucket_chain)
-- | no branch from here
-- |
-- -----
-- | <- block at height saved_height
-- |
-- | buffer chain
-- |-----
-- |    | posssible branch originating from the buffer chain
-- -----|
-- |    |
-- |    |
-- |    ------
-- |         | edge branches
-- |-----    |
-- |    |    |
-- |    |    + <- branch front
-- |    +
-- +

instance NFData Chain where
    rnf (Chain {
        buffer_chain = buffer_chain,
        edge_branches = edge_branches
    }) =
        rnf buffer_chain `seq` rnf edge_branches

initChain :: TCKRConf -> Database -> IO Chain
initChain conf@(TCKRConf {
    tckr_bucket_block_name = block_name,
    tckr_bucket_chain_name = chain_name,

    tckr_genesis_raw = genesis_raw,
    tckr_max_tree_insert_depth = max_depth
}) db = do
    bucket_block <- openBucket db block_name
    bucket_chain <- openBucket db chain_name

    let chain = Chain {
            chain_conf = conf,

            bucket_block = bucket_block,
            bucket_chain = bucket_chain,

            saved_height = 0,

            orphan_pool = OSET.empty,
            
            buffer_chain = Nothing,
            edge_branches = []
        }

    entries <- countIO bucket_chain
    let height = entries - 1 :: Height

    putStrLn $ "a tree of height " ++ show entries ++ " - 1 found in database"

    if entries == 0 then
        -- empty chain
        -- load genesis to the memory
        case decodeLE genesis_raw of
            (Left err, _) -> error $ "genesis decode error: " ++ show err

            (Right genesis, _) -> do
                insertIO bucket_block (block_hash genesis) (0, genesis)

                return $ chain {
                    edge_branches = [BlockNode {
                        prev_node = Nothing,
                        block_data = blockHeader genesis,

                        acc_diff = targetBDiff (hash_target genesis),
                        cur_height = 0
                    }]
                }
    else do
        -- height >= 0

        -- load at least tckr_max_tree_insert_depth blocks into the edge_branches
        let range = take max_depth [ height, height - 1 .. 0 ]

        -- in descending order of height
        hashes <- maybeCat <$> mapM (lookupIO bucket_chain) range
               :: IO [Hash256]

        res    <- maybeCat <$> mapM (lookupAsIO bucket_block) hashes
               :: IO [(Height, BlockHeader)] -- read headers only
        
        let fold_proc (height, BlockHeader block) Nothing =
                Just $ BlockNode {
                    prev_node = Nothing,
                    block_data = block,
                    acc_diff = targetBDiff (hash_target block),
                    cur_height = height
                }

            fold_proc (height, BlockHeader block) (Just node) =
                Just $ BlockNode {
                    prev_node = Just node,
                    block_data = block,
                    acc_diff = targetBDiff (hash_target block) + acc_diff node,
                    cur_height = height
                }

        -- print height

        -- print (height, max_depth, range)

        return $ case foldr fold_proc Nothing res of
            Nothing -> error "corrupted database(height not correct)"
            Just branch ->
                chain {
                    edge_branches = [branch],
                    saved_height = last range
                }

branchHeights :: Chain -> [Height]
branchHeights (Chain { edge_branches = branches }) =
    map cur_height branches

branchHeight :: Branch -> Height
branchHeight = cur_height

searchBranch' :: (Branch -> Bool) -> Branch -> (Branch, Maybe Branch)
searchBranch' pred node@(BlockNode {
    prev_node = mprev
}) =
    if pred node then (node, Just node)
    else case mprev of
        Nothing -> (node, Nothing)
        Just prev -> searchBranch' pred prev

-- edge_branches and buffer_chain are the only place
-- to store BlockNode
searchBranch :: Chain -> (Branch -> Bool) -> Branch -> Maybe Branch
searchBranch (Chain {
    edge_branches = branches,
    buffer_chain = mbuffer_chain
}) pred branch =
    let (prev, res) = searchBranch' pred branch in
    
    case res of
        Just _ -> res -- found, no problem
        Nothing -> do -- maybe monad
            -- if not found, we have to consider two situations
            -- 1. the search has reached the real end(end of buffer chain)
            --    so no further search for block in memory is possible
            -- 2. the search reached the end of edge_branches, but has not
            --    reached the buffer chain, in this way, the search can be continued
            --    by researching on the buffer chain

            -- we have to compare the height of the end block
            -- and the height of the buffer chain to determine
            -- the situation

            bufc@(BlockNode {
                cur_height = bufc_height
            }) <- mbuffer_chain

            if cur_height prev == bufc_height + 1 then
                -- it is connected to the buffer chain
                -- search on the buffer chain & return the
                -- final result
                snd (searchBranch' pred bufc)
            else
                -- not connected, end search
                Nothing

searchBranchHash chain hash =
    searchBranch chain ((== hash) . block_hash . block_data)

searchBranchHeight chain height =
    searchBranch chain ((== height) . cur_height)

-- public functions

-- blocks at a specific height
blocksAtHeight :: Chain -> Height -> IO [Block]
blocksAtHeight chain height =
    (maybeCat <$>) $ forM (edge_branches chain) $
        \b -> blockAtHeight chain b height

-- block at a specific height on a branch
blockAtHeight :: Chain -> Branch -> Height -> IO (Maybe Block)
blockAtHeight chain@(Chain {
    bucket_chain = bucket_chain,
    bucket_block = bucket_block,
    saved_height = saved_height
}) branch@(BlockNode {
    cur_height = max_height
}) height =
    if height > max_height || height < 0 then
        return Nothing
    else if height >= saved_height then do
        -- the block should be in memory
        return (block_data <$> searchBranchHeight chain height branch)
    else do
        -- the block should be in the db
        mhash <- lookupIO bucket_chain height
        
        case mhash of
            Nothing -> return Nothing
            Just hash -> do
                mpair <- lookupIO bucket_block hash
                return (snd <$> mpair)

-- block with the given hash on a branch
blockWithHash :: Chain -> Branch -> Hash256 -> IO (Maybe Branch)
blockWithHash chain@(Chain {
    bucket_chain = bucket_chain,
    bucket_block = bucket_block,
    saved_height = saved_height
}) branch@(BlockNode {
    cur_height = max_height
}) hash =
    case searchBranchHash chain hash branch of
        Just bn -> return (Just bn)
        Nothing -> do
            mres <- lookupIO bucket_block hash

            case mres of
                Nothing -> return Nothing
                Just (height, block) -> do
                    mhash <- lookupIO bucket_chain height

                    -- print (isPartial (txns block))
        
                    -- the block is indeed in the chain
                    if height < saved_height && mhash == Just hash then
                        -- create a tmp node to store the result
                        return $ Just $ BlockNode {
                            acc_diff = undefined,
                            cur_height = height,
                            block_data = block,
                            prev_node = Nothing
                        }
                    else
                        return Nothing -- not in the chain(probably due to data lost)

hasFullBlock :: Branch -> Bool
hasFullBlock = isFullBlock . block_data

toFullBlockNode :: Chain -> Branch -> IO (Maybe Branch)
toFullBlockNode (Chain {
    bucket_block = bucket_block
}) node@(BlockNode {
    block_data = block
}) =
    if isFullBlock block then return (Just node)
    else do
        mres <- lookupIO bucket_block (block_hash block)

        case mres of
            Nothing -> return Nothing -- full block not found
            Just (_, block) ->
                -- replace with full block
                return (Just (node { block_data = block }))

-- find prev block and insert the block to the chain
insertBlock :: Chain -> Block -> Maybe (Branch, Chain)
insertBlock chain@(Chain {
    edge_branches = edge_branches
}) block@(Block {
    hash_target = hash_target,
    prev_hash = prev_hash
}) = do
    let search = searchBranchHash chain prev_hash

    -- find the first appearing branch node
    prev_bn <- foldl (<|>) Nothing (map search edge_branches)

    -- construct new node
    let new_node =
            BlockNode {
                prev_node = Just prev_bn,
                block_data = blockHeader block, -- only store the header

                cur_height = cur_height prev_bn + 1,
                acc_diff = acc_diff prev_bn + targetBDiff hash_target
            }

    -- previous block found, inserting to the tree
    let new_branches =
            case elemIndex prev_bn edge_branches of
                -- check if the prev_bn is at the top of branches
                Just leaf_idx -> replace leaf_idx new_node edge_branches
                Nothing -> new_node : edge_branches
    
    return (new_node, chain { edge_branches = new_branches })

-- difference between the max and the second max value
minTopDiff :: Real t => [t] -> t
minTopDiff lst =
    let
        (max1, i) = maximum $ zip lst [0..]
        remain = take i lst ++ drop (i + 1) lst
        max2 = maximum remain
    in
        if null remain then max1
        else max2 - max1

-- in ascending order of height
branchToBlockList' :: [(Height, Block)] -> Maybe Branch -> [(Height, Block)]
branchToBlockList' cur Nothing = cur
branchToBlockList' cur (Just (BlockNode {
    cur_height = height,
    block_data = block,
    prev_node = mprev
})) = branchToBlockList' ((height, block) : cur) mprev

branchToBlockList = branchToBlockList' [] . Just

-- save the height -> hash mappings
-- in the branch to the db
saveBranch :: Chain -> Branch -> IO ()
saveBranch (Chain {
    bucket_chain = bucket_chain
}) branch = do
    -- ascending order of height
    let pairs = branchToBlockList branch

    if not (null pairs) then do
        -- save height
        let (height, _) = last pairs

        forM_ pairs $ \(height, (Block { block_hash = hash })) ->
            insertIO bucket_chain height hash
    else
        return ()

-- save block into the block map
saveBlock :: Chain -> Branch -> IO ()
saveBlock chain branch =
    insertIO (bucket_block chain) hash (cur_height branch, block)
    where
        block = block_data branch
        hash = block_hash block

-- try to fix the highest branch to save some memory
tryFixBranch :: Chain -> IO (Maybe Chain)
tryFixBranch chain@(Chain {
    chain_conf = conf,
    buffer_chain = buffer_chain,
    edge_branches = branches
}) = do
    let depth =
            if length branches > 1 then
                minTopDiff $ map cur_height branches
            else if not (null branches) then
                fi $ length (branchToBlockList (head branches))
            else 0

        winner@(BlockNode {
            prev_node = mprev
        }) = maximum branches

    -- print depth

    if fi depth >= tckr_max_tree_insert_depth conf then
        case mprev of
            -- only one node in the branch -> keep the original chain
            Nothing -> return Nothing

            -- remove loser branches and replace the buffer chain
            Just prev -> do
                -- save the whole winner chain
                -- so that everything have now is in
                -- the db, and utxo can be saved as well
                saveBranch chain winner

                return $ Just $ chain {
                    -- update saved_height
                    saved_height =
                        case buffer_chain of
                            Nothing -> saved_height chain
                            Just bufc -> cur_height bufc + 1,

                    buffer_chain = Just prev,
                    edge_branches = [winner {
                        prev_node = Nothing
                    }]
                }

    else return Nothing

takeBranch' :: Int -> Maybe Branch -> [Branch]
takeBranch' n Nothing = []
takeBranch' 0 _ = []
takeBranch' n (Just branch@(BlockNode {
    prev_node = prev
})) = branch : takeBranch' (n - 1) prev

takeBranch n branch =
    if n >= 0 then takeBranch' n (Just branch)
    else []

allBranches = edge_branches

-- topNBlocks chain max_number_of_block
-- take maxn from each branch
-- highest height first
-- topNBlocks :: Chain -> Int -> IO [Block]
-- topNBlocks (Chain {
--     edge_branches = branches
-- }) maxn =
--     return $
--     map block_data $
--     sortBy (\a b -> compare (cur_height b) (cur_height a))
--            (concatMap (takeBranch maxn) branches)

-- top n blocks of a paticular branch(if height < n, return height blocks)
-- in descending order of heights
topNBlocks :: Chain -> Branch -> Int -> IO [Block]
topNBlocks chain branch n' =
    maybeCat <$> mapM (blockAtHeight chain branch) range
    where
        n = fi n'
        height = branchHeight branch
        range = [ height, height - 1 .. height - n + 1 ]

addOrphan :: Chain -> Block -> Chain
addOrphan chain block =
    chain {
        orphan_pool = orphan_pool chain OSET.|> block
    }

removeOrphan :: Chain -> Block -> Chain
removeOrphan chain block =
    chain {
        orphan_pool = block `OSET.delete` orphan_pool chain
    }

orphanList :: Chain -> [Block]
orphanList = OSET.toAscList . orphan_pool

-- received before && is in a branch
hasBlockInChain :: Chain -> Block -> IO Bool
hasBlockInChain chain@(Chain {
    bucket_block = bucket_block
}) block@(Block {
    block_hash = hash
}) = do
    mres <- lookupAsIO bucket_block hash :: IO (Maybe (Height, Placeholder))
    
    case mres of
        Nothing -> return False
        Just (height, _) ->
            (not . null) <$> blocksAtHeight chain height

hasBlockInOrphan :: Chain -> Block -> Bool
hasBlockInOrphan chain block =
    block `OSET.member` orphan_pool chain

-- search two places for the block: block db and the orphan pool
hasBlock :: Chain -> Block -> IO Bool
hasBlock chain block =
    (||) <$> hasBlockInChain chain block
         <*> return (hasBlockInOrphan chain block)
