-- this module is in charge of the storage of block chain
-- without any validation

module Tucker.Storage.Block (
    Height,
    Branch(..),
    Chain,

    initChain,
    branchHeights,
    branchHeight,

    heightInDb,
    removeHeightInDb,
    
    blocksAtHeight,
    blockAtHeight,
    branchAtHeight,

    blockWithHash,
    branchWithHash,

    toFullBlockNode,
    toFullBlockNodeFail,

    insertBlock,
    tryFixBranch,

    allBranches,
    topNBlocks,

    saveMainBranch,
    saveBlock,

    prevBlockNode,

    addOrphan,
    removeOrphan,
    orphanList,

    hasBlockInChain,
    hasBlockInOrphan,

    isMainBranch,
    mainBranch,
    setMainBranch,

    commonAncestor,
    forkPath
    -- searchBranch,
    -- searchBranchHash,
    -- searchBranchHeight
) where

import Data.Int
import Data.Word
import Data.List hiding (map)
import qualified Data.Set as SET

import Control.Monad
import Control.Applicative

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.Error
import Tucker.IOMap
import Tucker.DeepSeq

data Branch
    = BlockNode {
        prev_node  :: Maybe Branch, -- NOTE: use realPrevNode instead
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
        chain_conf      :: TCKRConf,

        bucket_block    :: DBBucket Hash256 (Height, Block),
        bucket_chain    :: DBBucket Height Hash256,

        saved_height    :: Height, -- hight of the lowest block stored in memory

        orphan_pool_idx :: SET.Set Hash256,
        orphan_pool     :: [Block],
        buffer_chain    :: Maybe Branch, -- NEVER use buffer_chain alone
        -- replace the old buffer chain when a new buffer chain is formed
        -- the length of the buffer chain should >= tckr_max_tree_insert_depth

        -- the buffer chain should be transparent to external modules

        edge_branches   :: [Branch],

        main_branch     :: Int -- idx of the main branch
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

{-

structures:

1. chain map: Map Height Hash
2. block map: Map Hash (Height, Block)
3. editable tree

-}

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

            orphan_pool_idx = SET.empty,
            orphan_pool = [],
            
            buffer_chain = Nothing,
            edge_branches = [],

            main_branch = 0
        }

    entries <- quickCountIO bucket_chain
    let height = entries - 1 :: Height

    -- error "exit"

    tLnM ("a chain of height " ++ show height ++ " is found in the database")

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

                        acc_diff = targetBDiff conf (hash_target genesis),
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
                    acc_diff = targetBDiff conf (hash_target block),
                    cur_height = height
                }

            fold_proc (height, BlockHeader block) (Just node) =
                Just $ BlockNode {
                    prev_node = Just node,
                    block_data = block,
                    acc_diff = targetBDiff conf (hash_target block) + acc_diff node,
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

heightInDb :: Chain -> IO Height
heightInDb chain = (+(-1)) <$> quickCountIO (bucket_chain chain)

-- remove a block at a given height
-- NOTE: this function should only be used in internal modules
-- this function takes action immediately on the db
removeHeightInDb :: Chain -> Height -> IO ()
removeHeightInDb chain height =
    deleteIO (bucket_chain chain) height

branchHeights :: Chain -> [Height]
branchHeights (Chain { edge_branches = branches }) =
    map cur_height branches

branchHeight :: Branch -> Height
branchHeight = cur_height

-- max height of any buffer chain node
bufferChainHeight :: Chain -> Height
bufferChainHeight chain =
    case buffer_chain chain of
        Nothing -> saved_height chain - 1
        -- no buffer chain -> end of any edge branch - 1 is the max height
        Just node -> cur_height node

-- real previous node
-- if the node is at the end of a edge branch
-- it will try to search in the buffer chain
realPrevNode :: Chain -> Branch -> Maybe Branch
realPrevNode chain branch =
    case prev_node branch of
        Just n -> Just n
        Nothing ->
            -- search buffer chain
            if cur_height branch == bufferChainHeight chain + 1 then
                -- end of edge branch, previous node is the buffer chain
                buffer_chain chain
            else
                -- end of every node in mem
                Nothing

searchBranch :: Chain -> (Branch -> Bool) -> Branch -> Maybe Branch
searchBranch chain pred node =
    if pred node then Just node
    else case realPrevNode chain node of
        Nothing -> Nothing
        Just prev -> searchBranch chain pred prev

searchBranchHash chain hash =
    searchBranch chain ((== hash) . block_hash . block_data)

searchBranchHeight chain height =
    searchBranch chain ((== height) . cur_height)

createSingleNode :: Block -> Height -> Branch
createSingleNode block height =
    BlockNode {
        block_data = block,
        cur_height = height,
        acc_diff = error "accessing single node",
        prev_node = error "accessing single node"
    }

-- blocks at a specific height
blocksAtHeight :: Chain -> Height -> IO [Block]
blocksAtHeight chain height =
    (maybeCat <$>) $ forM (edge_branches chain) $
        \b -> blockAtHeight chain b height

blockAtHeight chain branch height =
    maybe Nothing (Just . block_data) <$> branchAtHeight chain branch height

-- block at a specific height on a branch
branchAtHeight :: Chain -> Branch -> Height -> IO (Maybe Branch)
branchAtHeight chain@(Chain {
    bucket_chain = bucket_chain,
    bucket_block = bucket_block,
    saved_height = saved_height
}) branch@(BlockNode {
    cur_height = max_height
}) height =
    if height > max_height || height < 0 then
        return Nothing
    else if height >= saved_height then
        -- the block should be in memory
        return (searchBranchHeight chain height branch)
    else do
        -- the block should be in the db
        mhash <- lookupIO bucket_chain height
        
        case mhash of
            Nothing -> return Nothing
            Just hash -> do
                mpair <- lookupIO bucket_block hash
                return $ case mpair of
                    Just (_, block) -> Just (createSingleNode block height)
                    Nothing -> Nothing

blockWithHash chain branch hash =
    maybe Nothing (Just . block_data) <$> branchWithHash chain branch hash

-- block with the given hash on a branch
branchWithHash :: Chain -> Branch -> Hash256 -> IO (Maybe Branch)
branchWithHash chain@(Chain {
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
                        return $ Just (createSingleNode block height)
                    else
                        return Nothing -- not in the chain(probably due to data lost)

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

toFullBlockNodeFail :: Chain -> Branch -> IO Branch
toFullBlockNodeFail chain node = do
    mres <- toFullBlockNode chain node

    case mres of
        Nothing -> error "failed to load full block(probably because of corrupt db)"
        Just node -> return node

-- find prev block and insert the block to the chain
insertBlock :: Chain -> Block -> Maybe (Branch, Chain)
insertBlock chain@(Chain {
    chain_conf = conf,
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
                acc_diff = acc_diff prev_bn + targetBDiff conf hash_target
            }

    -- tLnM (edge_branches, prev_bn, elemIndex prev_bn edge_branches)

    -- previous block found, inserting to the tree
    let new_branches =
            case elemIndex prev_bn edge_branches of
                -- check if the prev_bn is at the top of branches
                Just leaf_idx -> replace leaf_idx new_node edge_branches
                Nothing -> edge_branches ++ [ new_node ]
 
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
    prev_node = mprev -- use prev_node because saveBranch need to save edge branch only
})) =
    branchToBlockList' ((height, block) : cur) mprev

branchToBlockList = branchToBlockList' [] . Just

saveMainBranch chain = saveBranch chain (mainBranch chain)

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
saveBlock :: Chain -> Height -> Block -> IO ()
saveBlock chain height block =
    insertIO (bucket_block chain) hash (height, block)
    where hash = block_hash block

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

        main@(BlockNode {
            prev_node = mprev
        }) = mainBranch chain -- maximum branches

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
                saveBranch chain main

                return $ Just $ chain {
                    -- update saved_height
                    saved_height =
                        case buffer_chain of
                            Nothing -> saved_height chain
                            Just bufc -> cur_height bufc + 1,

                    buffer_chain = Just prev,
                    edge_branches = [main {
                        prev_node = Nothing
                    }]
                }

    else return Nothing

allBranches = edge_branches

-- in DESCENDING order of heights
takeBranch' :: Chain -> Int -> Maybe Branch -> [Branch]
takeBranch' chain n Nothing = [] -- running out of nodes
takeBranch' chain 0 mnode = [] -- end
takeBranch' chain n (Just node) =
    if n < 0 then []
    else
        node : takeBranch' chain (n - 1) (realPrevNode chain node)

takeBranch :: Chain -> Int -> Branch -> [Branch]
takeBranch chain n node = takeBranch' chain n (Just node)

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

-- previous node of a node
-- only garanteed to return Just when
-- the branch node is on the edge and has non-zero height
prevBlockNode :: Chain -> Branch -> IO (Maybe Branch)
prevBlockNode chain branch =
    branchAtHeight chain branch (cur_height branch - 1)

addOrphan :: Chain -> Block -> Chain
addOrphan chain block =
    if block_hash block `SET.member` (orphan_pool_idx chain) then chain
    else
        chain {
            orphan_pool_idx = SET.insert (block_hash block) (orphan_pool_idx chain),
            orphan_pool = orphan_pool chain ++ [block]
        }

removeOrphan :: Chain -> Block -> Chain
removeOrphan chain block =
    chain {
        orphan_pool_idx = SET.delete (block_hash block) (orphan_pool_idx chain),
        orphan_pool = delete block (orphan_pool chain)
    }

orphanList :: Chain -> [Block]
orphanList = orphan_pool

-- received before && is in a branch
hasBlockInChain :: Chain -> Hash256 -> IO Bool
hasBlockInChain chain@(Chain {
    bucket_block = bucket_block,
    bucket_chain = bucket_chain,
    saved_height = saved_height,
    edge_branches = branches
}) hash = do
    mres <- lookupAsIO bucket_block hash :: IO (Maybe (Height, Placeholder))
    
    case mres of
        Nothing -> return False
        Just (height, _) ->
            if height >= saved_height then do
                let found = maybeCat (map (searchBranchHash chain hash) branches)

                if null found then return False
                else return True
            else do
                mhash <- lookupIO bucket_chain height :: IO (Maybe Hash256)

                return $ case mhash of
                    Nothing -> False
                    Just hash' -> hash == hash'

hasBlockInOrphan :: Chain -> Hash256 -> Bool
hasBlockInOrphan chain hash =
    hash `SET.member` orphan_pool_idx chain

isMainBranch :: Chain -> Branch -> Bool
isMainBranch chain branch =
    case elemIndex branch (edge_branches chain) of
        Nothing -> error "failed to find corresponding branch"
        Just idx -> main_branch chain == idx

    -- all (cur_height branch >=) (map cur_height branches)

mainBranch :: Chain -> Branch
mainBranch chain = edge_branches chain !! main_branch chain

setMainBranch :: Chain -> Branch -> Chain
setMainBranch chain branch =
    case elemIndex branch (edge_branches chain) of
        Nothing -> error "failed to find corresponding edge branch"
        Just idx ->
            chain {
                main_branch = idx
            }

-- precedent: cur_height b1 == cur_height b2
commonAncestor' :: Chain -> Branch -> Branch -> Maybe Branch
commonAncestor' chain b1 b2 =
    if b1 == b2 then Just b1
    else do -- trace back 1 node
        p1 <- mp1
        p2 <- mp2
        commonAncestor' chain p1 p2

    where mp1 = realPrevNode chain b1
          mp2 = realPrevNode chain b2

commonAncestor :: Chain -> Branch -> Branch -> Maybe Branch
commonAncestor chain b1 b2 =
    if h1 > h2 then
        -- trace branch 1 back to height h2
        let mb1 = searchBranchHeight chain h2 b1
        in case mb1 of
            Nothing -> Nothing -- no common ancestor in mem
            Just b1 -> commonAncestor' chain b1 b2
    else -- h1 <= h2
        -- trim branch 2 instead
        let mb2 = searchBranchHeight chain h1 b2
        in case mb2 of
            Nothing -> Nothing -- no common ancestor in mem
            Just b2 -> commonAncestor' chain b1 b2

    where h1 = cur_height b1
          h2 = cur_height b2
          dh = if h1 > h2 then h1 - h2 else h2 - h1

-- get the different paths of two branches from the fork point
--               |      fork path 1      |
--               ------------------------->
--               |          b1
-- ---------------
--               |      b2
--               ----------------->
--               |  fork path 2  |
forkPath :: Chain -> Branch -> Branch -> ([Branch], [Branch])
forkPath chain b1 b2 =
    let mca = commonAncestor chain b1 b2
        h0 =
            case mca of
                Nothing -> error "no common ancestor found"
                Just ca -> cur_height ca

        h1 = cur_height b1
        h2 = cur_height b2
    in
        (takeBranch chain (fi (h1 - h0)) b1,
         takeBranch chain (fi (h2 - h0)) b2)
