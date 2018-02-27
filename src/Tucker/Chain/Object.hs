{-# LANGUAGE DuplicateRecordFields, ConstraintKinds #-}

-- block chain implementation

module Tucker.Chain.Object where

import Data.Int -- hiding (map, findIndex, null)
import Data.Word
import Data.Maybe
import Data.List hiding (map)
import qualified Data.Set as SET
import qualified Data.Foldable as FD
import qualified Data.ByteString as BSR
import qualified Data.Set.Ordered as OSET

import Control.Monad
import Control.Monad.Loops
import Control.Applicative
import Control.Monad.Morph
import Control.Monad.Trans.Resource

import Control.Exception

import Debug.Trace

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Auth
import Tucker.Conf
import Tucker.Error
import Tucker.DeepSeq

import Tucker.Chain.Tx

-- data BlockTree = BlockTree [[Block]] deriving (Eq, Show)
-- data Block

-- type TreeHeight = Int

-- simple in-mem representation of block chain
-- Chain root blocks

-- a main chain is a Chain with root == nullHash256
-- a branch is a Chain with root /= nullHash256

-- data Chain = Chain {
--         root_hash :: Hash256,
--         chain     :: [Block]
--     } deriving (Eq, Show)

type Height = Word64
-- height starts from 0(genesis)

data Branch
    = BlockNode {
        prev_node  :: Maybe Branch,
        block_data :: Block, -- NOTE: block_data only contains a block header
        cur_height :: Height,
        acc_diff   :: Difficulty
    } deriving (Show)

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

-- Branches fork_point_hash last_nodes
-- type Branches = [Branch]
    -- last nodes of branches
    -- all branches can be traced back to a common fork point

data Chain =
    Chain {
        -- block db contains all valid blocks(blocks that can be linked to the tree)
        -- received(including those on the side branch)
        std_conf      :: TCKRConf,

        db_global     :: Database,
        db_subs       :: [Database],
        
        bucket_block  :: DBBucket Hash256 (Height, Block),
        bucket_chain  :: DBBucket Height Hash256,

        tx_set        :: TxSet,

        saved_height  :: Height, -- hight of the lowest block stored in memory

        orphan_pool   :: OSET.OSet Block,
        buffer_chain  :: Maybe Branch, -- NEVER use buffer_chain alone
        -- replace the old buffer chain when a new buffer chain is formed
        -- the length of the buffer chain should >= tckr_max_tree_insert_depth
        edge_branches :: [Branch]
    }

instance NFData Chain where
    rnf (Chain {
        buffer_chain = buffer_chain,
        edge_branches = edge_branches
    }) =
        rnf buffer_chain `seq` rnf edge_branches

initChain :: TCKRConf -> ResIO Chain
initChain conf@(TCKRConf {
    tckr_db_path = db_path,
    tckr_tx_db_path = tx_db_path,
    tckr_bucket_block_name = block_name,
    tckr_bucket_chain_name = chain_name,

    tckr_genesis_raw = genesis_raw,
    tckr_max_tree_insert_depth = max_depth
}) = do
    db_global <- openDB def db_path
    db_tx     <- openDB def tx_db_path

    bucket_block <- lift $ openBucket db_global block_name
    bucket_chain <- lift $ openBucket db_global chain_name

    tx_set <- lift $ initTxSet conf db_tx

    let chain = Chain {
            std_conf = conf,

            db_global = db_global,
            db_subs = [ db_tx ],

            bucket_block = bucket_block,
            bucket_chain = bucket_chain,

            tx_set = tx_set,

            saved_height = 0,

            orphan_pool = OSET.empty,
            
            buffer_chain = Nothing,
            edge_branches = []
        }

    -- mheight <- lift $ get db_global "height"

    entries <- lift $ countB bucket_chain
    let height = entries - 1 :: Height

    lift $ putStrLn $ "a tree of height " ++ show entries ++ " - 1 found in database"

    if entries == 0 then
        -- empty chain
        -- load genesis to the memory
        case decodeLE genesis_raw of
            (Left err, _) -> error $ "genesis decode error: " ++ show err

            (Right genesis, _) -> do
                lift $ setB bucket_block (block_hash genesis) (0, genesis)

                return $ chain {
                    edge_branches = [BlockNode {
                        prev_node = Nothing,
                        block_data = blockHeader genesis,

                        acc_diff = targetBDiff (hash_target genesis),
                        cur_height = 0
                    }]
                }
    else lift $ do
        -- load at least tckr_max_tree_insert_depth blocks into the edge_branches
        let min_height =
                if height >= fi max_depth then
                    height - fi max_depth + 1
                else 0
            
            range =
                if height > 0 then [ min_height, min_height + 1 .. height ]
                else [0]

        hashes <- maybeCat <$> mapM (getB bucket_chain) range
               :: IO [Hash256]

        res    <- maybeCat <$> mapM (getAsB bucket_block) hashes
               :: IO [(Height, BlockHeader)] -- read headers only
        
        let fold_proc Nothing (height, BlockHeader block) =
                Just $ BlockNode {
                    prev_node = Nothing,
                    block_data = block,
                    acc_diff = targetBDiff (hash_target block),
                    cur_height = height
                }

            fold_proc (Just node) (height, BlockHeader block) =
                Just $ BlockNode {
                    prev_node = Just node,
                    block_data = block,
                    acc_diff = targetBDiff (hash_target block) + acc_diff node,
                    cur_height = height
                }

        -- print height

        return $ case foldl' fold_proc Nothing res of
            Nothing -> error "corrupted database(height not correct)"
            Just branch ->
                chain {
                    edge_branches = [branch],
                    saved_height = min_height
                }

branchHeights :: Chain -> [Height]
branchHeights (Chain { edge_branches = branches }) =
    map cur_height branches

forceChain :: Chain -> Chain
forceChain c = c {
        buffer_chain = force $ buffer_chain c,
        edge_branches = force $ edge_branches c
    }

-- load db from path
-- set orphan pool to Nothing
-- load at least tckr_max_tree_insert_depth blocks into the edge_branches
-- 

{-

what do we need to check with a block

1. duplication
2. non-empty transaction list
3. block hash satisfy diff_bits
4. timestamp < 2 hours in the future
5. first transaction is coinbase
6. partial transaction check (x)
7. total sig_script op count < MAX_BLOCK_SIGOPS
8. check merkle hash

if orphan then
    put into the orphan pool, query peer, done
else
    1. check diff_bits match the difficulty rules
    2. timestamp > median time of last 11 blocks
    3. check old block hash (x)

    if block in main branch then
        1. for all transaction besides coinbase:
            1. all input exists
            2. if input is coinbase, it must have COINBASE_MATURITY confirmations(depth of the block)
            3. inputs are unspent(in the main branch)
            4. sum of output <= sum of input
        2. delete duplicate transactions from the transaction pool
        3. relay
    else if block in side branch then
        if side branch <= main branch then
            add to the branch and do nothing
        else
            1. do the same check for all blocks from the fork point
            2. if reject, keep the original main branch

            3. put all valid transactions in side branch to the tx pool
            4. delete duplicate transactions from the transaction pool
            5. relay

9. try to connect orphan blocks to this new block if not rejected(from step 1)

what do we need to check for each transaction:

1. input exists(the input points to a valid block and a valid index)
2. if referenced transaction is a coinbase, make sure the depth of the input block is >= 100
3. verify signature(involves script)
4. referenced output is not spent
5. validity of values involved(amount of input, amount of output, sum(inputs) >= sum(outputs))

-}

-- search edge branches, if found, insert to the branch
-- else search chain, if found, check if it's too deep, abandon the block
-- if not too deep, load the required range of block into the edge

-- in ascending order of height
branchToBlockList' :: [(Height, Block)] -> Maybe Branch -> [(Height, Block)]
branchToBlockList' cur Nothing = cur
branchToBlockList' cur (Just (BlockNode {
    cur_height = height,
    block_data = block,
    prev_node = mprev
})) = branchToBlockList' ((height, block) : cur) mprev

branchToBlockList = branchToBlockList' [] . Just

-- search and return a block node
-- return (previous node, result)
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

insertToEdge :: Chain -> Block -> Maybe (Branch, Chain)
insertToEdge chain@(Chain {
    edge_branches = edge_branches
}) block@(Block {
    hash_target = hash_target,
    prev_hash = prev_hash
}) = do
    let search = searchBranchHash chain prev_hash

    -- trace "inserting!" $ return 0

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

blocksAtHeight :: Chain -> Height -> IO [Block]
blocksAtHeight chain height =
    (maybeCat <$>) $ forM (edge_branches chain) $
        \b -> blockAtHeight chain b height

blockAtHeight :: Chain -> Branch -> Height -> IO (Maybe Block)
blockAtHeight chain@(Chain {
    bucket_chain = bucket_chain,
    bucket_block = bucket_block,
    saved_height = saved_height
}) branch@(BlockNode {
    cur_height = max_height
}) height =
    if height > max_height then
        return Nothing
    else if height >= saved_height then do
        -- the block should be in memory
        return (block_data <$> searchBranchHeight chain height branch)
    else do
        -- the block should be in the db
        mhash <- getB bucket_chain height
        
        case mhash of
            Nothing -> return Nothing
            Just hash -> do
                mpair <- getB bucket_block hash
                return (snd <$> mpair)

-- get the block height in a given branch by a given hash
blockHeightByHash :: Chain -> Branch -> Hash256 -> IO (Maybe Height)
blockHeightByHash chain@(Chain {
    bucket_chain = bucket_chain,
    bucket_block = bucket_block,
    saved_height = saved_height
}) branch@(BlockNode {
    cur_height = max_height
}) hash =
    case searchBranchHash chain hash branch of
        Just (BlockNode {
            cur_height = height
        }) -> return $ Just height

        Nothing -> do
            mpair <- getAsB bucket_block hash :: IO (Maybe (Height, Placeholder))
            
            case fst <$> mpair of
                Nothing -> return Nothing
                Just height -> do
                    -- extra check to make sure the block is indeed exist
                    mhash <- getB bucket_chain height
                    
                    case mhash of
                        Nothing -> return Nothing -- is not in the chain
                        Just hash' ->
                            if height < saved_height &&
                               hash' == hash then return $ Just height
                            else
                                return Nothing -- not in the chain due to data lost

corrupt = reject "corrupted database"

hashTargetValid :: Chain -> Branch -> IO Bool
hashTargetValid chain@(Chain {
    std_conf = conf
}) branch@(BlockNode {
    cur_height = height,
    block_data = Block {
        hash_target = hash_target
    }
}) = do
    let change_cond = shouldDiffChange conf height
        expect_target =
            if tckr_use_special_min_diff conf then
                -- TODO: non-standard special-min-diff
                return $ fi tucker_bdiff_diff1
            else do
                mprev_1_block <- blockAtHeight chain branch (height - 1)

                let (Block { hash_target = old_target, timestamp = t2 }) =
                        maybe corrupt id mprev_1_block

                if not change_cond then
                    return old_target
                else do
                    mprev_2016_block <- blockAtHeight chain branch (height - 2016)

                    let (Block { timestamp = t1 }) = maybe corrupt id mprev_2016_block
                    
                    let actual_span = fi $ t2 - t1
                        expect_span = fi $ tckr_expect_diff_change_time conf

                        -- cal_span is in [ exp * 4, exp / 4 ]
                        -- to avoid rapid increase in difficulty
                        cal_span =
                            if actual_span > expect_span * 4 then
                                expect_span * 4
                            else if actual_span < expect_span `div` 4 then
                                expect_span `div` 4
                            else
                                actual_span
                    
                        new_target = old_target * cal_span `div` expect_span

                        real_target = unpackHash256 (packHash256 new_target)

                    return real_target

    (>= hash_target) <$> expect_target

    where
        shouldDiffChange conf height =
            height /= 0 &&
            height `mod` fi (tckr_diff_change_span conf) == 0

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
            setB bucket_chain height hash
    else
        return ()

-- try to fix a highest branch to save some memory
fixBranch :: Chain -> IO Chain
fixBranch chain@(Chain {
    std_conf = conf,
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

    if fi depth > tckr_max_tree_insert_depth conf then
        case mprev of
            -- only one node in the branch -> keep the original chain
            Nothing -> return chain

            -- remove loser branches and replace the buffer chain
            Just prev -> do
                -- write old buffer_chain to db_chain
                saved_height <-
                    case buffer_chain of
                        Nothing ->
                            return $ saved_height chain

                        Just bufc -> do
                            saveBranch chain bufc
                            return $ cur_height bufc + 1

                return $ chain {
                    saved_height = saved_height,
                    buffer_chain = Just prev,
                    edge_branches = [winner {
                        prev_node = Nothing
                    }]
                }

    else return chain

-- should not fail
collectOrphan :: Chain -> IO Chain
collectOrphan chain@(Chain {
    orphan_pool = orphan_pool
}) = do
    let orphan_list = OSET.toAscList orphan_pool
        fold_proc (suc, chain) block = do
            -- print $ "collect orphan " ++ show block
            mres <- addBlock chain block
            return $ case mres of
                Right new_chain -> (True, new_chain)
                Left _ -> (suc, chain) -- don't throw

    (suc, chain) <- foldM fold_proc (False, chain) orphan_list

    if suc then collectOrphan chain -- if success, try to collect the orphan again
    else return chain -- otherwise return the original chain

takeBranch' :: Int -> Maybe Branch -> [Branch]
takeBranch' n Nothing = []
takeBranch' 0 _ = []
takeBranch' n (Just branch@(BlockNode {
    prev_node = prev
})) = branch : takeBranch' (n - 1) prev

takeBranch n branch =
    if n >= 0 then takeBranch' n (Just branch)
    else []

-- latestBlocks chain max_number_of_block
-- take maxn from each branch
-- highest height first
latestBlocks :: Int -> Chain -> IO [Block]
latestBlocks maxn (Chain {
    edge_branches = branches
}) =
    return $
    map block_data $
    sortBy (\a b -> compare (cur_height b) (cur_height a))
           (concatMap (takeBranch maxn) branches)

addBlock :: Chain -> Block -> IO (Either TCKRError Chain)
addBlock chain block =
    force <$> ioToEitherIO (addBlockFail chain block)

addBlocks :: (Block -> Either TCKRError Chain -> IO ()) -> Chain -> [Block] -> IO Chain
addBlocks proc chain [] = return chain
addBlocks proc chain (block:blocks) = do
    res <- addBlock chain block
    res `seq` proc block res

    let new_chain = case res of
            Left _ -> chain
            Right chain -> chain
    
    new_chain `seq` addBlocks proc new_chain blocks

-- throws a TCKRError when rejecting the block
addBlockFail :: Chain -> Block -> IO Chain
addBlockFail chain@(Chain {
    std_conf = conf,
    tx_set = tx_set,
    bucket_block = bucket_block,
    orphan_pool = orphan_pool
}) block@(Block {
    block_hash = block_hash,
    hash_target = hash_target,
    timestamp = timestamp,
    merkle_root = merkle_root,
    txns = txns'
}) = do
    expectTrue "require full block" $
        isFullBlock block

    let txns = FD.toList txns'

    expectFalseIO "block already exists" $
        chain `hasBlockInChain` block

    -- don't check if the block is in orphan pool

    expectTrue "empty tx list" $
        not (null txns)

    expectTrue "hash target not met" $
        hash_target > block_hash

    cur_time <- unixTimestamp

    expectTrue "timestamp too large" $
        timestamp <= cur_time + tckr_max_block_future_diff conf

    expectTrue "first transaction is not coinbase" $
        isCoinbase (head txns)

    -- TODO:
    -- for each transaction, apply "tx" checks 2-4
    -- for the coinbase (first) transaction, scriptSig length must be 2-100
    -- reject if sum of transaction sig opcounts > MAX_BLOCK_SIGOPS

    expectTrue "merkle root claimed not correct" $
        merkleRoot block == merkle_root

    case insertToEdge chain block of
        Nothing -> do -- no previous hash found
            has_recv <- bucket_block `hasB` prev_hash block
        
            traceIO "orphan block!"

            -- if has_recv then
            --     -- have received the previous block
            --     -- but it's too deep to change
            --     reject "the previous block is too old to fetch"
            -- else
            --     -- put it into the orphan pool
            return $ chain {
                orphan_pool = orphan_pool OSET.|> block
            }

        Just (branch, chain) -> do
            -- block inserted, new branch leaf created

            expectTrueIO "wrong difficulty" $
                hashTargetValid chain branch

            -- TODO: reject if timestamp is the median time of the last 11 blocks or before(MTP?)
            -- TODO: further block checks

            {-
                1. input exists(the input points to a valid block and a valid index)
                2. if referenced transaction is a coinbase, make sure the depth of the input block is >= 100
                3. verify signature(involves script)
                4. referenced output is not spent
                5. validity of values involved(amount of input, amount of output, sum(inputs) >= sum(outputs))
            -}

            forM_ (zip [0..] txns) $ \(idx, tx) -> do
                let is_coinbase = isCoinbase tx

                expectTrue "more than one coinbase txns" $
                    idx == 0 || not is_coinbase

                if not is_coinbase then do
                    in_values <- forM (map prev_out (tx_in tx)) $ \outp@(OutPoint txid _) -> do
                        value <- expectMaybeIO ("outpoint not in utxo " ++ show outp) $
                            lookupUTXO tx_set outp

                        locator <- expectMaybeIO "failed to locate tx(db corrupt)" $
                            findTxId tx_set txid

                        mheight <- blockHeightByHash chain branch (locatorToHash locator)

                        height <- expectMaybeIO "cannot find corresponding block(db corrupt) or tx not in the current branch" $
                            blockHeightByHash chain branch (locatorToHash locator)

                        -- if the tx is coinbase(idx == 0), check coinbase maturity
                        expectTrue "coinbase maturity not met" $
                            locatorToIdx locator /= 0 ||
                            cur_height branch - height > fi (tckr_coinbase_maturity conf)

                        return value

                    -- validity of values
                    expectTrue "sum of inputs less than the sum of output" $
                        sum (in_values) >= getOutputValue tx
                else
                    -- omitting coinbase value check
                    return ()

                addTx tx_set block idx

            -- add tx to tx and utxo pool
            -- mapM_ (addTx tx_set block) [ 0 .. length txns - 1 ]
             
            -- all check passed
            -- write the block into the block database
            setB bucket_block block_hash (cur_height branch, block)

            is_orphan <- chain `hasBlockInOrhpan` block

            final_chain <-
                if is_orphan then
                    -- remove the block from orphan pool if accepted
                    return $ chain {
                        orphan_pool = OSET.delete block orphan_pool
                    }
                else
                    -- new block added, collect orphan
                    collectOrphan chain

            -- latestBlocks 3 chain >>= print

            fixBranch final_chain

-- received before && is in a branch
hasBlockInChain :: Chain -> Block -> IO Bool
hasBlockInChain chain@(Chain {
    bucket_block = bucket_block
}) block@(Block {
    block_hash = hash
}) = do
    mres <- getAsB bucket_block hash :: IO (Maybe (Height, Placeholder))
    
    case mres of
        Nothing -> return False
        Just (height, _) ->
            (not . null) <$> blocksAtHeight chain height

hasBlockInOrhpan :: Chain -> Block -> IO Bool
hasBlockInOrhpan chain block =
    return $ block `OSET.member` orphan_pool chain

-- search two places for the block: block db and the orphan pool
hasBlock :: Chain -> Block -> IO Bool
hasBlock chain block =
    (||) <$> hasBlockInChain chain block
         <*> hasBlockInOrhpan chain block

reject :: String -> a
reject msg = throw $ TCKRError msg

expect :: Eq a => String -> a -> IO a -> IO ()
expect msg exp mobs = do
    obs <- mobs
    
    if exp == obs then return ()
    else
        reject msg

expectTrueIO msg cond = expect msg True cond
expectFalseIO msg cond = expect msg False cond
expectTrue msg cond = expect msg True $ pure cond
expectFalse msg cond = expect msg False $ pure cond

expectMaybe msg (Just v) = return v
expectMaybe msg Nothing = reject msg

expectMaybeIO msg m = m >>= expectMaybe msg

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
