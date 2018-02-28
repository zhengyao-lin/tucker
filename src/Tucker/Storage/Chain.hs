{-# LANGUAGE DuplicateRecordFields #-}

-- block chain implementation

module Tucker.Storage.Chain where

import Data.Int -- hiding (map, findIndex, null)
import Data.Word
import qualified Data.Foldable as FD

import Control.Monad
import Control.Exception
import Control.Monad.Morph
import Control.Monad.Trans.Resource

import Debug.Trace

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Auth
import Tucker.Conf
import Tucker.Error
import Tucker.DeepSeq

import Tucker.Storage.Tx
import Tucker.Storage.Block

data BlockChain =
    BlockChain {
        bc_conf   :: TCKRConf,
        bc_dbs    :: [Database],

        bc_chain  :: Chain,
        bc_tx_set :: TxSet
    }

instance NFData BlockChain where
    rnf (BlockChain {
        bc_chain = bc_chain,
        bc_tx_set = bc_tx_set
    }) = rnf bc_chain `seq` rnf bc_tx_set

initBlockChain :: TCKRConf -> ResIO BlockChain
initBlockChain conf@(TCKRConf {
    tckr_block_db_path = block_db_path,
    tckr_tx_db_path = tx_db_path
}) = do
    block_db <- openDB def block_db_path
    tx_db <- openDB def tx_db_path

    bc_chain <- lift $ initChain conf block_db
    bc_tx_set <- lift $ initTxSet conf tx_db

    return $ BlockChain {
        bc_conf = conf,
        bc_dbs = [ block_db, tx_db ],

        bc_chain = bc_chain,
        bc_tx_set = bc_tx_set
    }

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

latestBlocks :: BlockChain -> Int -> IO [Block]
latestBlocks chain n = topNBlocks (bc_chain chain) n

corrupt = reject "corrupted database"

calNewTarget :: TCKRConf -> Hash256 -> Word32 -> Hash256
calNewTarget conf old_target actual_span =
    -- real target used
    unpackHash256 (packHash256 new_target)
    where
        expect_span = fi $ tckr_expect_diff_change_time conf

        -- new_span is in [ exp * 4, exp / 4 ]
        -- to avoid rapid increase in difficulty
        new_span =
            if actual_span > expect_span * 4 then
                expect_span * 4
            else if actual_span < expect_span `div` 4 then
                expect_span `div` 4
            else
                actual_span

        new_target = old_target * (fi new_span) `div` (fi expect_span)

shouldDiffChange :: TCKRConf -> Height -> Bool
shouldDiffChange conf height =
    height /= 0 &&
    height `mod` fi (tckr_diff_change_span conf) == 0

hashTargetValid :: BlockChain -> Branch -> IO Bool
hashTargetValid bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain
}) branch@(BlockNode {
    cur_height = height,
    block_data = Block {
        hash_target = hash_target
    }
}) = do
    -- previous block
    Just (Block {
        hash_target = old_target,
        btimestamp = t2
    }) <- blockAtHeight chain branch (height - 1)

    target <-
        if tckr_use_special_min_diff conf then
            -- TODO: non-standard special-min-diff
            return $ fi tucker_bdiff_diff1
        else if shouldDiffChange conf height then do
            Just (Block { btimestamp = t1 }) <-
                blockAtHeight chain branch (height - 2016)

            return $ calNewTarget conf old_target (t2 - t1)
        else
            return old_target

    return $ hash_target <= target

-- should not fail
collectOrphan :: BlockChain -> IO BlockChain
collectOrphan bc@(BlockChain {
    bc_chain = chain
}) = do
    let orphan_list = orphanList chain
        fold_proc (suc, bc) block = do
            -- print $ "collect orphan " ++ show block
            mres <- addBlock bc block
            return $ case mres of
                Right new_bc -> (True, new_bc)
                Left _ -> (suc, bc) -- don't throw

    (suc, bc) <- foldM fold_proc (False, bc) orphan_list

    if suc then collectOrphan bc -- if success, try to collect the orphan again
    else return bc -- otherwise return the original chain

addBlock :: BlockChain -> Block -> IO (Either TCKRError BlockChain)
addBlock bc block =
    force <$> ioToEitherIO (addBlockFail bc block)

addBlocks :: (Block -> Either TCKRError BlockChain -> IO ())
          -> BlockChain -> [Block]
          -> IO BlockChain
addBlocks proc bc [] = return bc
addBlocks proc bc (block:blocks) = do
    res <- addBlock bc block
    res `seq` proc block res

    let new_bc = case res of
            Left _ -> bc
            Right bc -> bc
    
    new_bc `seq` addBlocks proc new_bc blocks

-- test & save chain to the disk
trySyncChain :: BlockChain -> IO BlockChain
trySyncChain bc@(BlockChain {
    bc_chain = chain,
    bc_tx_set = tx_set
}) = do
    mres <- tryFixBranch chain

    case mres of
        Nothing -> return bc -- no change
        Just chain -> do
            -- blockchain flushed back to disk
            -- sync utxo
            syncUTXO tx_set
            return $ bc { bc_chain = chain }

genScriptConf :: TCKRConf -> Block -> IO ScriptConf
genScriptConf conf out_block =
    return $ def {
        script_enable_p2sh = btimestamp out_block >= tckr_p2sh_enable_time conf
    }

-- locatorToTx :: Chain -> TxLocator -> IO (Maybe TxPayload)
-- locatorToTx (Chain {
--     bucket_block = bucket_block
-- }) locator =
--     (liftM (txns . snd) <$> getB bucket_block hash) >>=
--         (return . (>>= (!!! idx)))
--     where
--         idx = fi (locatorToIdx locator)
--         hash = locatorToHash locator

-- throws a TCKRError when rejecting the block
addBlockFail :: BlockChain -> Block -> IO BlockChain
addBlockFail bc@(BlockChain {
    bc_conf = conf,
    bc_tx_set = tx_set,
    bc_chain = chain
}) block@(Block {
    block_hash = block_hash,
    hash_target = hash_target,
    btimestamp = timestamp,
    merkle_root = merkle_root,
    txns = txns'
}) = do
    expectTrue "require full block" $
        isFullBlock block

    let all_txns = FD.toList txns'

    expectFalseIO "block already exists" $
        chain `hasBlockInChain` block

    -- don't check if the block is in orphan pool

    expectTrue "empty tx list" $
        not (null all_txns)

    expectTrue "hash target not met" $
        hash_target > block_hash

    cur_time <- unixTimestamp

    expectTrue "timestamp too large" $
        timestamp <= cur_time + tckr_max_block_future_diff conf

    expectTrue "first transaction is not coinbase" $
        isCoinbase (head all_txns)

    -- TODO:
    -- for each transaction, apply "tx" checks 2-4
    -- for the coinbase (first) transaction, scriptSig length must be 2-100
    -- reject if sum of transaction sig opcounts > MAX_BLOCK_SIGOPS

    expectTrue "merkle root claimed not correct" $
        merkleRoot block == merkle_root

    case insertBlock chain block of
        Nothing -> do -- no previous hash found
            -- traceIO "orphan block!"
            error ("block orphaned " ++ show block)
            return $ bc { bc_chain = addOrphan chain block }

        Just (branch, chain) -> do
            -- block inserted, new branch leaf created

            -- update chain
            bc <- return $ bc { bc_chain = chain }

            expectTrueIO "wrong difficulty" $
                hashTargetValid bc branch

            -- TODO: reject if timestamp is the median time of the last 11 blocks or before(MTP?)
            -- TODO: further block checks

            {-
                1. input exists(the input points to a valid block and a valid index)
                2. if referenced transaction is a coinbase, make sure the depth of the input block is >= 100
                3. verify signature(involves script)
                4. referenced output is not spent
                5. validity of values involved(amount of input, amount of output, sum(inputs) >= sum(outputs))
            -}

            forM_ (zip [0..] all_txns) $ \(idx, tx) -> do
                let is_coinbase = isCoinbase tx

                expectTrue "more than one coinbase txns" $
                    idx == 0 || not is_coinbase

                traceM $ "checking tx " ++ show idx

                if not is_coinbase then do
                    in_values <- forM ([0..] `zip` tx_in tx) $ \(in_idx, inp@(TxInput {
                            prev_out = outp@(OutPoint txid out_idx)
                        })) -> do

                        value <- expectMaybeIO ("outpoint not in utxo " ++ show outp) $
                            lookupUTXO tx_set outp

                        locator <- expectMaybeIO "failed to locate tx(db corrupt)" $
                            findTxId tx_set txid

                        bnode <- expectMaybeIO "cannot find corresponding block(db corrupt) or tx not in the current branch" $
                            blockWithHash chain branch (locatorToHash locator)
                        
                        bnode <-
                            -- special case -> outpoint is in the current block
                            if bnode == branch then return bnode
                            else expectMaybeIO ("failed to load full block for " ++ show bnode) $
                                toFullBlockNode chain bnode

                        let prev_tx_idx = locatorToIdx locator
                            prev_tx_body =
                                (if bnode == branch then
                                     FullList all_txns
                                 else
                                     txns (block_data bnode)) `index` fi prev_tx_idx

                            prev_tx_out = tx_out prev_tx_body !! fi out_idx

                        -- if the tx is coinbase(idx == 0), check coinbase maturity
                        expectTrue ("coinbase maturity not met for outpoint tx " ++ show txid) $
                            prev_tx_idx /= 0 ||
                            cur_height branch - cur_height bnode >= fi (tckr_coinbase_maturity conf)

                        script_conf <- genScriptConf conf (block_data bnode)

                        let pk_sc = decodeFailLE (pk_script prev_tx_out)
                            sig_sc = decodeFailLE (sig_script inp)
                            state = initState script_conf prev_tx_body tx in_idx
                            check_res = runEval state [ sig_sc, pk_sc ]

                        -- traceShowM (out_idx, prev_tx_out)

                        expectTrue ("invalid script/signature for outpoint tx " ++
                                    show txid ++ ": " ++ show check_res ++
                                    ": " ++ show [ sig_sc, pk_sc ]) $
                            check_res == ValidTx

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
            saveBlock chain branch

            if chain `hasBlockInOrphan` block then
                return $ bc { bc_chain = removeOrphan chain block }
            else do
                -- not orphan, collect other orphan
                collectOrphan bc >>= trySyncChain

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
