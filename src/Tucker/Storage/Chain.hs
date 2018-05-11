{-# LANGUAGE DuplicateRecordFields, FlexibleContexts #-}

-- block chain implementation

module Tucker.Storage.Chain where

import Data.Int -- hiding (map, findIndex, null)
import Data.Word
import Data.Bits
import qualified Data.Foldable as FD
import qualified Data.ByteString as BSR

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Monad.Morph
import Control.Monad.Loops
import Control.Monad.Trans.Resource

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Auth
import Tucker.Conf
import Tucker.Error
import Tucker.Signal
import Tucker.Thread
import Tucker.DeepSeq

import Tucker.Container.IOMap

import Tucker.Storage.Tx
import Tucker.Storage.Block
import Tucker.Storage.SoftFork

data BlockChain =
    BlockChain {
        bc_conf         :: TCKRConf,
        bc_dbs          :: [Database],

        bc_thread_state :: Maybe ThreadState, -- thread-support is optional

        bc_chain        :: Chain,
        bc_tx_state     :: TxState UTXOCache1,
        bc_fork_state   :: SoftForkState
        -- use 1 layer of cache in UTXO
    }

data VerifyConf =
    VerifyConf {
        verify_enable_csv    :: Bool,
        verify_enable_segwit :: Bool,
        verify_cur_mtp       :: Timestamp,
        verify_cur_height    :: Height,
        verify_check_dup_tx  :: Bool
    }

instance NFData BlockChain where
    rnf (BlockChain {
        bc_chain = bc_chain,
        bc_tx_state = bc_tx_state
    }) = rnf bc_chain `seq` rnf bc_tx_state

initBlockChain :: TCKRConf -> Maybe ThreadState -> ResIO BlockChain
initBlockChain conf@(TCKRConf {
    tckr_block_db_path = block_db_path,
    tckr_tx_db_path = tx_db_path,
    tckr_block_db_max_file = block_db_max_file,
    tckr_tx_db_max_file = tx_db_max_file
}) m_thread_state = do
    block_db <- openDB (optMaxFile def block_db_max_file) block_db_path
    tx_db <- openDB (optMaxFile def tx_db_max_file) tx_db_path

    bc_chain <- lift $ initChain conf block_db
    bc_tx_state <- lift $ initTxState conf tx_db
    bc_fork_state <- lift $ initForkState conf block_db

    let tmp = BlockChain {
        bc_conf = conf,
        bc_dbs = [ block_db, tx_db ],

        bc_thread_state = m_thread_state,

        bc_chain = bc_chain,
        bc_tx_state = bc_tx_state,
        bc_fork_state = bc_fork_state
    }

    return tmp

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
6. the highest 3 bits must be 001


mem pool transaction check

not coinbase, regular input/output check
no duplication in mem pool and main branch
referenced output not in other tx in the pool

when searching for output: make sure to search the transaction pool as well
Add to transaction pool
Relay transaction to peers
For each orphan transaction that uses this one as one of its inputs, run all these steps (including this one) recursively on that orphan

-}

{-

notes for BIP9

0. four states: DEFINED, STARTED, LOCKED_IN, FAILED
1. uses bits in vers(32-bits) in total to represent deployment
2. only the lowest 29 bits(2 bits restricted by BIP34, 1 other bit is used for future extension) are used
3. blocks within the same retarget period have the same state of a deployment(per 2016 blocks)
4. if 1916 blocks of the previous 2016 blocks have the version bit,
   switch the state of the next retarget period to LOCKED_IN

-}

nextInitBlock :: BlockChain -> ByteString -> Address -> IO Block
nextInitBlock bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain
}) msg addr =
    let main = mainBranch chain
        next_height = cur_height main + 1
        fee = feeAtHeight conf next_height
        coinbase = stdCoinbase conf msg addr fee

    in appendTx coinbase <$> nextEmptyBlock bc

-- next empty block on the main branch
-- NOTE that this block does not contain a coinbase
nextEmptyBlock :: BlockChain -> IO Block
nextEmptyBlock bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain
}) = do
    let main = mainBranch chain
        next_height = cur_height main + 1

    [tip] <- topNBlockHashes chain main 1

    now <- unixTimestamp
    mtp <- medianTimePast bc main next_height

    target <- targetAtHeight bc main next_height now

    return $ updateBlockHashes $ Block {
        block_hash = undefined,
        vers = 0,
        prev_hash = tip,
        merkle_root = 0,
        btimestamp = max mtp now,
        hash_target =  target,
        nonce = 0,
        txns = FullList [],
        enc_cache = Nothing
    }

latestBlockHashes :: BlockChain -> Int -> IO [Hash256]
latestBlockHashes (BlockChain { bc_chain = chain }) n =
    fmap concat $
    forM (allBranches chain) $ \branch ->
        topNBlockHashes chain branch n

-- blocks used for synchronization
flagBlockHashes :: BlockChain -> Int -> IO [Hash256]
flagBlockHashes bc@(BlockChain { bc_chain = chain }) n = do
    latest <- latestBlockHashes bc n
    return (latest ++ [bottomMemBlockHash chain])

corrupt = reject "corrupted database"

calNewTarget :: TCKRConf -> Hash256 -> Timestamp -> Hash256
calNewTarget conf old_target actual_span =
    -- real target used
    unpackHash256 (packHash256 new_target)
    where
        expect_span = fi $ tckr_expect_retarget_time conf

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

-- medianTimePast :: BlockChain -> Branch -> Int -> IO Timestamp
-- medianTimePast bc@(BlockChain {
--     bc_chain = chain
-- }) branch n = do
--     ts <- map btimestamp <$> (topNBlocks chain branch n)

--     return $
--         if null ts then 0
--         else median ts

data BlockCondition
    = AtHeight Height
    | WithHash Hash256

-- block at a specific height in the main branch
-- returning a partial block with its height
lookupBlock :: BlockChain -> BlockCondition -> IO (Maybe (Height, Block))
lookupBlock (BlockChain {
    bc_chain = chain
}) cond = do
    let main = mainBranch chain
    
    res <- case cond of
        AtHeight height -> branchAtHeight chain main height
        WithHash hash -> branchWithHash chain main hash

    return ((\b -> (cur_height b, block_data b)) <$> res)

getFullBlock :: BlockChain -> Block -> IO (Maybe Block)
getFullBlock bc block =
    toFullBlock (bc_chain bc) block

-- mtp of a branch node
medianTimePastBranch bc branch = medianTimePast bc branch (cur_height branch)

-- median timestamp of n blocks before height
medianTimePast :: BlockChain -> Branch -> Height -> IO Timestamp
medianTimePast bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain
}) branch height = do
    let n = tckr_mtp_number conf

    mblocks <-
        mapM (blockAtHeight chain branch) [ height - 1, height - 2 .. height - fi n ]

    let ts = map btimestamp (maybeCat mblocks)

    return $ if null ts then 0
             else median ts

shouldRetarget :: TCKRConf -> Height -> Bool
shouldRetarget conf height =
    height /= 0 &&
    height `mod` fi (tckr_retarget_span conf) == 0

feeAtHeight :: TCKRConf -> Height -> Value
feeAtHeight conf height =
    fi (tckr_initial_fee conf) `shift`
    (fi height `div` fi (tckr_fee_half_rate conf))

updateForkDeploy :: BlockChain -> Branch -> IO ()
updateForkDeploy bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain,
    bc_fork_state = fork_state
}) branch =
    let block = block_data branch in

    if shouldRetarget conf (cur_height branch) then do
        tLnM ("retarget and update deploy status on block " ++ show block)

        -- update
        -- prev <- expectMaybeIO "no previous branch" (prevBlockNode chain branch)

        -- mtp of the parent block
        mtp <- medianTimePast bc branch (cur_height branch - 1)
        forks <- lookupNonFinalForks fork_state

        -- tLnM ("non-final forks " ++ forks)

        forM_ forks $ \fork -> do
            tLnM ("update status on fork " ++ show fork)

            case fork_status fork of
                FORK_STATUS_DEFINED ->
                    if mtp > fork_timeout fork then
                        changeForkStatus fork_state fork FORK_STATUS_FAILED
                    else if mtp > fork_start fork then
                        changeForkStatus fork_state fork FORK_STATUS_STARTED
                    else
                        tLnM "no status change"
                
                FORK_STATUS_STARTED ->
                    if mtp > fork_timeout fork then
                        changeForkStatus fork_state fork FORK_STATUS_FAILED
                    else do
                        -- pull out number of blocks supporting the fork
                        number <- getRecord fork_state fork
                        
                        if number >= fi (tckr_soft_fork_lock_threshold conf) then
                            changeForkStatus fork_state fork FORK_STATUS_LOCKED_IN
                        else
                            tLnM ("supporting blocks: " ++ show number ++ "/" ++
                                  show (tckr_retarget_span conf))

                FORK_STATUS_LOCKED_IN ->
                    changeForkStatus fork_state fork FORK_STATUS_ACTIVE

                _ -> return ()

        -- clear the stat for the current retarget period
        clearRecord fork_state
        recordBlock fork_state block
    else
        recordBlock fork_state block

{-

dep_bucket DeploymentId -> [Deployment]
           id 32        -> numbers of time appeared for each bit(0-28) in the current retarget period

at first, all deployment is defined
when a block is added,
check:
if height % 2016 == 0 and the block is in the main branch then
    for all deployment declared in version:
        case deployment status of
            defined:
                if MTP > timeout then
                    set stauts to failed
                else if MTP > start then
                    set status to start
                else
                    nothing

            started:
                if MTP > timeout then
                    set stauts to failed
                else
                    check 95% threshold
                    if reached then
                        set status to locked_in
                    else
                        nothing

            locked_in:
                set status to active

            _ -> nothing
-}

-- shouldUseSpecialDiff :: BlockChain -> IO Bool
-- shouldUseSpecialDiff (BlockChain {
--     bc_conf = conf,
--     bc_last_recv = last_recv
-- }) = do
--     now <- unixTimestamp
--     return $ tckr_use_special_min_diff conf &&
--              (last_recv == 0 || now - last_recv >= tckr_special_min_timeout conf)

-- correct target of a branch at a specific height
targetAtHeight :: BlockChain -> Branch -> Height -> Timestamp -> IO Hash256
targetAtHeight bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain
}) branch height check_time = do
    -- previous block
    Just (Block {
        hash_target = old_target,
        btimestamp = t2
    }) <- blockAtHeight chain branch (height - 1)

    -- NOTE: there is a bug here
    -- in a chain with special-min-diff rule,
    -- blocks with lower targets can get in
    -- after a max target block is added
    -- we need to look up further up the chain for regular difficulty

    if tckr_use_special_min_diff conf &&
        check_time - t2 >= tckr_special_min_timeout conf then
        -- special-min-diff(mainly for testnet)
        -- allow diff1 blocks when last_recv == 0 as well because
        -- we don't know the specific time of receiving the last block
        return $ fi (tckr_bdiff_diff1_target conf)
    else if shouldRetarget conf height then do
        Just (Block { btimestamp = t1 }) <-
            blockAtHeight chain branch (height - fi (tckr_retarget_span conf))

        return $ calNewTarget conf old_target (t2 - t1)
    else
        return old_target

hashTargetValid :: BlockChain -> Branch -> IO Bool
hashTargetValid bc branch@(BlockNode {
    cur_height = height,
    block_data = Block {
        hash_target = hash_target,
        btimestamp = time
    }
}) = (hash_target <=) <$> targetAtHeight bc branch height time

-- should not fail
collectOrphanBlock :: BlockChain -> Hash256 -> IO BlockChain
collectOrphanBlock bc@(BlockChain {
    bc_chain = chain
}) new_block = do
    let orphan_list =
            -- only add blocks with prev_hash == the newly added block
            -- to prevent infinite recursion
            filter ((== new_block) . prev_hash) (orphanList chain)

        fold_proc bc block = do
            -- print $ "collect orphan " ++ show block
            mres <- addBlock bc block
            return $ case mres of
                Right new_bc -> new_bc
                Left _ -> bc

    foldM fold_proc bc orphan_list

addBlock :: BlockChain -> Block -> IO (Either TCKRError BlockChain)
addBlock bc block =
    force <$> tryT (addBlockFail bc block)

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

syncBlockChain :: BlockChain -> IO ()
syncBlockChain bc = noStop $ do
    saveMainBranch (bc_chain bc)
    syncTxState (bc_tx_state bc)
    syncForkState (bc_fork_state bc)

-- push & fix a certain part of chain back to the database(from which no branching is possible)
tryFlushChain :: BlockChain -> IO BlockChain
tryFlushChain bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain,
    bc_tx_state = tx_state,
    bc_fork_state = fork_state
}) =
    if tckr_mem_only conf then return bc -- no flushing back
    else noStop $ do
        mres <- tryFixBranch chain

        case mres of
            Nothing -> return bc -- no change
            Just chain -> do
                -- blockchain flushed back to disk
                -- sync utxo
                syncTxState tx_state
                syncForkState fork_state
                
                return $ bc { bc_chain = chain }

genScriptConf :: BlockChain -> VerifyConf -> UTXOValue -> IO ScriptConf
genScriptConf bc ver_conf utxo_value =
    return $ def {
        script_enable_csv = verify_enable_csv ver_conf,
        script_enable_segwit = verify_enable_segwit ver_conf,
        script_enable_p2sh =
            parent_ts utxo_value >= tckr_p2sh_enable_time (bc_conf bc)
            -- NOTE: using the block timestamp here(not mtp)
    }

shouldEnableFork :: BlockChain -> String -> IO Bool
shouldEnableFork (BlockChain {
    bc_fork_state = fork_state
}) name =
    isActiveStatus <$> getForkStatus fork_state name

genVerifyConf :: BlockChain -> Branch -> Maybe Branch -> IO VerifyConf
genVerifyConf bc@(BlockChain {
    bc_conf = conf,
    bc_chain = bc_chain,
    bc_fork_state = fork_state
}) branch mnode = do
    csv <- shouldEnableFork bc "csv"
    segwit <- shouldEnableFork bc "segwit"

    let height = case mnode of
            Nothing -> mainBranchHeight bc
            Just node -> cur_height node

    mtp <- medianTimePast bc branch height

    return $ VerifyConf {
        verify_enable_csv = csv,
        verify_enable_segwit = segwit,
        verify_cur_mtp = mtp,
        verify_cur_height = height,
        verify_check_dup_tx = mtp >= tckr_dup_tx_disable_time conf
    }

parentBranchOfTx :: UTXOMap a => BlockChain -> TxState a -> Branch -> Hash256 -> IO (Branch, Word32)
parentBranchOfTx bc@(BlockChain {
    bc_chain = chain
}) tx_state branch txid = do
    locator <- expectMaybeIO "failed to locate tx(db corrupt)" $
        findTxId tx_state txid

    -- should ONLY be used if you checking coinbase maturity or csv validity
    bnode <- expectMaybeIO "cannot find corresponding block(db corrupt) or tx not in the current branch" $
        branchWithHash chain branch (locatorToHash locator)

    -- bnode <- expectMaybeIO ("failed to load full block for " ++ show bnode) $
    --     toFullBlockNode chain bnode

    return (bnode, locatorToIdx locator)

verifyScript :: BlockChain -> VerifyConf -> TxPayload -> Int -> UTXOValue -> IO ()
verifyScript bc ver_conf tx in_idx uvalue = do
    let prev_tx_idx = tx_index uvalue
        prev_tx_out = u_tx_out uvalue
        input = tx_in tx !! in_idx
        OutPoint prev_txid out_idx = prev_out input

    script_conf <- genScriptConf bc ver_conf uvalue

    let pk_sc = decodeFailLE (pk_script prev_tx_out)
        sig_sc = decodeFailLE (sig_script input)
        state = initState script_conf prev_tx_out tx (fi in_idx)
        check_res = runEval state [ sig_sc, pk_sc ]

    expectTrue (printf
        "script validation failed: tx %s, %s to tx %s, %s: %s, scripts: %s"
        (show prev_txid) (show out_idx) (show (txid tx)) (show in_idx) (show check_res)
        (show (sig_sc, pk_sc))) (check_res == ValidTx)

verifyRelLockTime :: BlockChain -> Branch -> VerifyConf -> TxInput -> UTXOValue -> IO ()
verifyRelLockTime bc branch ver_conf input uvalue =
    -- check BIP 68
    case inputRelLockTime input of
        Just rlock_time ->
            case rlock_time of
                RelLockTimeHeight v -> do
                    let cur = verify_cur_height ver_conf
                        exp = parent_height uvalue + fi v
                    
                    expectTrue ("relative lock-time(block height) not met(expecting " ++
                                show exp ++ ", current " ++ show cur) $
                        cur >= exp

                RelLockTime512S v -> do
                    prev_time <- medianTimePast bc branch (parent_height uvalue)

                    let cur = verify_cur_mtp ver_conf
                        exp = fi v * 512 + prev_time
                    
                    expectTrue ("relative lock-time(512s) not met(expecting " ++
                                show exp ++ ", current " ++ show cur) $
                        cur >= exp

        Nothing -> return () -- no requirement for lock time

verifyInput :: UTXOMap a
            => BlockChain -> TxState a -> VerifyConf
            -> Branch -> TxPayload -> Int -> IO Value
verifyInput bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain,
    bc_fork_state = fork_state
}) tx_state ver_conf branch tx in_idx = do
    let input@(TxInput {
            prev_out = outp@(OutPoint prev_txid out_idx)
        }) = tx_in tx !! in_idx

    -- :: UTXOValue
    uvalue <- expectMaybeIO ("outpoint not in utxo " ++ show outp) $
        lookupUTXO tx_state outp

    when (verify_enable_csv ver_conf && version tx >= 2) $
        verifyRelLockTime bc branch ver_conf input uvalue

    -- if the funding tx is coinbase(idx == 0), check coinbase maturity
    expectTrue ("coinbase maturity not met for outpoint tx " ++ show prev_txid) $
        tx_index uvalue /= 0 ||
        cur_height branch - parent_height uvalue >= fi (tckr_coinbase_maturity conf)

    verifyScript bc ver_conf tx in_idx uvalue

    -- all test passed, return value
    return (value (u_tx_out uvalue))

updateChain :: BlockChain -> Chain -> IO BlockChain
updateChain bc chain =
    return $ bc {
        bc_chain = chain
    }

hasTxInBranch :: BlockChain -> Branch -> Hash256 -> IO Bool
hasTxInBranch bc branch txid = do
    mloc <- findTxId (bc_tx_state bc) txid

    case mloc of
        Nothing -> return False
        Just loc ->
            hasBlockInBranch (bc_chain bc) branch (locatorToHash loc)

    -- findTxId -> check if the block is in the main branch

hasTx :: BlockChain -> Hash256 -> IO Bool
hasTx bc txid =
    let tx_state = bc_tx_state bc in
    anyM (\f -> f txid) [
        hasTxInMemPool tx_state,
        hasTxInOrphanPool tx_state,
        hasTxInBranch bc (mainBranch (bc_chain bc))
    ]

-- NOTE: the block given is not necessarily the top block
verifyBlockTx :: BlockChain -> Branch -> Block -> IO ()
verifyBlockTx bc branch block = do
    let all_txns = FD.toList (txns block)
        conf = bc_conf bc

    bnode <- expectMaybeIO "the block being verified is not in the given branch" $
        branchWithHash (bc_chain bc) branch (block_hash block)

    begin_time <- msMonoTime
    ver_conf <- genVerifyConf bc branch (Just bnode)

    let mtp = verify_cur_mtp ver_conf
        block_timestamp =
            if verify_enable_csv ver_conf then mtp
            else btimestamp block

    -- using cached utxo because any error
    -- in the tx will cause the tx state to roll back
    withCacheUTXO (bc_tx_state bc) $ \tx_state -> do
        let coinbase = head all_txns
            normal_tx = tail all_txns
            ntxns = length all_txns

        -- check lock-time
        forM_ all_txns $ \tx ->
            unless (isFinalTx tx) $
                case txLockTime tx of
                    LockTimeStamp min_stamp ->
                        expectTrue ("tx lock-time(timestamp) not met(expect " ++ show min_stamp ++
                                    ", " ++ show mtp ++ " given") $
                            block_timestamp >= min_stamp

                    LockTimeHeight min_height ->
                        expectTrue "tx lock-time(height) not met" $
                            cur_height bnode >= min_height
            -- else lock-time is irrelevant

        when (verify_check_dup_tx ver_conf) $
            forM_ (zip [0..] all_txns) $ \(idx, tx) -> do
                -- check duplicated tx
                mlocator <- findTxId tx_state (txid tx)
                
                case mlocator of
                    Nothing -> return () -- no record
                    Just locator ->
                        expectTrue ("duplicated transaction " ++ show (txid tx)) $
                            locatorToHash locator == block_hash block &&
                            locatorToIdx locator == idx

        all_fees <- forM (zip [1..] normal_tx) $ \(idx, tx) -> do
            expectTrue "more than one coinbase txns" $
                not (isCoinbase tx)

            -- tLnM $ "checking tx " ++ show idx

            let len = fi (length (tx_in tx))
                idxs = [ 0 .. len - 1 ]
                job = tckr_job_number (bc_conf bc)
                sep_idxs = foldList job idxs

                verify in_idx = do
                    tM $ wss (Color Blue False)
                       $ "[tx " ++ show idx ++ "/" ++ show ntxns ++ " " ++ take 8 (show (txid tx)) ++ "..., " ++
                         show in_idx ++ "/" ++ show len ++ "]"

                    verifyInput bc tx_state ver_conf branch tx in_idx

                parVerify tstate = do
                    -- tLnM "checking input in parallel"

                    res <- forkMap tstate THREAD_VALIDATION (mapM verify) sep_idxs
            
                    return (sum (concat res))

                seqVerify = foldM (\val idx -> (val +) <$> verify idx) 0 idxs

            in_value <-
                if length idxs >= tckr_min_parallel_input_check conf then do
                    case bc_thread_state bc of
                        Just tstate -> parVerify tstate
                        Nothing -> seqVerify -- threading is not supported
                else
                    seqVerify

            let total_out_value = getOutputValue tx
                fee = in_value - total_out_value
            
            -- validity of values
            expectTrue "sum of inputs less than the sum of output" $
                fee >= 0

            addTx tx_state (cur_height bnode) block idx

            return fee

        -- checking coinbase

        let block_fee = feeAtHeight conf (cur_height branch)
            tx_fee = sum all_fees
            coinbase_out = getOutputValue coinbase

        expectTrue "coinbase output greater than the sum of the block creation fee and tx fees" $
            coinbase_out <= block_fee + tx_fee

        addTx tx_state (cur_height bnode) block 0

    end_time <- msMonoTime :: IO Integer

    tLnM ("verified " ++ show (length all_txns) ++
          " txns in " ++ show (end_time - begin_time) ++ "ms")

mainBranchHeight :: BlockChain -> Height
mainBranchHeight (BlockChain {
    bc_chain = chain
}) =
    branchHeight (mainBranch chain)

-- return (mem pool size, orphan pool size)
txPoolStatus :: BlockChain -> IO (Int, Int)
txPoolStatus bc =
    (,) <$> memPoolSize (bc_tx_state bc)
        <*> orphanPoolSize (bc_tx_state bc)

-- get mem pool txns
allMemPoolTxns :: BlockChain -> IO [TxPayload]
allMemPoolTxns bc = memPoolTxns (bc_tx_state bc)

lookupMemPool :: BlockChain -> Hash256 -> IO (Maybe TxPayload)
lookupMemPool bc txid = lookupMemPoolTx (bc_tx_state bc) txid

-- revert the change of a block on UTXO
-- assuming the block is a full block
revertBlockOnUTXO :: UTXOMap a => BlockChain -> TxState a -> Branch -> Block -> IO ()
revertBlockOnUTXO bc tx_state branch block = do
    let all_txns = FD.toList (txns block)

    -- remove all added outputs
    mapM_ (removeTx tx_state block) [ 0 .. length all_txns - 1 ]

    -- coinbase doesn't have any input
    forM_ ([0..] `zip` tail all_txns) $ \(i, tx) -> do
        -- re-add all outputs cancelled
        -- tM ("reverting tx " ++ show i)

        forM_ (tx_in tx) $ \input@(TxInput {
            prev_out = OutPoint prev_txid out_idx
        }) -> do
            (prev_bnode, tx_idx) <-
                parentBranchOfTx bc tx_state branch prev_txid

            prev_bnode <- toFullBlockNodeFail (bc_chain bc) prev_bnode

            addOutput tx_state (cur_height prev_bnode)
                      (block_data prev_bnode) (fi tx_idx) (fi out_idx)

hasBlock :: BlockChain -> Hash256 -> IO Bool
hasBlock (BlockChain { bc_chain = chain }) hash =
    if not (hasBlockInOrphan chain hash) then
        hasBlockInChain chain hash
    else
        return True

-- throws a TCKRError when rejecting the block
addBlockFail :: BlockChain -> Block -> IO BlockChain
addBlockFail bc@(BlockChain {
    bc_conf = conf,
    bc_tx_state = tx_state,
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
        chain `hasBlockInChain` block_hash

    -- don't check if the block is in orphan pool

    expectTrue "block weight limit passed" $
        blockWeight block <= tckr_block_weight_limit conf

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
            expectFalse "repeated orphan block" $
                chain `hasBlockInOrphan` block_hash

            tLnM "block orphaned"
            updateChain bc (addOrphan chain block)

        Just (branch, chain) -> do
            -- block inserted, new branch leaf created

            -- update chain
            bc <- updateChain bc chain

            -- tLnM (isPartial (txns (block_data branch)))

            -- save the block data first
            -- all actions below will find the block
            -- in the chain as other normal blocks
            saveBlock chain (cur_height branch) block

            when (tckr_enable_mtp_check conf) $
                expectTrueIO "MTP rule not met" $
                    (btimestamp block >=) <$> medianTimePastBranch bc branch

            when (tckr_enable_difficulty_check conf) $
                expectTrueIO "wrong difficulty" $
                    hashTargetValid bc branch

            let main_branch = mainBranch chain

            bc <-
                if main_branch == branch then do
                    -- tLnM "adding to the main branch"
                    -- adding to the main branch
                    
                    let skip_tx_check = Just True == do
                            (height, _) <- tckr_block_assumed_valid conf
                            return (cur_height branch < fi height)

                    if not skip_tx_check then
                        verifyBlockTx bc branch block
                    else do
                        -- trust all txns
                        tLnM "txns assumed valid"
                        addTxns tx_state (cur_height branch) block

                    -- update fork deploy status
                    updateForkDeploy bc branch

                    return bc

                else if branch > main_branch then do
                    -- branch becoming the main branch
                    
                    tLnM "!!! main branch change"

                    -- set main branch
                    bc <- updateChain bc (setMainBranch chain branch)

                    -- if block check fails here, the new block will be rejected
                    -- and no update in main branch is possible
                    withCacheUTXO tx_state $ \tx_state -> do
                        let (fp1, fp2) = forkPath chain main_branch branch

                        -- [[TxPayload]]
                        -- fp1 is in descending order of heights
                        -- fp1 is from the original main branch
                        forM_ fp1 $ \bnode -> do
                            bnode <- toFullBlockNodeFail chain bnode
                            revertBlockOnUTXO bc tx_state main_branch (block_data bnode)
        
                        -- recheck all new main branch nodes
                        
                        -- fp2 is in descending order of heights
                        forM_ (reverse fp2) $ \bnode -> do
                            bnode <- toFullBlockNodeFail chain bnode
                            -- verify all blocks in the new main branch
                            verifyBlockTx bc branch (block_data bnode)

                    return bc
                else
                    -- still not main branch do nothing
                    return bc

            bc <- updateChain bc (removeOrphan chain block)

            -- remove accepted txns from mem pool
            forM_ (tail all_txns) $ \tx -> do
                removePoolTx tx_state (txid tx)

            bc <- collectOrphanBlock bc block_hash
            
            tryFlushChain bc

addPoolTx :: BlockChain -> TxPayload -> IO (Maybe TCKRError)
addPoolTx bc tx =
    either Just (const Nothing) <$>
    force <$> tryT (addPoolTxFail bc tx)

-- verify tx in the pool and either reject it or put it into the mem pool or orphan pool
addPoolTxFail :: BlockChain -> TxPayload -> IO ()
addPoolTxFail bc@(BlockChain {
    bc_conf = conf,
    bc_tx_state = tx_state
}) tx = do
    -- (no check for locktime)
    let main = mainBranch (bc_chain bc)

    ntx <- txPoolSize tx_state
    now <- unixTimestamp

    when (ntx >= tckr_pool_tx_limit conf) $ do
        timeoutPoolTx tx_state (now - tckr_pool_tx_timeout conf)
        fin <- txPoolSize tx_state

        tLnM ("reducing mem pool/orphan pool to a total size of " ++ show fin)

    is_orphan <- hasTxInOrphanPool tx_state (txid tx)
    
    when is_orphan $
        removeOrphanTx tx_state (txid tx)

    -- check for duplication(in chain, mem pool, and orphan pool)
    expectFalseIO "duplicated tx in chain" $
        hasTxInBranch bc main (txid tx)

    expectFalseIO "duplicated tx in mem pool" $
        hasTxInMemPool tx_state (txid tx)

    ver_conf <- genVerifyConf bc main Nothing

    values <- forM ([0..] `zip` tx_in tx) $
        \(in_idx, TxInput { prev_out = prev_out }) -> do
            muvalue <- lookupUTXOMemPool tx_state prev_out

            case muvalue of
                Nothing -> do
                    addOrphanTx tx_state tx
                    -- reject "orphaned tx due to the lack of outpoint in utxo/mem pool"

                    return maxBound

                Just uvalue -> do
                    -- TODO: add coinbase maturity check?

                    -- check script
                    verifyScript bc ver_conf tx in_idx uvalue

                    -- return input value
                    return (value (u_tx_out uvalue))

    unless (any (== maxBound) values) $ do
        let total_out_value = getOutputValue tx
            fee = sum values - total_out_value
        
        expectTrue "sum of inputs less than the sum of output" $
            fee >= 0

        addMemPoolTx tx_state tx

        -- retry on any orphan txns with input pointing to the current tx
        dep_orphan_txns <-
            filterOrphanPoolTx tx_state $ \otx ->
                flip any (tx_in otx) $ \(TxInput { prev_out = OutPoint prev_txid _ }) ->
                    prev_txid == txid tx

        -- there is unlikely infinite recursion because
        -- we assume txns don't have cyclic dependence
        mapM_ (addPoolTx bc) dep_orphan_txns

reject :: String -> a
reject msg = throw $ TCKRError msg

expect :: Eq a => String -> a -> IO a -> IO ()
expect msg exp mobs = do
    obs <- mobs
    unless (exp == obs) (reject msg)

expectTrueIO msg cond = expect msg True cond
expectFalseIO msg cond = expect msg False cond
expectTrue msg cond = expect msg True $ pure cond
expectFalse msg cond = expect msg False $ pure cond

expectMaybe msg (Just v) = return v
expectMaybe msg Nothing = reject msg

expectMaybeIO msg m = m >>= expectMaybe msg
