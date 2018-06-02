{-# LANGUAGE DuplicateRecordFields, FlexibleContexts #-}

-- block chain implementation

module Tucker.State.Chain where

import Data.Int -- hiding (map, findIndex, null)
import Data.Word
import Data.Bits
import qualified Data.Foldable as FD
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

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
import qualified Tucker.Container.Set as SET

import Tucker.State.Tx
import Tucker.State.Block
import Tucker.State.SoftFork

import Tucker.Wallet.Wallet

data BlockChain =
    BlockChain {
        bc_conf         :: TCKRConf,
        bc_dbs          :: [Database],

        bc_thread_state :: Maybe ThreadState, -- thread-support is optional

        bc_chain        :: Chain,
        bc_tx_state     :: TxState UTXOCache1, -- use 1 layer of cache in UTXO
        bc_fork_state   :: SoftForkState,

        bc_wallet       :: Maybe Wallet
    }

data VerifyConf =
    VerifyConf {
        verify_enable_csv    :: Bool,
        verify_enable_segwit :: Bool,
        verify_cur_time      :: Timestamp,
        verify_cur_height    :: Height,
        verify_cur_branch    :: Branch,
        verify_block_hash    :: Maybe Hash256,
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
        bc_fork_state = bc_fork_state,

        bc_wallet = Nothing
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

corrupt = error "corrupted database"

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

-- block at a specific height in the main branch
-- returning a partial block with its height
lookupMainBranchBlock :: BlockChain -> BlockSearchType -> IO (Maybe (Height, Block))
lookupMainBranchBlock (BlockChain {
    bc_chain = chain
}) cond =
    (\b -> (cur_height b, block_data b)) <$$>
    lookupBlockNode chain [mainBranch chain] cond

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
        mapM (lookupBlock chain [branch] . AtHeight)
        [ height - 1, height - 2 .. height - fi n ]

    let ts = map btimestamp (maybeCat mblocks)

    return $ if null ts then 0
             else median ts

shouldRetarget :: TCKRConf -> Height -> Bool
shouldRetarget conf height =
    height /= 0 &&
    height `mod` fi (tckr_retarget_span conf) == 0

feeAtHeight :: TCKRConf -> Height -> Satoshi
feeAtHeight conf height =
    fi (tckr_initial_fee conf) `shiftR`
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

-- correct target of a branch at a specific height
targetAtHeight :: BlockChain -> Branch -> Height -> Timestamp -> IO Hash256
targetAtHeight bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain
}) branch height check_time = do
    let bh1 = height - 1
        bh2 = height - fi (tckr_retarget_span conf)

    -- previous block
    Just (Block {
        hash_target = old_target,
        btimestamp = t2
    }) <- lookupBlock chain [branch] (AtHeight bh1)

    -- NOTE: there is a bug here
    -- in a chain with special-min-diff rule,
    -- blocks with lower targets can get in
    -- after a max target block is added
    -- we need to look up further up the chain for regular difficulty

    if tckr_use_special_min_diff conf &&
        check_time - t2 > 2 * tckr_target_spacing conf then
        -- special-min-diff(mainly for testnet)
        -- allow diff1 blocks when last_recv == 0 as well because
        -- we don't know the specific time of receiving the last block
        return $ fi (tckr_bdiff_diff1_target conf)
    else if shouldRetarget conf height then do
        Just (Block { btimestamp = t1 }) <-
            lookupBlock chain [branch] (AtHeight bh2)

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

addBlock :: BlockChain -> Block -> IO (Either Rejection BlockChain)
addBlock bc block =
    force <$> try (addBlockFail bc block)

addBlocks :: (Block -> Either Rejection BlockChain -> IO ())
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
    let conf = bc_conf bc
        height = verify_cur_height ver_conf
    in return def {
        script_enable_csv = verify_enable_csv ver_conf,
        script_enable_cltv = fi height >= tckr_bip65_height conf,
        script_enable_segwit = verify_enable_segwit ver_conf,
        script_enable_strict_sig = fi height >= tckr_bip66_height conf,
        script_enable_p2sh =
            parent_ts utxo_value >= tckr_p2sh_enable_time (bc_conf bc)
            -- NOTE: using the block timestamp here(not mtp)
    }

shouldEnableFork :: BlockChain -> String -> IO Bool
shouldEnableFork (BlockChain {
    bc_fork_state = fork_state
}) name =
    isActiveStatus <$> getForkStatus fork_state name

genVerifyConf :: BlockChain -> Branch -> Height -> Maybe Block -> IO VerifyConf
genVerifyConf bc@(BlockChain {
    bc_conf = conf,
    bc_chain = bc_chain,
    bc_fork_state = fork_state
}) branch height mblock = do
    csv <- shouldEnableFork bc "csv"
    segwit <- shouldEnableFork bc "segwit"

    -- let height =
    --     case mnode of
    --         Nothing -> mainBranchHeight bc
    --         Just node -> cur_height node

    mtp <- medianTimePast bc branch height
    now <- unixTimestamp

    return $ VerifyConf {
        verify_enable_csv = csv,
        verify_enable_segwit = segwit,
        verify_cur_height = height,
        verify_cur_time =
            if csv then mtp -- use mtp
            else case mblock of
                Just block -> btimestamp block -- use block timestamp before activation of csv
                Nothing -> now, -- use cur time

        verify_cur_branch = branch,

        verify_block_hash = block_hash <$> mblock,
        verify_check_dup_tx = mtp >= tckr_dup_tx_disable_time conf
    }

parentBranchOfTx :: UTXOMap a => BlockChain -> TxState a -> Branch -> Hash256 -> IO (Branch, Word32)
parentBranchOfTx bc@(BlockChain {
    bc_chain = chain
}) tx_state branch txid = do
    locator <- expectMaybeIO
        REJECT_INVALID
        "failed to locate tx(db corrupt)" $
        findTxId tx_state txid

    -- should ONLY be used if you checking coinbase maturity or csv validity
    bnode <- expectMaybeIO
        REJECT_INVALID
        "cannot find corresponding block(db corrupt) or tx not in the current branch" $
        lookupBlockNode chain [branch] (WithHash (locatorToHash locator))

    -- bnode <- expectMaybeIO ("failed to load full block for " ++ show bnode) $
    --     toFullBlockNode chain bnode

    return (bnode, locatorToIdx locator)

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


verifyScript :: BlockChain -> VerifyConf -> TxPayload -> Int -> UTXOValue -> IO ()
verifyScript bc ver_conf tx in_idx uvalue = do
    let prev_tx_idx = tx_index uvalue
        prev_tx_out = u_tx_out uvalue
        input = tx_in tx !! in_idx
        OutPoint prev_txid out_idx = prev_out input

    script_conf <- genScriptConf bc ver_conf uvalue

    pk_sc <- expectEither REJECT_MALFORMED "public key script decode error" $
        decodeAllLE (pk_script prev_tx_out)

    sig_sc <- expectEither REJECT_MALFORMED "signature script decode error" $
        decodeAllLE (sig_script input)

    let state = initState script_conf prev_tx_out tx (fi in_idx)
        check_res = runEval state [ sig_sc, pk_sc ]

    expectTrue
        REJECT_INVALID
        (printf "script validation failed: tx %s, %s to tx %s, %s: %s, scripts: %s"
         (show prev_txid) (show out_idx) (show (txid tx)) (show in_idx) (show check_res)
         (show (sig_sc, pk_sc)))
        (check_res == ValidTx)

verifyRelLockTime :: BlockChain -> VerifyConf -> TxInput -> UTXOValue -> IO ()
verifyRelLockTime bc ver_conf input uvalue =
    -- check BIP 68
    case inputRelLockTime input of
        Just rlock_time ->
            case rlock_time of
                RelLockTimeHeight v -> do
                    let cur = verify_cur_height ver_conf
                        exp = parent_height uvalue + fi v
                    
                    expectTrue
                        REJECT_INVALID
                        ("relative lock-time(block height) not met(expecting " ++
                         show exp ++ ", current " ++ show cur) $
                        cur >= exp

                RelLockTime512S v -> do
                    prev_time <-
                        medianTimePast bc (verify_cur_branch ver_conf)
                                       (parent_height uvalue)

                    let cur = verify_cur_time ver_conf
                        exp = fi v * 512 + prev_time
                    
                    expectTrue
                        REJECT_INVALID
                        ("relative lock-time(512s) not met(expecting " ++
                         show exp ++ ", current " ++ show cur) $
                        cur >= exp

        Nothing -> return () -- no requirement for lock time

verifyInput :: UTXOMap a
            => BlockChain -> TxState a -> VerifyConf
            -> TxPayload -> Int -> IO Satoshi
verifyInput bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain,
    bc_fork_state = fork_state
}) tx_state ver_conf tx in_idx = do
    let input@(TxInput {
            prev_out = outp@(OutPoint prev_txid out_idx)
        }) = tx_in tx !! in_idx

    -- :: UTXOValue
    uvalue <- expectMaybeIO
        REJECT_INVALID
        ("outpoint not in utxo " ++ show outp) $
        lookupUTXO tx_state outp

    when (verify_enable_csv ver_conf && version tx >= 2) $
        verifyRelLockTime bc ver_conf input uvalue

    -- if the funding tx is coinbase(idx == 0), check coinbase maturity
    expectTrue
        REJECT_INVALID
        ("coinbase maturity not met for outpoint tx " ++ show prev_txid) $
        tx_index uvalue /= 0 ||
        verify_cur_height ver_conf - parent_height uvalue >= fi (tckr_coinbase_maturity conf)

    verifyScript bc ver_conf tx in_idx uvalue

    -- all test passed, return value
    return (value (u_tx_out uvalue))

-- verify (absolute)lock time
verifyLockTime :: BlockChain -> VerifyConf -> TxPayload -> IO ()
verifyLockTime bc ver_conf tx =
    let time = verify_cur_time ver_conf
        height = verify_cur_height ver_conf
    in unless (isFinalTx tx) $
        case txLockTime tx of
            LockTimeStamp min_stamp ->
                expectTrue
                    REJECT_INVALID 
                    ("tx lock-time(timestamp) not met(expect " ++ show min_stamp ++
                        ", " ++ show time ++ " given") $
                    time >= min_stamp

            LockTimeHeight min_height ->
                expectTrue
                    REJECT_INVALID
                    "tx lock-time(height) not met" $
                    height >= min_height

-- check for duplicated txns
verifyDupTx :: BlockChain -> VerifyConf -> TxPayload -> IO ()
verifyDupTx bc ver_conf tx = do
    mlocator <- findTxId (bc_tx_state bc) (txid tx)
                
    case mlocator of
        Nothing -> return () -- no record
        Just locator -> do
            let hash = locatorToHash locator
                match_record =
                    case verify_block_hash ver_conf of
                        Just block_hash -> hash == block_hash
                        Nothing -> False

            -- it's a duplicated tx unless it was included in the same block
            -- OR the recorded block doesn't exist in the current branch
            unless match_record $
                expectFalseIO
                    REJECT_DUPLICATE
                    ("duplicated transaction " ++ show (txid tx)) $
                    hasBlockInBranch (bc_chain bc) (verify_cur_branch ver_conf) hash

-- basic checks for tx
verifyBasicTx :: BlockChain -> TxPayload -> IO ()
verifyBasicTx bc tx = do
    expectTrue REJECT_INVALID "more than one coinbase txns" $
        not (isCoinbase tx)

    expectFalse REJECT_INVALID "empty input list" $
        null (tx_in tx)

    expectFalse REJECT_INVALID "empty output list" $
        null (tx_out tx)    

-- verify bitcoin flow
-- NOTE: this function cannot be used to verify coinbases
-- returns tx fee
verifyFlow :: UTXOMap a
            => BlockChain -> TxState a
            -> VerifyConf -> TxPayload -> IO Satoshi
verifyFlow bc tx_state ver_conf tx = do
    let len = fi (length (tx_in tx))
        idxs = [ 0 .. len - 1 ]
        job = tckr_job_number (bc_conf bc)
        sep_idxs = foldList job idxs

        verify in_idx =
            verifyInput bc tx_state ver_conf tx in_idx

        parVerify tstate = do
            -- tLnM "checking input in parallel"

            res <- forkMap tstate THREAD_VALIDATION (mapM verify) sep_idxs
    
            return (sum (concat res))

        seqVerify = foldM (\val idx -> (val +) <$> verify idx) 0 idxs

    in_value <-
        if length idxs >= tckr_min_parallel_input_check (bc_conf bc) then do
            case bc_thread_state bc of
                Just tstate -> parVerify tstate
                Nothing -> seqVerify -- threading is not supported
        else
            seqVerify

    let total_out_value = getOutputValue tx
        fee = in_value - total_out_value

    forM_ ([0..] `zip` tx_out tx) $ \(out_idx, out) ->
        when (tckr_reject_non_std_tx (bc_conf bc)) $ do
            pk_script <- expectEither REJECT_MALFORMED "public key script decode error" $
                decodeAllLE (pk_script out)

            expectTrue
                REJECT_NONSTANDARD
                ("non-standard public key script for tx " ++ show (txid tx) ++ " " ++ show out_idx) $
                getScriptType pk_script /= SCRIPT_NONSTD
    
    -- validity of values
    expectTrue
        REJECT_INVALID
        "sum of inputs less than the sum of output" $
        fee >= 0

    return fee

-- verify all txns in a block
-- NOTE: the block given is not necessarily the top block
verifyBlockTxns :: BlockChain -> Branch -> Height -> Block -> IO ()
verifyBlockTxns bc branch height block = do
    let all_txns = FD.toList (txns block)
        conf = bc_conf bc
        
    begin_time <- msMonoTime
    ver_conf <- genVerifyConf bc branch height (Just block)

    has_wit <-
        if verify_enable_segwit ver_conf then
            case getWitnessCommitment conf block of
                Nothing -> return False
                Just (com_hash, wit_resv) -> do
                    let merk = hash256ToBS (witnessMerkleRoot block)

                    expectTrue REJECT_INVALID "witness merkle hash not match" $
                        doubleSHA256 (merk <> wit_resv) == hash256ToBS com_hash
                    
                    return True
        else
            return False

    unless has_wit $
        -- reject blocks having unexpected witness data
        expectFalse REJECT_INVALID "unexpected witness" $
            any hasWitness all_txns

    -- using cached utxo because any error
    -- in the tx will cause the tx state to roll back
    withCacheUTXO (bc_tx_state bc) $ \tx_state -> do
        let coinbase = head all_txns
            normal_txns = tail all_txns
            ntxns = length all_txns

        -- check lock-time
        forM_ all_txns (verifyLockTime bc ver_conf)

        -- check for duplicated tx if enabled
        when (verify_check_dup_tx ver_conf) $
            forM_ all_txns (verifyDupTx bc ver_conf)

        -- normal tx check
        all_fees <- forM (zip [1..] normal_txns) $ \(idx, tx) -> do
            tM $ wss (Color Blue False)
               $ "[tx " ++ show idx ++ "/" ++ show ntxns ++ " " ++ take 8 (show (txid tx)) ++ "...]"

            verifyBasicTx bc tx
            fee <- verifyFlow bc tx_state ver_conf tx
            addTx tx_state height block idx
            return fee

        -- checking coinbase

        let block_fee = feeAtHeight conf (cur_height branch)
            tx_fee = sum all_fees
            coinbase_out = getOutputValue coinbase

        expectTrue
            REJECT_INVALID
            "coinbase output greater than the sum of the block creation fee and tx fees" $
            coinbase_out <= block_fee + tx_fee

        addTx tx_state height block 0

    end_time <- msMonoTime :: IO Integer

    tLnM ("verified " ++ show (length all_txns) ++
          " txns in " ++ show (end_time - begin_time) ++ "ms")

mainBranchHeight :: BlockChain -> Height
mainBranchHeight (BlockChain {
    bc_chain = chain
}) =
    branchHeight (mainBranch chain)

mainBranchTip :: BlockChain -> Block
mainBranchTip bc =
    let chain = bc_chain bc in
    block_data (mainBranch chain)

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

withWallet :: BlockChain -> (Wallet -> IO a) -> IO ()
withWallet bc proc = maybe (return ()) (void . proc) (bc_wallet bc)

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

        when (i /= 0) $
            withWallet bc $ \wallet ->
                unregisterTx wallet tx

        forM_ (tx_in tx) $ \input@(TxInput {
            prev_out = outpoint@(OutPoint prev_txid out_idx)
        }) -> do
            (prev_bnode, tx_idx) <-
                parentBranchOfTx bc tx_state branch prev_txid

            prev_block <- block_data <$> toFullBlockNodeFail (bc_chain bc) prev_bnode
            let prev_tx = FD.toList (txns prev_block) !! fi tx_idx

            addOutput tx_state (cur_height prev_bnode)
                      prev_block (fi tx_idx) (fi out_idx)

            withWallet bc $ \wallet ->
                registerOutPoint wallet outpoint (tx_out prev_tx !! fi out_idx)

hasBlock :: BlockChain -> Hash256 -> IO Bool
hasBlock (BlockChain { bc_chain = chain }) hash =
    if hasBlockInOrphan chain hash then return True
    else
        hasBlockInChain chain hash

-- min version required
minVersion :: Integral t => BlockChain -> Height -> t
minVersion bc height =
    let conf = bc_conf bc in
    if fi height >= tckr_bip65_height conf then 4
    else if fi height >= tckr_bip66_height conf then 3
    else if fi height >= tckr_bip34_height conf then 2
    else 0

-- throws a Rejection when rejecting the block
addBlockFail :: BlockChain -> Block -> IO BlockChain
addBlockFail bc@(BlockChain {
    bc_conf = conf,
    bc_tx_state = tx_state
}) block@(Block {
    vers = vers,
    block_hash = block_hash,
    hash_target = hash_target,
    btimestamp = timestamp,
    merkle_root = merkle_root,
    txns = txns'
}) = do
    expectTrue REJECT_INVALID "require full block" $
        isFullBlock block

    let all_txns = FD.toList txns'

    expectFalseIO REJECT_DUPLICATE "block already exists" $
        bc_chain bc `hasBlockInChain` block_hash

    -- don't check if the block is in orphan pool

    expectTrue REJECT_INVALID "block weight limit passed" $
        blockWeight block <= tckr_block_weight_limit conf

    expectTrue REJECT_INVALID "empty tx list" $
        not (null all_txns)

    expectTrue REJECT_INVALID "hash target not met" $
        hash_target > block_hash

    cur_time <- unixTimestamp

    expectTrue REJECT_INVALID "timestamp exceeds furture timestamp limit" $
        timestamp <= cur_time + tckr_max_block_time_future_diff conf

    expectTrue REJECT_INVALID "first transaction is not coinbase" $
        isCoinbase (head all_txns)

    -- TODO:
    -- for each transaction, apply "tx" checks 2-4
    -- for the coinbase (first) transaction, scriptSig length must be 2-100
    -- reject if sum of transaction sig opcounts > MAX_BLOCK_SIGOPS

    expectTrue REJECT_INVALID "merkle root claimed not correct" $
        merkleRoot block == merkle_root

    case insertBlock (bc_chain bc) block of
        Nothing -> do -- no previous hash found
            expectFalse REJECT_INVALID "repeated orphan block" $
                bc_chain bc `hasBlockInOrphan` block_hash

            tLnM "block orphaned"
            updateChain bc (addOrphan (bc_chain bc) block)

        Just (branch, chain') -> do
            -- block inserted, new branch leaf created

            -- update chain
            bc <- updateChain bc chain'

            -- tLnM (isPartial (txns (block_data branch)))

            -- save the block data first
            -- all actions below will find the block
            -- in the chain as other normal blocks
            saveBlock (bc_chain bc) (cur_height branch) block

            when (tckr_enable_mtp_check conf) $
                expectTrueIO REJECT_INVALID "MTP rule not met" $
                    (btimestamp block >=) <$> medianTimePastBranch bc branch

            when (tckr_enable_difficulty_check conf) $
                expectTrueIO REJECT_INVALID "wrong difficulty" $
                    hashTargetValid bc branch

            let height = branchHeight branch

            -- versions for BIP 34, BIP 66, BIP 65
            expectTrue REJECT_INVALID "bad block version" $
                vers >= minVersion bc height

            let main_branch = mainBranch (bc_chain bc)

            bc <-
                if main_branch == branch then do
                    -- tLnM "adding to the main branch"
                    -- adding to the main branch
                    
                    let skip_tx_check = Just True == do
                            (height, _) <- tckr_block_assumed_valid conf
                            return (cur_height branch < fi height)

                    if not skip_tx_check then
                        verifyBlockTxns bc branch (cur_height branch) block
                    else do
                        -- trust all txns
                        tLnM "txns assumed valid"
                        addTxns tx_state (cur_height branch) block

                    -- update fork deploy status
                    updateForkDeploy bc branch

                    return bc

                else if branch > main_branch then do
                    -- branch becoming the main branch
                    
                    tLnM "main branch change"

                    -- set main branch
                    bc <- updateChain bc (setMainBranch (bc_chain bc) branch)

                    -- if block check fails here, the new block will be rejected
                    -- and no update in main branch is possible
                    withCacheUTXO tx_state $ \tx_state -> do
                        let (fp1, fp2) = forkPath (bc_chain bc) main_branch branch

                        -- [[TxPayload]]
                        -- fp1 is in descending order of heights
                        -- fp1 is from the original main branch
                        forM_ fp1 $ \bnode -> do
                            bnode <- toFullBlockNodeFail (bc_chain bc) bnode
                            revertBlockOnUTXO bc tx_state main_branch (block_data bnode)
        
                        -- recheck all new main branch nodes
                        
                        -- fp2 is in descending order of heights
                        forM_ (reverse fp2) $ \bnode -> do
                            bnode <- toFullBlockNodeFail (bc_chain bc) bnode
                            -- verify all blocks in the new main branch
                            verifyBlockTxns bc branch (cur_height bnode) (block_data bnode)

                    return bc
                else
                    -- block added to a side branch
                    -- leave it for now
                    return bc

            bc <- updateChain bc (removeOrphan (bc_chain bc) block)

            -- retry verification on all orphaned txns
            orphan_txns <- orphanPoolTxns (bc_tx_state bc)
            forM_ orphan_txns (addPoolTx bc)

            forM_ (tail all_txns) $ \tx -> do
                -- remove accepted txns from mem pool
                removePoolTx tx_state (txid tx)

                -- register tx in wallet
                withWallet bc $ \wallet ->
                    registerTx wallet tx

            bc <- collectOrphanBlock bc block_hash
            
            tryFlushChain bc

addPoolTx :: BlockChain -> TxPayload -> IO (Maybe Rejection)
addPoolTx bc tx =
    either Just (const Nothing) <$>
    force <$> try (addPoolTxFail bc tx)

txPoolFull :: BlockChain -> IO Bool
txPoolFull bc =
    (>= tckr_pool_tx_limit (bc_conf bc)) <$> txPoolSize (bc_tx_state bc)

-- verify tx in the pool and either reject it or put it into the mem pool or orphan pool
addPoolTxFail :: BlockChain -> TxPayload -> IO ()
addPoolTxFail bc@(BlockChain {
    bc_conf = conf,
    bc_tx_state = tx_state
}) tx = do
    -- (no check for locktime)
    let main = mainBranch (bc_chain bc)

    now <- unixTimestamp
    ver_conf <- genVerifyConf bc main (mainBranchHeight bc) Nothing

    timeoutPoolTx tx_state (now - tckr_pool_tx_timeout conf)

    expectFalseIO REJECT_INVALID "mem pool full" (txPoolFull bc)

    removeOrphanTx tx_state (txid tx)

    -- check for duplication(in chain, mem pool, and orphan pool)
    expectFalseIO REJECT_DUPLICATE "duplicated tx in chain" $
        hasTxInBranch bc main (txid tx)

    expectFalseIO REJECT_DUPLICATE "duplicated tx in mem pool" $
        hasTxInMemPool tx_state (txid tx)

    verifyBasicTx bc tx

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
        
        expectTrue REJECT_INVALID "sum of inputs less than the sum of output" $
            fee >= 0

        expectTrue REJECT_INSUFFICIENTFEE "min tx fee not met" $
            feeRate fee (sizeOf tx) >= tckr_min_tx_fee_rate conf

        addMemPoolTx tx_state fee tx

        -- retry on any orphan txns with input pointing to the current tx
        dep_orphan_txns <-
            filterOrphanPoolTx tx_state $ \otx ->
                flip any (tx_in otx) $ \(TxInput { prev_out = OutPoint prev_txid _ }) ->
                    prev_txid == txid tx

        -- there is unlikely infinite recursion because
        -- we assume txns don't have cyclic dependence
        mapM_ (addPoolTx bc) dep_orphan_txns

reject :: RejectType -> String -> a
reject rtype msg = throw $ Rejection rtype msg

expect :: Eq a => RejectType -> String -> a -> IO a -> IO ()
expect rtype msg exp mobs = do
    obs <- mobs
    unless (exp == obs) (reject rtype msg)

expectTrueIO rtype msg cond = expect rtype msg True cond
expectFalseIO rtype msg cond = expect rtype msg False cond
expectTrue rtype msg cond = expect rtype msg True (return cond)
expectFalse rtype msg cond = expect rtype msg False (return cond)

expectMaybe rtype msg (Just v) = return v
expectMaybe rtype msg Nothing = reject rtype msg

expectMaybeIO rtype msg m = m >>= expectMaybe rtype msg

expectEither rtype msg (Right v) = return v
expectEither rtype msg (Left err) = reject rtype (msg ++ ": " ++ show err)
