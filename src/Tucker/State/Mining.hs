module Tucker.State.Mining where

import Data.Word
import qualified Data.Foldable as FD
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Exception

import Tucker.Msg
import Tucker.Enc
import Tucker.Auth
import Tucker.Conf
import Tucker.Util

import Tucker.State.Tx
import Tucker.State.Block
import Tucker.State.Chain
import Tucker.State.SoftFork

import Tucker.Wallet.Address

{-

BitMiner coinbase
03 92cc13
09 4269744d696e746572
2c fabe6d6d00000000000000000000000000000000000000000000000000000000000000000100000000000000
09 6465765a1e00000001
0000000603000000

unknown coinbase
03 91cc13: height
04 cbdbf85a: time
08 40000015147b0200: extra nonce?
0d 2f6e6f64655374726174756d2f: "/nodeStratum/"

-}

data CoinbaseInfo =
    CoinbaseInfo {
        cb_height :: Height,
        cb_time :: Timestamp,
        cb_nonce :: Word64,
        cb_msg :: String
    }

witnessReserved :: TCKRConf -> ByteString    
witnessReserved conf = nullHash256BS

witnessCommitment :: TCKRConf -> Block -> TxOutput
witnessCommitment conf block =
    TxOutput {
        value = 0,
        pk_script = encodeLE [
            OP_RETURN,
            OP_PUSHDATA (com_header <> com_hash) Nothing
        ]
    }
    where
        com_header = tckr_wit_commit_header conf
        com_hash =
            doubleSHA256 (hash256ToBS (witnessMerkleRoot block) <> witnessReserved conf)

-- config, coinbase msg, receiver address, generated value
coinbaseP2PKH :: TCKRConf -> Block -> CoinbaseInfo -> String -> Satoshi -> TxPayload
coinbaseP2PKH conf block info addr fee =
    let segwit = any hasWitness (FD.toList (txns block)) in

    updateTx def {
        version = 0,
        
        tx_in = [
            TxInput {
                prev_out = OutPoint nullHash256 maxBound,
                sig_script = encodeLE [
                    -- first item is the block height
                    OP_INT (fi (cb_height info)),
                    OP_INT (fi (cb_time info)),
                    OP_INT (fi (cb_nonce info)),
                    OP_PUSHDATA (BS.pack (cb_msg info)) Nothing
                ],
                seqn = maxBound
            }
        ],

        tx_out = [
            TxOutput {
                value = fee,
                pk_script = encodeLE (genPubKeyScript (decodeAddressFail conf addr))
            }
        ] ++ if segwit then [witnessCommitment conf block] else [],

        tx_witness =
            if segwit then
                [ TxWitness [ witnessReserved conf ] ]
            else
                [],

        lock_time = 0
    }

nextVersion :: BlockChain -> IO BlockVersion
nextVersion bc = do
    -- set all "started" softfork bits to 1
    forks <- lookupNonFinalForks (bc_fork_state bc)
    return (genDeployVersion (map fork_bit forks))

nextBlock :: BlockChain -> String -> String -> Word64 -> IO Block
nextBlock bc@(BlockChain {
    bc_conf = conf,
    bc_tx_state = tx_state,
    bc_chain = chain
}) msg addr extra_nonce = do
    now <- unixTimestamp

    base <- nextEmptyBlock bc

    let main = mainBranch chain
        next_height = cur_height main + 1
        gen_fee = feeAtHeight conf next_height

        info = CoinbaseInfo {
                cb_height = next_height,
                cb_time = now,
                cb_nonce = extra_nonce,
                cb_msg = msg
            }

        tmp_coinbase = coinbaseP2PKH conf base info addr 0
    
    base <- return (appendTx tmp_coinbase base)

    -- adding txns
    -- sort txns by fees first
    -- TODO: ignore any interdependence for now
    sorted <- memPoolTxnsSortedByFeeRate tx_state

    let fold_proc (block, tot_fee) tx = do
            res <- try (verifyInclusion bc tx) :: IO (Either Rejection Satoshi)

            case res of
                Right fee -> do
                    let block' = appendTx tx block

                    if blockWeight block' <= tckr_block_weight_limit conf then
                        return (block', tot_fee + fee)
                    else
                        -- weight too large, don't add
                        return (block, tot_fee)
                    
                Left rej -> do
                    tLnM ("tx not included: " ++ show rej)
                    return (block, tot_fee)

    (block, tx_fee) <- foldM fold_proc (base, 0) sorted

    let tot_fee = gen_fee + tx_fee
        coinbase = coinbaseP2PKH conf block info addr tot_fee -- update new tx info

    tLnM (show (length (txns block)) ++ " txns included")

    return (updateCoinbase coinbase block)

    -- append all other txns
    -- return (foldl (\block (tx, _) -> appendTx tx block) base txns)

-- next empty block on the main branch
-- NOTE that this block does not contain a coinbase
nextEmptyBlock :: BlockChain -> IO Block
nextEmptyBlock bc@(BlockChain {
    bc_conf = conf,
    bc_chain = chain
}) = do
    let main = mainBranch chain
        next_height = cur_height main + 1
        use_min_diff = tckr_use_special_min_diff_mine conf
        tip = mainBranchTip bc
        last_time = btimestamp tip
    
    now <- unixTimestamp
    mtp <- medianTimePast bc main next_height

    target <-
        if use_min_diff then
            return (fi (tckr_bdiff_diff1_target conf))
        else
            targetAtHeight bc main next_height last_time

    v <- nextVersion bc

    return $ updateBlock Block {
        block_hash = undefined,
        vers = v,
        prev_hash = block_hash tip,
        merkle_root = 0,

        btimestamp =
            if use_min_diff then
                last_time + 2 * tckr_target_spacing conf + 1
            else
                max mtp now,

        hash_target = target,
        nonce = 0,
        txns = FullList [],
        enc_cache = Nothing
    }

-- check if the tx can be included in the next block(in the main branch)
-- without changing the state
verifyInclusion :: BlockChain -> TxPayload -> IO Satoshi
verifyInclusion bc tx = do
    let main = mainBranch (bc_chain bc)
        height = mainBranchHeight bc
        tx_state = bc_tx_state bc

    ver_conf <- genVerifyConf bc main height Nothing

    verifyLockTime bc ver_conf tx

    when (verify_check_dup_tx ver_conf) (verifyDupTx bc ver_conf tx)
    
    verifyBasicTx bc tx
    verifyFlow bc tx_state ver_conf tx
