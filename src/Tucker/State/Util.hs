module Tucker.State.Util where

import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Resource

import Tucker.Msg
import Tucker.Util
import Tucker.Conf
import Tucker.Thread

import Tucker.State.Tx
import Tucker.State.Block
import Tucker.State.Chain
import Tucker.State.SoftFork

-- this is a temporary fix for missing outpoints due to db corruption
addOutPoint :: BlockChain -> OutPoint -> IO ()
addOutPoint bc@(BlockChain {
    bc_chain = chain,
    bc_tx_state = tx_state
}) (OutPoint txid out_idx) = do
    (bnode, tx_idx) <- parentBranchOfTx bc tx_state (mainBranch chain) txid

    addOutput tx_state (cur_height bnode)
              (block_data bnode) (fi tx_idx) (fi out_idx)

    syncUTXO tx_state

withBlockChain :: TCKRConf -> Maybe ThreadState -> (BlockChain -> IO a) -> IO a
withBlockChain conf mthread proc = runResourceT $ do
    bc <- initBlockChain conf mthread
    lift $ proc bc

-- fallback to height h
fallbackToHeight :: BlockChain -> Height -> IO ()
fallbackToHeight bc@(BlockChain {
    bc_chain = chain,
    bc_tx_state = tx_state
}) height = do
    db_height <- heightInDb chain
    if height <= db_height then do
        let revert_range = [ db_height, db_height - 1 .. height + 1 ]
            branch = mainBranch chain

        tLnM ("reverting from " ++ show db_height ++ " to " ++ show (height + 1))

        forM_ revert_range $ \height -> do
            tM ("reverting block at " ++ show height)
            Just bnode <- lookupBlockNode chain [branch] (AtHeight height)
            bnode <- toFullBlockNodeFail chain bnode

            -- revert utxo
            revertBlockOnUTXO bc tx_state branch (block_data bnode)

            removeHeightInDb chain height

            syncUTXO tx_state

        tLnM "reverting finished"
    else
        error "can only fallback to a height stored in the database"

setForkStatus :: BlockChain -> String -> SoftForkStatus -> IO ()
setForkStatus (BlockChain {
    bc_fork_state = fork_state
}) name status = do
    Just fork <- getForkByName fork_state name
    changeForkStatus fork_state fork status
    syncForkState fork_state
