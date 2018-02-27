module Tucker.Chain.Tx where

import Data.Word
import qualified Data.Set as SET
import qualified Data.Foldable as FD

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Conf

type TxLocator = (Hash256, Word32)

txLocator :: Hash256 -> Word32 -> TxLocator
txLocator = (,)

locatorToHash = fst
locatorToIdx = snd

-- a giant structure handling all transactions(orphans, mempool, ...)
data TxSet =
    TxSet {
        tx_set_conf    :: TCKRConf,

        -- from txid to (block hash, tx index)
        -- tx that are valid and accepted in blocks
        bucket_tx      :: DBBucket Hash256 TxLocator,

        -- a set of unspent tx output
        -- tx in this set must be included in blocks
        bucket_utxo    :: DBBucket OutPoint Value,

        -- currently useless without mining
        -- valid yet not included in blocks
        tx_mem_pool    :: SET.Set TxPayload,
        -- input not found but may be valid in the near future
        tx_orphan_pool :: SET.Set TxPayload
    }

initTxSet :: TCKRConf -> Database -> IO TxSet
initTxSet conf@(TCKRConf {
    tckr_bucket_tx_name = tx_name,
    tckr_bucket_utxo_name = utxo_name
}) db = do
    bucket_tx <- openBucket db tx_name
    bucket_utxo <- openBucket db utxo_name

    return $ TxSet {
        tx_set_conf = conf,
        bucket_tx = bucket_tx,
        bucket_utxo = bucket_utxo,

        tx_mem_pool = SET.empty,
        tx_orphan_pool = SET.empty
    }

-- add a list of block-included tx and add/remove utxo respectively
-- this function contains no validation of the tx
addTx :: TxSet -> Block -> Int -> IO ()
addTx (TxSet {
    bucket_tx = bucket_tx,
    bucket_utxo = bucket_utxo
}) block idx = do
    let tx = FD.toList (txns block) !! idx
        spent = map prev_out (tx_in tx)
        len_out = length (tx_out tx)

        -- :: [(OutPoint, Value)]
        unspent =
            map (uncurry OutPoint) (replicate len_out (txid tx) `zip` [0..])
            `zip`
            map value (tx_out tx)

    setB bucket_tx (txid tx) (txLocator (block_hash block) (fi idx))

    -- remove/add spent/unspent outpoint
    mapM_ (deleteB bucket_utxo) spent
    mapM_ (uncurry (setB bucket_utxo)) unspent

-- lookup for a utxo
lookupUTXO :: TxSet -> OutPoint -> IO (Maybe Value)
lookupUTXO = getB . bucket_utxo

-- lookup for a accepted txid
findTxId :: TxSet -> Hash256 -> IO (Maybe TxLocator)
findTxId = getB . bucket_tx
