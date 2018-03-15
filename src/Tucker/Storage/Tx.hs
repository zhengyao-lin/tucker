{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Tucker.Storage.Tx where

import Data.Word
import Data.Bits
import qualified Data.Set as SET
import qualified Data.Foldable as FD

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.IOMap
import Tucker.DeepSeq

type TxLocator = (Hash256, Word32)

txLocator :: Hash256 -> Word32 -> TxLocator
txLocator = (,)

locatorToHash = fst
locatorToIdx = snd

-- the last byte of indicates whether the outpoint is spent
type UTXOArg c = c OutPoint Value

type UTXOCache c = UTXOArg (CacheMap c)

-- two layers of cached
type UTXOCache0 = UTXOArg DBBucket
type UTXOCache1 = UTXOCache UTXOCache0
type UTXOCache2 = UTXOCache UTXOCache1
type UTXOCache3 = UTXOCache UTXOCache2

type UTXOMap a = IOMap a OutPoint Value

-- class (UTXOMap a, IsCacheMap a) => UTXOCacheN a where

-- a giant structure handling all transactions(orphans, mempool, ...)
data TxState u =
    TxState {
        tx_set_conf    :: TCKRConf,

        -- from txid to (block hash, tx index)
        -- tx that are valid and accepted in blocks
        bucket_tx      :: DBBucket Hash256 TxLocator,

        -- a set of unspent tx output
        -- tx in this set must be included in blocks
        -- bucket_utxo    :: DBBucket OutPoint Value,
        utxo_map       :: u,

        -- currently useless without mining
        -- valid yet not included in blocks
        tx_mem_pool    :: SET.Set TxPayload,
        -- input not found but may be valid in the near future
        tx_orphan_pool :: SET.Set TxPayload
    }

instance NFData (TxState a) where
    rnf _ = ()

initTxState :: TCKRConf -> Database -> IO (TxState UTXOCache1)
initTxState conf@(TCKRConf {
    tckr_bucket_tx_name = tx_name,
    tckr_bucket_utxo_name = utxo_name
}) db = do
    bucket_tx <- openBucket db tx_name
    bucket_utxo <- openBucket db utxo_name

    -- utxo is cached because it needs to be
    -- sync'd it at the same time when chain is flushed back,
    -- otherwise some outpoints in utxo may be
    -- lost due to sudden exit(blockchain may be younger than utxo)
    utxo_map <- wrapCacheMap bucket_utxo

    return $ TxState {
        tx_set_conf = conf,
        bucket_tx = bucket_tx,
        utxo_map = utxo_map,

        tx_mem_pool = SET.empty,
        tx_orphan_pool = SET.empty
    }

getSpentAndUnspent :: TxPayload -> ([OutPoint], [(OutPoint, Value)])
getSpentAndUnspent tx =
    let spent = map prev_out (tx_in tx)
        len_out = length (tx_out tx)

        -- :: [(OutPoint, Value)]
        unspent = map (uncurry OutPoint) (replicate len_out (txid tx) `zip` [0..])
                  `zip`
                  map value (tx_out tx)

    in (spent, unspent)

-- core state change in bitcoin
applyUTXO :: UTXOMap a => TxState a -> TxPayload -> IO ()
applyUTXO (TxState { utxo_map = utxo_map }) tx = do
    let (spent, unspent) = getSpentAndUnspent tx

    -- remove/add spent/unspent outpoint
    -- mapM_ (deleteIO utxo_map) spent
    mapM_ (applyIO utxo_map complement) spent
    -- take the complement of the value instead of deleting the entry
    
    mapM_ (uncurry (insertIO utxo_map)) unspent

-- revert the UTXO state
-- ASSUMING the tx has already been applied to UTXO
revertUTXO :: UTXOMap a => TxState a -> TxPayload -> IO ()
revertUTXO (TxState { utxo_map = utxo_map }) tx = do
    let (spent, unspent) = getSpentAndUnspent tx

    mapM_ (applyIO utxo_map complement) spent
    mapM_ (deleteIO utxo_map . fst) unspent

-- add a list of block-included tx and add/remove utxo respectively
-- this function contains no validation of the tx
addTx :: UTXOMap a => TxState a -> Block -> Int -> IO ()
addTx state@(TxState {
    bucket_tx = bucket_tx,
    utxo_map = utxo_map
}) block idx = do
    let tx = FD.toList (txns block) !! idx

    -- add tx to database
    insertIO bucket_tx (txid tx) (txLocator (block_hash block) (fi idx))

    -- apply utxo
    applyUTXO state tx

addTxns :: UTXOMap a => TxState a -> Block -> IO ()
addTxns state block =
    let len = length (txns block) in
    mapM_ (addTx state block) [ 0 .. len - 1 ]

-- lookup for a utxo
lookupUTXO :: UTXOMap a => TxState a -> OutPoint -> IO (Maybe Value)
lookupUTXO state outpoint = do
    mvalue <- lookupIO (utxo_map state) outpoint

    return $ mvalue >>= \v ->
        if v >= 0 then return v
        else fail "spent value"

-- lookup for a accepted txid
findTxId :: TxState a -> Hash256 -> IO (Maybe TxLocator)
findTxId = lookupIO . bucket_tx

syncUTXO :: UTXOMap a => TxState (UTXOArg (CacheMap a)) -> IO ()
syncUTXO = syncCacheMap . utxo_map

cacheUTXO :: UTXOMap a => TxState a -> IO (TxState (UTXOCache a))
cacheUTXO state = do
    new_map <- wrapCacheMap (utxo_map state)
    return (state { utxo_map = new_map })

withCacheUTXO :: UTXOMap a => TxState a -> (TxState (UTXOCache a) -> IO b) -> IO b
withCacheUTXO state proc = do
    new_state <- cacheUTXO state
    res <- proc new_state
    syncUTXO new_state
    return res
