{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Tucker.State.Tx where

import Data.Word
import Data.Bits
import Data.List
import qualified Data.Foldable as FD

import Control.Monad

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.Atom
import Tucker.DeepSeq

import Tucker.Container.IOMap
import qualified Tucker.Container.Set as SET
import qualified Tucker.Container.Map as MAP

type TxLocator = (Hash256, Word32)

txLocator :: Hash256 -> Word32 -> TxLocator
txLocator = (,)

locatorToHash = fst
locatorToIdx = snd

type UTXOArg c = c OutPoint UTXOValue

type UTXOCache c = UTXOArg (CacheMap c)

-- two layers of cached
type UTXOCache0 = UTXOArg DBBucket
type UTXOCache1 = UTXOCache UTXOCache0
type UTXOCache2 = UTXOCache UTXOCache1
type UTXOCache3 = UTXOCache UTXOCache2

type UTXOMap a = IOMap a OutPoint UTXOValue

data UTXOValue =
    UTXOValue {
        parent_ts     :: Timestamp,
        parent_height :: Height,
        tx_index      :: Word32, -- index of the tx in its parent block
        u_tx_out      :: TxOutput
    } deriving (Show)

instance Encodable UTXOValue where
    encode end (UTXOValue {
        parent_ts = parent_ts,
        parent_height = parent_height,
        tx_index = tx_index,
        u_tx_out = u_tx_out
    }) =
        encode end parent_ts <>
        encode end parent_height <>
        encode end tx_index <>
        encode end u_tx_out

instance Decodable UTXOValue where
    decoder = do
        ts <- decoder
        height <- decoder
        tx_idx <- decoder
        tx_out <- decoder

        return $ UTXOValue {
            parent_ts = ts,
            parent_height = height,
            tx_index = tx_idx,
            u_tx_out = tx_out
        }

{-

1. a update tool for the current db
2. change the value of the UTXO map to a TxOutput
3. change corresponding things in the chain verification

* we need to include more information in the UTXO set
1. TxOutput
2. index of the parant tx in its block
3. mtp of the block

-}

-- class (UTXOMap a, IsCacheMap a) => UTXOCacheN a where

data PoolTx =
    PoolTx {
        tx_data  :: TxPayload,
        inc_time :: Timestamp,
        tx_fee   :: Satoshi
    }

-- a giant structure handling all transactions(orphans, mempool, ...)
data TxState u =
    TxState {
        tx_set_conf    :: TCKRConf,

        -- from txid to (block hash, tx index)
        -- tx that are valid and accepted in blocks
        bucket_tx      :: CacheMapWrap DBBucket Hash256 TxLocator,

        -- a set of unspent tx output
        -- tx in this set must be included in blocks
        -- bucket_utxo    :: DBBucket OutPoint Satoshi,
        utxo_map       :: u,

        -- for efficiency reason, mem pool and orphan pool
        -- are directly stored in mem

        -- currently useless without mining
        -- valid yet not included in blocks
        tx_out_mask    :: Atom (SET.TSet OutPoint), -- outputs used by mem pool txns
        tx_out_new     :: Atom (UTXOArg MAP.TMap), -- new outputs enabled by the mem pool txns
        tx_mem_pool    :: Atom (MAP.TMap Hash256 PoolTx),

        -- NOTE: make sure out_mask and out_new may overlap
        
        -- input not found but may be valid in the near future
        tx_orphan_pool :: Atom (MAP.TMap Hash256 PoolTx)
    }

instance NFData (TxState a) where
    rnf _ = ()

initTxState :: TCKRConf -> Database -> IO (TxState UTXOCache1)
initTxState conf@(TCKRConf {
    tckr_bucket_tx_name = tx_name,
    tckr_bucket_utxo_name = utxo_name
}) db = do
    bucket_tx <- openBucket db tx_name >>= wrapCacheMap
    utxo_map <- openBucket db utxo_name >>= wrapCacheMap
    -- utxo is cached because it needs to be
    -- sync'd it at the same time when chain is flushed back,
    -- otherwise some outpoints in utxo may be
    -- lost due to sudden exit(blockchain may be younger than utxo)

    out_mask <- newA SET.empty
    out_new <- newA MAP.empty
    mem_pool <- newA MAP.empty
    orphan_pool <- newA MAP.empty

    return $ TxState {
        tx_set_conf = conf,
        bucket_tx = bucket_tx,
        utxo_map = utxo_map,

        tx_out_mask = out_mask,
        tx_out_new = out_new,
        tx_mem_pool = mem_pool,
        tx_orphan_pool = orphan_pool
    }

-- add a list of block-included tx and add/remove utxo respectively
-- this function contains no validation of the tx
addTx :: UTXOMap a => TxState a -> Height -> Block -> Int -> IO ()
addTx state@(TxState {
    bucket_tx = bucket_tx,
    utxo_map = utxo_map
}) height block idx = do
    let tx = FD.toList (txns block) !! idx
        insert =
            flip map (zip [0..] (tx_out tx)) $ \(i, output) ->
                -- insert key, value pairs
                (OutPoint (txid tx) i, UTXOValue {
                    parent_ts = btimestamp block,
                    parent_height = height,
                    tx_index = fi idx,
                    u_tx_out = output
                })

        delete = map prev_out (tx_in tx)

    -- add tx to database
    insertIO bucket_tx (txid tx) (txLocator (block_hash block) (fi idx))

    mapM_ (uncurry (insertIO utxo_map)) insert
    mapM_ (deleteIO utxo_map) delete

    -- apply utxo
    -- applyUTXO state tx

-- remove the output of a tx from the utxo
-- NOTE: this function will not recover the outputs
-- removed previously by this tx
removeTx :: UTXOMap a => TxState a -> Block -> Int -> IO ()
removeTx state@(TxState {
    utxo_map = utxo_map
}) block idx = do
    let tx = FD.toList (txns block) !! idx
        delete = map (OutPoint (txid tx)) [ 0 .. fi (length (tx_out tx)) ]

    mapM_ (deleteIO utxo_map) delete

-- add a specific output
addOutput :: UTXOMap a => TxState a -> Height -> Block -> Int -> Int -> IO ()
addOutput state@(TxState {
    utxo_map = utxo_map
}) height block idx out_idx =
    let tx = FD.toList (txns block) !! idx in

    insertIO utxo_map (OutPoint (txid tx) (fi out_idx)) $ UTXOValue {
        parent_ts = btimestamp block,
        parent_height = height,
        tx_index = fi idx,
        u_tx_out = tx_out tx !! out_idx
    }

addTxns :: UTXOMap a => TxState a -> Height -> Block -> IO ()
addTxns state height block =
    let len = length (txns block) in
    mapM_ (addTx state height block) [ 0 .. len - 1 ]

-- lookup for a utxo
lookupUTXO :: UTXOMap a => TxState a -> OutPoint -> IO (Maybe UTXOValue)
lookupUTXO state outpoint = do
    lookupIO (utxo_map state) outpoint

-- lookup for a accepted txid
findTxId :: TxState a -> Hash256 -> IO (Maybe TxLocator)
findTxId = lookupIO . bucket_tx

syncTxState :: UTXOMap a => TxState (UTXOArg (CacheMap a)) -> IO ()
syncTxState state = do
    syncUTXO state
    syncCacheMap (bucket_tx state)

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

-- lookup mem pool along with mem pool
-- only returns TxOuput because we can not guarantee more info(block height, etc.)
lookupUTXOMemPool :: UTXOMap a => TxState a -> OutPoint -> IO (Maybe UTXOValue)
lookupUTXOMemPool state outpoint = do
    masked <- SET.member outpoint <$> getA (tx_out_mask state)

    -- make sure it's not masked by the mem pool txns
    if masked then return Nothing
    else do
        res <- lookupIO (utxo_map state) outpoint

        case res of
            Nothing -> do
                -- look up out_new
                res <- MAP.lookup outpoint <$> getA (tx_out_new state)
                return res

            Just _ -> return res

addMemPoolTx :: UTXOMap a => TxState a -> Satoshi -> TxPayload -> IO ()
addMemPoolTx (TxState {
    tx_out_mask = out_mask,
    tx_out_new = out_new,
    tx_mem_pool = mem_pool
}) fee tx = do
    now <- unixTimestamp

    -- add the tx to the mem pool
    appA (MAP.insert (txid tx) PoolTx {
        tx_data = tx,
        inc_time = now,
        tx_fee = fee
    }) mem_pool

    -- add all used output to out_mask
    forM_ (tx_in tx) $ \input ->
        appA (SET.insert (prev_out input)) out_mask

    -- add new output enabled by the tx
    forM_ ([0..] `zip` tx_out tx) $ \(i, out) ->
        flip appA out_new $ MAP.insert (OutPoint (txid tx) (fi i)) (UTXOValue {
            parent_ts = now, -- mem pool tx is always "current"
            parent_height = maxBound,
            tx_index = maxBound,
            u_tx_out = out
        })

removeMemPoolTx :: UTXOMap a => TxState a -> Hash256 -> IO ()
removeMemPoolTx (TxState {
    tx_out_mask = out_mask,
    tx_out_new = out_new,
    tx_mem_pool = mem_pool
}) txid = do
    mtx <- MAP.lookup txid <$> getA mem_pool

    case mtx of
        Nothing -> return ()
        Just (PoolTx { tx_data = tx }) -> do
            -- delete tx from the mem pool
            appA (MAP.delete txid) mem_pool

            -- remove outputs in the out_mask
            forM_ (tx_in tx) $ \input ->
                appA (SET.delete (prev_out input)) out_mask

            -- remove new output enabled by the tx
            forM_ ([0..] `zip` tx_out tx) $ \(i, out) ->
                appA (MAP.delete (OutPoint txid (fi i))) out_new

addOrphanTx :: UTXOMap a => TxState a -> TxPayload -> IO ()
addOrphanTx state tx = void $ do
    now <- unixTimestamp
    appA (MAP.insert (txid tx) PoolTx {
        tx_data = tx,
        inc_time = now,
        tx_fee = error "no fee for orphan tx"
    }) (tx_orphan_pool state)

removeOrphanTx :: UTXOMap a => TxState a -> Hash256 -> IO ()
removeOrphanTx state txid = do
    appA (MAP.delete txid) (tx_orphan_pool state)
    return ()

removePoolTx :: UTXOMap a => TxState a -> Hash256 -> IO ()
removePoolTx state txid = do
    removeMemPoolTx state txid
    removeOrphanTx state txid

filterOrphanPoolTx :: UTXOMap a => TxState a -> (TxPayload -> Bool) -> IO [TxPayload]
filterOrphanPoolTx state pred =
    filter pred <$> map (tx_data . snd) <$> MAP.toList <$> getA (tx_orphan_pool state)

-- remove timeout txns in the pool
timeoutPoolTx :: UTXOMap a => TxState a -> Timestamp -> IO ()
timeoutPoolTx state min = do
    appA (MAP.filter ((>= min) . inc_time)) (tx_mem_pool state)
    appA (MAP.filter ((>= min) . inc_time)) (tx_orphan_pool state)
    return ()

-- tx pool includes both the mem pool and orphan pool
txPoolSize :: UTXOMap a => TxState a -> IO Int
txPoolSize state =
    (+) <$> memPoolSize state
        <*> orphanPoolSize state

memPoolSize :: UTXOMap a => TxState a -> IO Int
memPoolSize state = MAP.size <$> getA (tx_mem_pool state)

orphanPoolSize :: UTXOMap a => TxState a -> IO Int
orphanPoolSize state = MAP.size <$> getA (tx_orphan_pool state)

hasTxInMemPool :: UTXOMap a => TxState a -> Hash256 -> IO Bool
hasTxInMemPool state hash =
    MAP.member hash <$> getA (tx_mem_pool state)

memPoolTxns :: UTXOMap a => TxState a -> IO [TxPayload]
memPoolTxns state =
    map (tx_data . snd) <$> MAP.toList <$> getA (tx_mem_pool state)

orphanPoolTxns :: UTXOMap a => TxState a -> IO [TxPayload]
orphanPoolTxns state = filterOrphanPoolTx state (const True)

lookupMemPoolTx :: UTXOMap a => TxState a -> Hash256 -> IO (Maybe TxPayload)
lookupMemPoolTx state txid =
    (tx_data <$>) <$> MAP.lookup txid <$> getA (tx_mem_pool state)

hasTxInOrphanPool :: UTXOMap a => TxState a -> Hash256 -> IO Bool
hasTxInOrphanPool state hash =
    MAP.member hash <$> getA (tx_orphan_pool state)

memPoolTxnsSortedByFeeRate :: UTXOMap a => TxState a -> IO [TxPayload]
memPoolTxnsSortedByFeeRate state =
    reverse <$> map tx_data <$>
    sortBy (\a b -> compare (feeRate a) (feeRate b)) <$>
    map snd <$>
    MAP.toList <$> getA (tx_mem_pool state)

    where feeRate ptx = tx_fee ptx `div` fi (sizeOf (tx_data ptx))
