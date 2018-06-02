module Tucker.Wallet.Wallet where

import Data.Word
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Resource

import Tucker.DB
import Tucker.ECC
import Tucker.Enc
import Tucker.Msg
import Tucker.Auth
import Tucker.Conf
import Tucker.Util

import Tucker.Container.IOMap

-- import Tucker.State.Tx

-- shows how to redeem a specific outpoint
data RedeemScheme
    = RedeemScript RawScript
    | RedeemWitness RawScript TxWitness

instance Encodable RedeemScheme where
    encode _ (RedeemScript script) =
        encodeLE (0 :: Word8) <> script

    encode _ (RedeemWitness script wit) =
        encodeLE (1 :: Word8) <> encodeLE wit <> script

instance Decodable RedeemScheme where
    decoder = do
        head <- decoder :: Decoder Word8

        case head of
            0 -> RedeemScript <$> allD
            1 -> do
                wit <- decoder
                script <- allD
                return (RedeemWitness script wit)

            _ -> fail "illegal redeem scheme format"

data Wallet =
    Wallet {
        wal_conf          :: TCKRConf,

        wal_root_key      :: ECCPrivateKey,

        wal_bucket_conf   :: DBBucket String Placeholder,
        wal_bucket_utxo   :: CacheMapWrap DBBucket OutPoint TxOutput,
        wal_bucket_redeem :: DBBucket ByteString RedeemScheme
    }

initWallet :: TCKRConf -> ResIO Wallet
initWallet conf@(TCKRConf {
    tckr_wallet_path = path,
    tckr_wallet_db_max_file = db_max_file,

    tckr_wallet_bucket_conf_name = conf_name,
    tckr_wallet_bucket_utxo_name = utxo_name,
    tckr_wallet_bucket_redeem_name = redeem_name
}) = do
    db <- openDB (optMaxFile def db_max_file) path

    bucket_conf <- lift $ openBucket db conf_name
    bucket_utxo <- lift $ openBucket db utxo_name >>= wrapCacheMap
    bucket_redeem <- lift $ openBucket db redeem_name

    -- init <- (== Just "true") <$> lift (lookupAsIO bucket_conf "init")
    mroot <- lift $ lookupAsIO bucket_conf "root"

    case mroot of
        Just root ->
            return Wallet {
                wal_conf = conf,
                wal_root_key = root,
                wal_bucket_conf = bucket_conf,
                wal_bucket_utxo = bucket_utxo,
                wal_bucket_redeem = bucket_redeem
            }

        Nothing -> error "uninitialized wallet"

isMine :: Wallet -> TxOutput -> IO Bool
isMine wallet output =
    isJust <$> lookupIO (wal_bucket_redeem wallet) hash
    where hash = sha256 (pk_script output)

-- tx should not be coinbase
registerTx :: Wallet -> TxPayload -> IO ()
registerTx wallet tx = do
    -- remove all used outpoints
    forM_ (tx_in tx) (deleteIO (wal_bucket_utxo wallet) . prev_out)

    -- add outpoint if isMine
    forM_ ([0..] `zip` tx_out tx) $ \(i, out) -> do
        is_mine <- isMine wallet out

        when is_mine $
            registerOutPoint wallet (OutPoint (txid tx) i) out

unregisterTx :: Wallet -> TxPayload -> IO ()
unregisterTx wallet tx =
    forM_ ([0..] `zip` tx_out tx) $ \(i, _) ->
        deleteIO (wal_bucket_utxo wallet) (OutPoint (txid tx) i)

registerOutPoint :: Wallet -> OutPoint -> TxOutput -> IO ()
registerOutPoint wallet outpoint output =
    insertIO (wal_bucket_utxo wallet) outpoint output

syncWallet :: Wallet -> IO ()
syncWallet = syncCacheMap . wal_bucket_utxo
