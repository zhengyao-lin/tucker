module Tucker.Wallet.Wallet where

import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Exception
import Control.Monad.Morph
import Control.Monad.Trans.Resource

import System.Directory

import Tucker.DB
import Tucker.ECC
import Tucker.Enc
import Tucker.Msg
import Tucker.Conf
import Tucker.Util
import Tucker.Error
import Tucker.Crypto

import Tucker.Container.IOMap

import Tucker.Wallet.HD
import Tucker.Wallet.Mnemonic
import Tucker.Wallet.TxBuilder

-- import Tucker.State.Tx

{-

[
    ([OP_PUSHDATA "\ETXh:\241\186WC\189\252y\140\248\DC4\239\238\171'5\236R\217^\206\213(\230\146\184\227LNVi" Nothing,OP_CHECKSIG],
     RedeemSign []),
    
    ([OP_PUSHDATA "\EOTh:\241\186WC\189\252y\140\248\DC4\239\238\171'5\236R\217^\206\213(\230\146\184\227LNVi\210\242hl\237\150\211u\167R\152\240~\211\aQ\226\163\244^-\CANK&\141\STX\200\213\221o\189\181" Nothing,OP_CHECKSIG],
     RedeemSign []),
    
    ([OP_DUP,OP_HASH160,OP_PUSHDATA "A\214;P\216\221^s\f\223Ly\165o\201)\167W\197H" Nothing,OP_EQUALVERIFY,OP_CHECKSIG],
     RedeemSign [OP_PUSHDATA "\ETXh:\241\186WC\189\252y\140\248\DC4\239\238\171'5\236R\217^\206\213(\230\146\184\227LNVi" Nothing]),
    
    ([OP_DUP,OP_HASH160,OP_PUSHDATA "\230*E\238^\224\154\222\164\v\r\GS\210\rp\221<\148\209\156" Nothing,OP_EQUALVERIFY,OP_CHECKSIG],
     RedeemSign [OP_PUSHDATA "\EOTh:\241\186WC\189\252y\140\248\DC4\239\238\171'5\236R\217^\206\213(\230\146\184\227LNVi\210\242hl\237\150\211u\167R\152\240~\211\aQ\226\163\244^-\CANK&\141\STX\200\213\221o\189\181" Nothing]),
    
    ([OP_HASH160,OP_PUSHDATA "\247|\226\196,\175\138\202\244\223\149u4\190\145q\206/\175\252" Nothing,OP_EQUAL],
     RedeemSign [OP_PUSHDATA "!\ETXh:\241\186WC\189\252y\140\248\DC4\239\238\171'5\236R\217^\206\213(\230\146\184\227LNVi\172" Nothing]),
     
    ([OP_HASH160,OP_PUSHDATA "m\254cEE;c\249\&0 VjA\189\129\244\188f\128\191" Nothing,OP_EQUAL],
     RedeemSign [OP_PUSHDATA "A\EOTh:\241\186WC\189\252y\140\248\DC4\239\238\171'5\236R\217^\206\213(\230\146\184\227LNVi\210\242hl\237\150\211u\167R\152\240~\211\aQ\226\163\244^-\CANK&\141\STX\200\213\221o\189\181\172" Nothing]),
     
    ([OP_HASH160,OP_PUSHDATA "7\FS\255\SO\190\210g#\179\169\SI\184\130\233\226\131\153\187\203\253" Nothing,OP_EQUAL],
     RedeemSign [OP_PUSHDATA "\ETXh:\241\186WC\189\252y\140\248\DC4\239\238\171'5\236R\217^\206\213(\230\146\184\227LNVi" Nothing,OP_PUSHDATA "v\169\DC4A\214;P\216\221^s\f\223Ly\165o\201)\167W\197H\136\172" Nothing]),
     
    ([OP_HASH160,OP_PUSHDATA "\DC2\162\195\&1{\SOH\139s\147\223\245-\t\223Z:k\221/\213" Nothing,OP_EQUAL],
     RedeemSign [OP_PUSHDATA "\EOTh:\241\186WC\189\252y\140\248\DC4\239\238\171'5\236R\217^\206\213(\230\146\184\227LNVi\210\242hl\237\150\211u\167R\152\240~\211\aQ\226\163\244^-\CANK&\141\STX\200\213\221o\189\181" Nothing,OP_PUSHDATA "v\169\DC4\230*E\238^\224\154\222\164\v\r\GS\210\rp\221<\148\209\156\136\172" Nothing])
]

-}

-- shows how to redeem a specific outpoint
data RedeemScheme
    = RedeemScript [ScriptOp]
    | RedeemWitness [ScriptOp] TxWitness
    | RedeemSign [ScriptOp]
    deriving (Show)

data RedeemOutPoint =
    RedeemOutPoint {
        rd_priv_key :: ECCPrivateKey,
        rd_prev_out :: OutPoint,
        rd_output   :: TxOutput,
        rd_scheme   :: RedeemScheme
    }

instance TxInputBuilder RedeemOutPoint where
    toDraftInput conf (RedeemOutPoint _ prev_out _ (RedeemScript script)) =
        return (TxInput {
            prev_out = prev_out,
            sig_script = encodeLE script,
            seqn = maxBound
        }, nullWitness)

    toDraftInput conf (RedeemOutPoint _ prev_out _ (RedeemWitness script wit)) =
        return (TxInput {
            prev_out = prev_out,
            sig_script = encodeLE script,
            seqn = maxBound
        }, wit)

    toDraftInput conf (RedeemOutPoint _ prev_out _ (RedeemSign script)) =
        return (TxInput {
            prev_out = prev_out,
            sig_script = BSR.empty,
            seqn = maxBound
        }, nullWitness)

    toFinalInput conf tx in_idx (RedeemOutPoint priv prev_out output (RedeemSign script)) = do
        let htype = HashType [SIGHASH_ALL]
            hash = txSigHashLegacy tx in_idx (decodeFailLE (pk_script output)) htype

        sig <- sign priv hash
         
        return (TxInput {
            prev_out = prev_out,
            sig_script = encodeLE $ [
                    OP_PUSHDATA (encodeAE sig <> encodeBE (hashTypeToInt htype :: Word8)) Nothing
                ] ++ script,
            seqn = maxBound
        }, nullWitness)

    toFinalInput conf _ _ r = toDraftInput conf r

instance Encodable RedeemScheme where
    encode _ (RedeemScript script) =
        encodeLE (0 :: Word8) <> encodeLE script

    encode _ (RedeemWitness script wit) =
        encodeLE (1 :: Word8) <> encodeLE wit <> encodeLE script

    encode _ (RedeemSign append) =
        encodeLE (2 :: Word8) <> encodeLE append

instance Decodable RedeemScheme where
    decoder = do
        head <- decoder :: Decoder Word8

        case head of
            0 -> RedeemScript <$> decoder
            1 -> do
                wit <- decoder
                script <- decoder
                return (RedeemWitness script wit)

            2 -> RedeemSign <$> decoder

            _ -> fail "illegal redeem scheme format"

data Wallet =
    Wallet {
        wal_conf          :: TCKRConf,

        wal_root_key      :: ECCPrivateKey,

        wal_bucket_conf   :: DBBucket String Placeholder,
        wal_bucket_utxo   :: CacheMapWrap DBBucket OutPoint TxOutput,
        wal_bucket_redeem :: DBBucket ByteString RedeemScheme
    }

type PrimaryAddress = ByteString -- hash160 of the compressed public key

initWallet :: TCKRConf -> ResIO Wallet
initWallet conf@(TCKRConf {
    tckr_wallet_path = path,
    tckr_wallet_db_max_file = db_max_file,

    tckr_wallet_bucket_conf_name = conf_name,
    tckr_wallet_bucket_utxo_name = utxo_name,
    tckr_wallet_bucket_redeem_name = redeem_name
}) = do
    exist <- lift $ doesPathExist path

    assertMT "the path of the wallet does not exist" exist

    db <- openDB (optMaxFile db_max_file def) path

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

getPrimaryAddress :: Wallet -> PrimaryAddress
getPrimaryAddress =
    ripemd160 . sha256 . encodeAE . compress . privToPub . wal_root_key

-- create a new wallet from mnemonic words
newWalletFromMnemonic :: TCKRConf -> [String] -> Maybe String -> IO ()
newWalletFromMnemonic conf words mpass =
    case mnemonicToSeed def words mpass of
        Right seed -> newWalletFromSeed conf seed
        Left err -> throw err

allPossiblePayments :: TCKRConf -> ECCPrivateKey -> [([ScriptOp], RedeemScheme)]
allPossiblePayments conf priv =
    let pub = privToPub priv

        pubs = [
                encodeLE (compress pub),
                encodeLE (uncompress pub)
            ]

        hash160 = ripemd160 . sha256

        -- pub_hashes = hash160 <$> pubs

        p2pk = for pubs $ \pub ->
            ([ OP_PUSHDATA pub Nothing, OP_CHECKSIG ], RedeemSign [])

        p2pkh = for pubs $ \pub ->
            ([ OP_DUP, OP_HASH160, OP_PUSHDATA (hash160 pub) Nothing, OP_EQUALVERIFY, OP_CHECKSIG ],
             RedeemSign [ OP_PUSHDATA pub Nothing ])

        p2sh = for (p2pk ++ p2pkh) $ \(script, RedeemSign append) ->
            ([ OP_HASH160, OP_PUSHDATA (hash160 (encodeLE script)) Nothing, OP_EQUAL ],
             RedeemSign (append ++ [ OP_PUSHDATA (encodeLE script) Nothing ]))

    in p2pk ++ p2pkh ++ p2sh

newWalletFromSeed :: TCKRConf -> ByteString -> IO ()
newWalletFromSeed conf@(TCKRConf {
    tckr_wallet_path = path,
    tckr_wallet_bucket_conf_name = conf_name,
    tckr_wallet_bucket_utxo_name = utxo_name,
    tckr_wallet_bucket_redeem_name = redeem_name
}) seed = do
    let path = tckr_wallet_path conf
    
    exist <- doesPathExist path

    assertMT "path already exists(maybe an old wallet is there)" (not exist)

    let key = either throw id (seedToMaskerKey conf seed)

    withDB def path $ \db -> do
        bucket_conf <- openBucket db conf_name
        bucket_redeem <- openBucket db redeem_name

        let priv = toECCPrivateKey key

        -- generate all possible patterns and their respective redeem schemes
        -- p2pk
        -- p2pkh
        -- p2sh - p2pkh
        -- p2sh - p2pk
        -- leave the witness versions for now
        -- p2wpkh
        -- p2sh - p2wpkh
        forM_ (allPossiblePayments conf priv) $ \(pk_script, redeem) ->
            insertIO bucket_redeem (sha256 (encodeLE pk_script)) redeem

        -- set the root key
        insertAsIO bucket_conf "root" priv

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
