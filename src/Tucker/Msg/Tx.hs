{-# LANGUAGE FlexibleInstances #-}

module Tucker.Msg.Tx where

import Data.Hex
import Data.Int
import Data.Char
import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

import Tucker.Enc
import Tucker.Conf
import Tucker.Auth
import Tucker.Util
import Tucker.Error
import Tucker.DeepSeq

import Tucker.Msg.Common
import Tucker.Msg.Hash256
import Tucker.Msg.ScriptOp

data OutPoint = OutPoint {
        tx_hash :: Hash256,
        out_idx :: Word32
    } deriving (Eq, Show, Read)

instance NFData OutPoint where
    rnf (OutPoint h w) = rnf (h, w)

type Value = Int64

data TxInput =
    TxInput {
        prev_out        :: OutPoint,
        sig_script      :: RawScript,
        seqn            :: Int32 -- sequence, currently not used
    } deriving (Eq, Show, Read)

instance NFData TxInput where
    rnf (TxInput a b c) = rnf (a, b, c)

data TxOutput =
    TxOutput {
        value           :: Value, -- in Satoshis, 10^-8 BTC
        pk_script       :: RawScript
    } deriving (Eq, Show)

instance NFData TxOutput where
    rnf (TxOutput a b) = rnf (a, b)

-- TxWitness is a list of stack items in general
data TxWitness =
    TxWitness [ByteString] deriving (Eq, Show)

instance NFData TxWitness where
    rnf (TxWitness items) = rnf items

data TxPayload =
    TxPayload {
        txid        :: Hash256,
        wtxid       :: Hash256,

        version     :: Int32,
        flag        :: Word8, -- currently only 1 or 0
        
        tx_in       :: [TxInput],
        tx_out      :: [TxOutput],
        tx_witness  :: [TxWitness],

        lock_time   :: Int32  -- the earliest time the tx can be used
                              -- if lock_time < 500,000,000, treat it as a block height
                              -- if lock_time >= 500,000,000, treat it as an unix timestamp
        -- tx_cache    :: Maybe ByteString
    } deriving (Eq, Show)

instance NFData TxPayload where
    rnf (TxPayload txid wtxid version flag tx_in tx_out tx_witness lock_time) =
        rnf txid `seq`
        rnf wtxid `seq`
        rnf version `seq`
        rnf flag `seq`
        rnf tx_in `seq`
        rnf tx_out `seq`
        rnf tx_witness `seq`
        rnf lock_time

-- data Wallet =
--     Wallet {
--         keypair :: ECCKeyPair
--     }

-- data OutPoint = OutPoint Hash Word32

instance Encodable OutPoint where
    encode end (OutPoint hash index) =
        encode end hash <> encode end index

instance Decodable OutPoint where
    decoder = do
        hash <- decoder
        index <- decoder
        return $ OutPoint hash index

instance Encodable TxInput where
    encode end (TxInput {
        prev_out = prev_out,
        sig_script = sig_script,
        seqn = seqn
    }) =
        mconcat [
            e prev_out,
            e (VInt $ fi $ BSR.length sig_script),
            e sig_script,
            e seqn
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable TxInput where
    decoder = do
        -- traceM "decoding tx input"

        prev_out <- decoder
        (VInt slen) <- decoder
        sig_script <- bsD $ fi slen
        seqn <- decoder

        -- traceM "decoding tx input finished"

        return $ TxInput {
            prev_out = prev_out,
            sig_script = sig_script,
            seqn = seqn
        }

instance Encodable TxOutput where
    encode end (TxOutput {
        value = value,
        pk_script = pk_script
    }) =
        mconcat [
            e value,
            e (VInt $ fi $ BSR.length pk_script),
            e pk_script
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable TxOutput where
    decoder = do
        value <- decoder
        (VInt slen) <- decoder
        pk_script <- bsD $ fi slen
        return $ TxOutput {
            value = value,
            pk_script = pk_script
        }

instance Encodable TxWitness where
    encode end (TxWitness items) =
        encodeVList end $ map VBStr items

instance Decodable TxWitness where
    decoder =
        vlistD decoder >>=
        return . TxWitness . map vstrToBS

instance MsgPayload TxPayload

instance Encodable TxPayload where
    -- encode end (TxPayload {
    --     tx_cache = Just cache
    -- }) = cache

    encode end (TxPayload {
        version = version,
        flag = flag, -- currently only 1 or 0
        
        tx_in = tx_in,
        tx_out = tx_out,
        tx_witness = tx_witness,

        lock_time = lock_time
    }) =
        mconcat [
            e version,
            
            if flag == 0 then BSR.empty else
                e (0x00 :: Word8) <>
                e flag,

            encodeVList end tx_in,
            encodeVList end tx_out,

            if flag == 0 then BSR.empty else
                encode end tx_witness, -- just an ordinary list, not vlist
            
            e lock_time
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable TxPayload where
    decoder = do
        buf <- allD'
        init_len <- lenD

        version <- decoder

        mark <- peekByteD

        flag <-
            if mark == 0 then byteD
            else return 0

        -- traceM "decoding tx"
        tx_in <- vlistD decoder
        -- traceM "input finished"
        tx_out <- vlistD decoder
        -- traceM "output finished"

        -- tx_witness is NOT a vlist because the size of it
        -- is implied by the size of tx_in
        tx_witness <-
            if flag /= 0 then listD (length tx_in) decoder
            else return []

        lock_time <- decoder

        final_len <- lenD

        return $ updateIds $ TxPayload {
            txid = nullHash256,
            wtxid = nullHash256,

            version = version,
            flag = flag,

            tx_in = tx_in,
            tx_out = tx_out,
            tx_witness = tx_witness,

            lock_time = lock_time

            -- tx_cache = Just $ BSR.take (init_len - final_len) buf
        }

-- tx with only 1 input with block hash 0 and n -1
isCoinbase :: TxPayload -> Bool
isCoinbase (TxPayload {
    tx_in = [TxInput {
        prev_out = OutPoint 0 (-1)
    }]
}) = True

isCoinbase _ = False

getTxId :: TxPayload -> Hash256
getTxId (TxPayload {
    version = version,
    tx_in = tx_in,
    tx_out = tx_out,
    lock_time = lock_time
}) =
    stdHash256 $ mconcat [
        encodeLE version,
        encodeVList LittleEndian tx_in,
        encodeVList LittleEndian tx_out,
        encodeLE lock_time
    ]

getWtxId :: TxPayload -> Hash256
getWtxId tx =
    stdHash256 $ encodeLE tx

-- update txid and wtxid
updateIds :: TxPayload -> TxPayload
updateIds tx =
    tx {
        txid = getTxId tx,
        wtxid = getWtxId tx
    }

-- update on Jan 14, 2018
-- there are(maybe) 5 standard transactions
-- 1. P2PKH(currently implemented)
--    sig_script: <signature> <public key>
--     pk_script: OP_DUP OP_HASH160 <public key hash> OP_EQUAL OP_CHECKSIG
-- 2. P2PK
--    sig_script: <signature>
--     pk_script: <public key> OP_CHECKSIG
-- 3. multi-signature(m out of n signatures must be provided)
--    sig_script: OP_0 <signature 1> <signature 2>
--     pk_script: M <public key 1> <public key 2> ... <public key N> N OP_CHECKMULTISIG
-- 4. data storage
--    sig_script: no sig_script(not spendable)
--     pk_script: OP_RETURN <data>(max 40 bytes)
-- 5. pay to script hash
--     reason: the length of UTXO of type 3 is too long(because it needs to contain many long public keys)
--     using P2SH, the script(redeem script) is presented later to save some memory on full nodes(maybe?)
--    sig_script: <sig> [sig] [sig...] <redeem_script>
--     pk_script: OP_HASH160 <hash160(redeem_script)> OP_EQUAL

-- what do we want:
-- given
--     1. A wallet
--     2. A series of input tx
--     3. A series of output tx
--     4. Build a signed standard tx body

-- what do we do:
-- 1. construct a raw tx body(for signing):
--     a) sig_script in each input is the old pk_script from the tx(may require looking up?)
--     b) new standard pk_script for each output
--     c) construct a standard pk_script by
--         OP_DUP
--         OP_HASH160
--         PUSHDATA1 <length of the address>
--         address(already base58check decoded)
--         OP_EQUALVERIFY
--         OP_CHECKSIG
--     d) remember to append 0x01000000 to the back of the raw body
-- 2. sign the raw body by ecc(sha256(sha256(body))) -> signature
-- 3. construct new sig_script by
--    OP_PUSHDATA1 <length of signature + 1>
--    signature
--    0x01
--    OP_PUSHDATA1 <length of public key>
--    public key
-- 4. construct a final tx body with sig_script all equaling to the script above
-- 5. note that in a tx_in, previous hash is the DOUBLE-sha256 of the previous transaction

-- get the raw hashes used as signature content
-- arg 1: current transaction
-- arg 2: input index that is currently checking/signing
-- arg 3: corresponding output
-- arg 4: hash type used
-- return: hash for signature of the current input

-- remove the witness data
stripWitness :: TxPayload -> TxPayload
stripWitness tx =
    tx {
        -- wtxid = txid tx,
        flag = 0,
        tx_witness = []
    }

-- raw tx to be signed
sigRawTx :: TxPayload -> Word32 -> [ScriptOp] -> HashType -> ByteString
sigRawTx tx idx' subscript htype =
    let tx_copy = stripWitness $ tx {
                -- remove all sig_script
                tx_in = map (\inp -> inp { sig_script = BSR.empty }) (tx_in tx)
            }

        idx = fi idx'
        
        -- subscript = decodeFailLE subscript' -- extractValidCode $ decodeFailLE $ pk_script prev

        inp = tx_in tx !! idx

        raw_htype = hashTypeToInt htype :: Word32

        final_tx = tx_copy {
            tx_in = replace idx ((tx_in tx_copy !! idx) {
                -- replace the corresponding sig_script
                -- with pk_script after OP_CODESEPARATOR
                sig_script = encodeLE subscript
            }) (tx_in tx_copy)
        }

    in
        -- trace (show final_tx) $
        encodeLE final_tx <> encodeLE raw_htype
        -- trace (show (prev, final_tx, hex final_str)) $
        -- bsToHash256 $ ba2bs $ sha256 $ sha256 final_str

-- generate a standard public key script
-- stdPkScript :: TCKRConf
--             -> Address {- alas String, base58check encoded -}
--             -> Either TCKRError ByteString
-- stdPkScript conf addr = do
--     pub_hash <- addr2hash conf addr
--     return $ encodeLE [
--             OP_DUP,
--             OP_HASH160,
--             OP_PUSHDATA pub_hash,
--             OP_EQUALVERIFY,
--             OP_CHECKSIG
--         ]

-- sign a raw transaction
-- signRawTx :: ECCKeyPair -> TxPayload -> IO ByteString
-- signRawTx pair tx = do
--     let
--         raw = encodeLE tx
--         -- the first sha256 performed here
--         hash_raw = ba2bs $ sha256 $ raw <> BSR.pack [ 0x01, 0x00, 0x00, 0x00 ]

--     -- another sha256 is performed here
--     -- seq (trace (show $ sha256 $ sha256 raw) 0) $
--     signSHA256DER pair hash_raw

-- stdSigScript :: ECCKeyPair -> ByteString -> ByteString
-- stdSigScript pair sign =
--     encodeLE [
--         OP_PUSHDATA $ sign <> bchar 0x01,
--         OP_PUSHDATA $ pair2pubenc pair
--     ]

-- -- create a standard transaction using the key pair, input, and output given
-- -- assuming the previous transaction is also standard, so we can generate the
-- -- pk_script without pulling it from elsewhere
-- stdTx :: TCKRConf -> ECCKeyPair -> [OutPoint] -> [(Value, Address)] -> Either TCKRError (IO TxPayload)
-- stdTx conf pair input output = do
--     let
--         self_addr = pair2addr conf pair

--     in_lst <- mapM (\outp -> do
--         -- assuming the output point has a standard script
--         script <- stdPkScript conf self_addr
--         return TxInput {
--             prev_out = outp,
--             sig_script = script,
--             seqn = -1
--         }) input

--     out_lst <- mapM (\(v, a) -> do
--         script <- stdPkScript conf a
--         return TxOutput {
--             value = v,
--             pk_script = script
--         }) output

--     let
--         wrap in_lst out_lst = updateIds $ TxPayload {
--             txid = nullHash256,
--             wtxid = nullHash256,

--             version = 1,
--             flag = 0,

--             tx_in = in_lst,
--             tx_out = out_lst,

--             tx_witness = [],
--             lock_time = 0

--             -- tx_cache = Nothing
--         }

--         raw = wrap in_lst out_lst

--     return $ do -- IO
--         sign <- signRawTx pair raw
--         let script = stdSigScript pair sign
        
--         -- replace all sig_script

--         new_in_lst <- mapM (\outp -> do
--             return TxInput {
--                 prev_out = outp,
--                 sig_script = script,
--                 seqn = -1
--             }) input
        
--         return $ wrap new_in_lst out_lst

-- unpackEither :: Either TCKRError a -> a
-- unpackEither (Right v) = v
-- unpackEither (Left err) = error $ show err

-- buildTxPayload :: TCKRConf -> WIF -> [OutPoint] -> [(Value, Address)] -> IO TxPayload
-- buildTxPayload conf wif in_lst out_lst =
--     unpackEither $ stdTx conf pair in_lst out_lst
--     where pair = unpackEither $ wif2pair conf wif

-- encodeTxPayload :: TCKRConf -> WIF -> [OutPoint] -> [(Value, Address)] -> IO ByteString
-- encodeTxPayload conf wif in_lst out_lst = do
--     tx <- buildTxPayload conf wif in_lst out_lst
--     return $ encodeLE tx

-- testBuildTx "5K31VmkAYGwaufdSF7osog9SmGNtzxX9ACsXMFrxJ1NsAmzkje9" [ OutPoint "81b4c832d70cb56ff957589752eb4125a4cab78a25a8fc52d6a09e5bd4404d48" 0 ] [ (10, "5K31VmkAYGwaufdSF7osog9SmGNtzxX9ACsXMFrxJ1NsAmzkje9") ]
-- testBuildTx "5HusYj2b2x4nroApgfvaSfKYZhRbKFH41bVyPooymbC6KfgSXdD" [ OutPoint (((!! 0) . unhex) "81b4c832d70cb56ff957589752eb4125a4cab78a25a8fc52d6a09e5bd4404d48") 0 ] [ (91234, "1KKKK6N21XKo48zWKuQKXdvSsCf95ibHFa") ]

-- total 1.3 btc = 130000000 satoshis
-- testBuildTx "933qtT8Ct7rGh29Eyb5gG69QrWmwGein85F1kuoShaGjJFFBSjk" [ OutPoint (((!! 0) . unhex) "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5") 0 ] [ (1000, "miro9ZNPjcLnqvnJpSm8P6CUf1WPU98jET"), (129899000, "mvU2ysD322amhCeCPMhPc3L7hKDGGWSBz7") ] -- tip 0.001

-- encodeTxPayload btc_testnet3 "933qtT8Ct7rGh29Eyb5gG69QrWmwGein85F1kuoShaGjJFFBSjk" [ OutPoint (((!! 0) . unhex) "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5") 0 ] [ (1000, "miro9ZNPjcLnqvnJpSm8P6CUf1WPU98jET"), (129899000, "mvU2ysD322amhCeCPMhPc3L7hKDGGWSBz7") ]

-- dehex v = case unhex v :: Maybe String of
--     Just str -> str
--     Nothing -> error "illegal hex"
