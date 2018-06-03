{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Tucker.Msg.Tx where

import Data.Hex
import Data.Int
import Data.Char
import Data.Word
import Data.Bits
import Data.Hashable
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Error
import Tucker.Crypto
import Tucker.DeepSeq

import Tucker.Msg.Common
import Tucker.Msg.Hash256
import Tucker.Msg.ScriptOp

data OutPoint = OutPoint {
        tx_hash :: Hash256,
        out_idx :: Word32
    } deriving (Eq, Show, Read)

instance Ord OutPoint where
    compare a b =
        case compare (tx_hash a) (tx_hash b) of
            EQ -> compare (out_idx a) (out_idx b)
            r  -> r

instance NFData OutPoint where
    rnf (OutPoint h w) = rnf (h, w)

instance Hashable OutPoint where
    hashWithSalt salt (OutPoint h idx) =
        hashWithSalt (salt + fi idx) h

data TxInput =
    TxInput {
        prev_out        :: OutPoint,
        sig_script      :: RawScript,
        seqn            :: Word32 -- sequence, currently not used
    } deriving (Eq, Show, Read)

data LockTime
    = LockTimeStamp Timestamp
    | LockTimeHeight Height deriving (Eq, Show)

data RelLockTime
    = RelLockTime512S Word
    | RelLockTimeHeight Height deriving (Show)

sequenceToRelLockTime :: Word32 -> Maybe RelLockTime
sequenceToRelLockTime s =
    if s .&. 0x80000000 == 0 then
        -- disable flag not set
        if s .&. 0x00400000 == 0 then
            Just (RelLockTimeHeight (fi v))
        else
            Just (RelLockTime512S (fi v))
    else
        Nothing
    where v = s .&. 0xffff

inputRelLockTime :: TxInput -> Maybe RelLockTime
inputRelLockTime input = sequenceToRelLockTime (seqn input)

numToLockTime :: Word32 -> LockTime
numToLockTime n =
    if n < 500000000 then
        LockTimeHeight (fi n)
    else
        LockTimeStamp (fi n)

txLockTime :: TxPayload -> LockTime
txLockTime tx = numToLockTime (lock_time tx)

-- see if the given lock time is greater or equal to the expect lock time
-- given >= expect
isRelLockTimeAtLeast :: RelLockTime -> RelLockTime -> Bool
isRelLockTimeAtLeast (RelLockTimeHeight a) (RelLockTimeHeight b) = a >= b
isRelLockTimeAtLeast (RelLockTime512S a) (RelLockTime512S b) = a >= b
isRelLockTimeAtLeast _ _ = False -- not comparable

isLockTimeAtLeast :: LockTime -> LockTime -> Bool
isLockTimeAtLeast (LockTimeHeight a) (LockTimeHeight b) = a >= b
isLockTimeAtLeast (LockTimeStamp a) (LockTimeStamp b) = a >= b
isLockTimeAtLeast _ _ = False -- not comparable

isFinalTx :: TxPayload -> Bool
isFinalTx tx = all ((== maxBound) . seqn) (tx_in tx)

-- compareRelLockTime :: RelLockTime -> RelLockTime -> Maybe (Word32, Word32)
-- compareRelLockTime (RelLockTimeHeight a) (RelLockTimeHeight b) = Just (a, b)
-- compareRelLockTime (RelLockTime512S a) (RelLockTime512S b) = Just (a, b)
-- compareRelLockTime _ _ = Nothing

-- compareLockTime :: LockTime -> LockTime -> Maybe (Word32, Word32)
-- compareLockTime (LockTimeHeight a) (LockTimeHeight b) = Just (a, b)
-- compareLockTime (LockTimeStamp a) (LockTimeStamp b) = Just (a, b)
-- compareLockTime _ _ = Nothing

instance NFData TxInput where
    rnf (TxInput a b c) = rnf (a, b, c)

data TxOutput =
    TxOutput {
        value           :: Satoshi, -- in Satoshis, 10^-8 BTC
        pk_script       :: RawScript
    } deriving (Eq, Show)

instance NFData TxOutput where
    rnf (TxOutput a b) = rnf (a, b)

nullTxOutput = TxOutput { value = -1, pk_script = BSR.empty }

-- TxWitness is a list of stack items
newtype TxWitness = TxWitness [ByteString] deriving (Eq, Show)

nullWitness = TxWitness []

instance NFData TxWitness where
    rnf (TxWitness items) = rnf items

data TxPayload =
    TxPayload {
        txid        :: Hash256,
        wtxid       :: Hash256,

        version     :: Word32,
        -- flag        :: Word8, -- currently only 1(contains witness) or 0(nothing)
        
        tx_in       :: [TxInput],
        tx_out      :: [TxOutput],
        tx_witness  :: [TxWitness],

        lock_time   :: Word32  -- the earliest time the tx can be used
                               -- if lock_time < 500,000,000, treat it as a block height
                               -- if lock_time >= 500,000,000, treat it as an unix timestamp
        -- tx_cache    :: Maybe ByteString
    } deriving (Show)

instance Default TxPayload where
    def = TxPayload {
        txid = undefined,
        wtxid = undefined,
        version = undefined,
        tx_in = undefined,
        tx_out = undefined,
        tx_witness = undefined,
        lock_time = undefined
    }

instance Eq TxPayload where
    tx1 == tx2 = txid tx1 == txid tx2

instance Hashable TxPayload where
    hashWithSalt s t = hashWithSalt s (txid t)

instance NFData TxPayload where
    rnf (TxPayload txid wtxid version tx_in tx_out tx_witness lock_time) =
        rnf txid `seq`
        rnf wtxid `seq`
        rnf version `seq`
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
    encodeB end (OutPoint hash index) =
        encodeB end hash <> encodeB end index

instance Decodable OutPoint where
    decoder = do
        hash <- decoder
        index <- decoder
        return $ OutPoint hash index

instance Sizeable OutPoint where
    sizeOf _ = sizeOf (undefined :: Hash256) + sizeOf (undefined :: Word32)

instance Encodable TxInput where
    encodeB end (TxInput {
        prev_out = prev_out,
        sig_script = sig_script,
        seqn = seqn
    }) =
        e prev_out <>
        e (vbstr sig_script) <>
        e seqn
        where
            e :: Encodable t => t -> Builder
            e = encodeB end

instance Decodable TxInput where
    decoder = do
        prev_out <- decoder
        (VInt slen) <- decoder
        sig_script <- bsD $ fi slen
        seqn <- decoder

        return $ TxInput {
            prev_out = prev_out,
            sig_script = sig_script,
            seqn = seqn
        }

instance Sizeable TxInput where
    sizeOf (TxInput {
        prev_out = prev_out,
        sig_script = sig_script,
        seqn = seqn
    }) =
        sizeOf prev_out +
        sizeOf (vbstr sig_script) +
        sizeOf seqn

instance Encodable TxOutput where
    encodeB end (TxOutput {
        value = value,
        pk_script = pk_script
    }) =
        e value <> e (vbstr pk_script)
        where
            e :: Encodable t => t -> Builder
            e = encodeB end

instance Decodable TxOutput where
    decoder = do
        value <- decoder
        (VInt slen) <- decoder
        pk_script <- bsD $ fi slen
        return $ TxOutput {
            value = value,
            pk_script = pk_script
        }

instance Sizeable TxOutput where
    sizeOf (TxOutput {
        value = value,
        pk_script = pk_script
    }) =
        sizeOf value + sizeOf (vbstr pk_script)

instance Encodable TxWitness where
    encodeB end (TxWitness items) =
        encodeVListB end (map vbstr items)

instance Decodable TxWitness where
    decoder =
        vlistD decoder >>=
        return . TxWitness . map vstrToBS

instance Sizeable TxWitness where
    sizeOf (TxWitness items) =
        sizeOf (VInt (fi (length items))) +
        sum (map (sizeOf . vbstr) items)

instance MsgPayload TxPayload

instance Encodable TxPayload where
    -- encode end (TxPayload {
    --     tx_cache = Just cache
    -- }) = cache

    encodeB end (TxPayload {
        version = version,
        -- flag = flag, -- currently only 1 or 0
        
        tx_in = tx_in,
        tx_out = tx_out,
        tx_witness = tx_witness,

        lock_time = lock_time
    }) =
        mconcat [
            e version,
            
            if null tx_witness then
                mempty
            else
                bcharB 0 <> bcharB 1,

            encodeVListB end tx_in,
            encodeVListB end tx_out,

            if null tx_witness then mempty
            else encodeB end tx_witness, -- just an ordinary list, not vlist
            
            e lock_time
        ]
        where
            e :: Encodable t => t -> Builder
            e = encodeB end

instance Decodable TxPayload where
    decoder = do
        buf <- allD'
        init_len <- lenD

        version <- decoder

        mark <- peekByteD
        flag <-
            if mark == 0 then
                byteD >> byteD
            else return 0

        tx_in <- vlistD decoder
        tx_out <- vlistD decoder

        -- tx_witness is NOT a vlist because the size of it
        -- is implied by the size of tx_in
        tx_witness <-
            if flag /= 0 then
                listD (length tx_in) decoder
            else return []

        lock_time <- decoder

        final_len <- lenD

        return $ updateTx TxPayload {
            txid = nullHash256,
            wtxid = nullHash256,

            version = version,

            tx_in = tx_in,
            tx_out = tx_out,
            tx_witness = tx_witness,

            lock_time = lock_time

            -- tx_cache = Just $ BSR.take (init_len - final_len) buf
        }

instance Sizeable TxPayload where
    sizeOf (TxPayload {
        version = version,
        -- flag = flag, -- currently only 1 or 0
        
        tx_in = tx_in,
        tx_out = tx_out,
        tx_witness = tx_witness,

        lock_time = lock_time
    }) =
        sizeOf version +
        (if null tx_witness then 0 else 2) +
        sizeOf (VInt (fi (length tx_in))) +
        sizeOf tx_in +
        sizeOf (VInt (fi (length tx_out))) +
        sizeOf tx_out +
        sizeOf tx_witness +
        sizeOf lock_time

-- tx with only 1 input with block hash 0 and n -1
isCoinbase :: TxPayload -> Bool
isCoinbase (TxPayload {
    tx_in = [TxInput {
        prev_out = OutPoint nullHash256 (-1)
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
updateTx :: TxPayload -> TxPayload
updateTx tx =
    tx {
        txid = getTxId tx,
        wtxid = getWtxId tx
    }

-- get output value
getOutputValue :: TxPayload -> Satoshi
getOutputValue tx =
    sum (map value (tx_out tx))

getWitness :: TxPayload -> Int -> Maybe TxWitness
getWitness tx in_idx =
    if in_idx < length (tx_witness tx) then
        Just (tx_witness tx !! in_idx)
    else
        Nothing

hasWitness :: TxPayload -> Bool
hasWitness = not . null . tx_witness

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
        tx_witness = []
    }

-- raw tx to be signed

-- hashtype
-- 1. SIGHASH_ALL -> the current imp
-- 2. SIGHASH_NONE
-- 3. SIGHASH_SINGLE

-- 1. set all inputs except the current's sequence to 0 if NONE || SINGLE

{-

Double SHA256 of the serialization of:
    1. nVersion of the transaction (4-byte little endian)
    2. hashPrevouts (32-byte hash)
    3. hashSequence (32-byte hash)
    4. outpoint (32-byte hash + 4-byte little endian) 
    5. scriptCode of the input (serialized as scripts inside CTxOuts)
    6. value of the output spent by this input (8-byte little endian)
    7. nSequence of the input (4-byte little endian)
    8. hashOutputs (32-byte hash)
    9. nLocktime of the transaction (4-byte little endian)
   10. sighash type of the signature (4-byte little endian)

-}

txSigHashV0Wit :: TxPayload -> Int -> TxOutput -> HashType -> ByteString -> ByteString
txSigHashV0Wit tx in_idx outp htype script_code =
    let
        inp = tx_in tx !! in_idx

        hash_none = hasHashType htype SIGHASH_NONE
        hash_single = hasHashType htype SIGHASH_SINGLE
        hash_anyonecanpay = hasHashType htype SIGHASH_ANYONECANPAY

        sr_hash_prevouts =
            if not hash_anyonecanpay then
                doubleSHA256 $ mconcat (map (encodeLE . prev_out) (tx_in tx))
            else
                nullHash256BS

        sr_hash_sequence =
            if hash_anyonecanpay || hash_single || hash_none then
                nullHash256BS
            else
                doubleSHA256 $ mconcat (map (encodeLE . seqn) (tx_in tx))

        sr_hash_outputs =
            if not hash_single && not hash_none then
                doubleSHA256 $ encodeLE (tx_out tx)
            else if hash_single && in_idx < length (tx_out tx) then
                doubleSHA256 $ encodeLE (tx_out tx !! in_idx)
            else
                nullHash256BS

        sr_version = encodeLE (version tx)
        sr_outpoint = encodeLE (prev_out inp)
        sr_script_code = script_code
        sr_value = encodeLE (value outp)
        sr_sequence = encodeLE (seqn inp)
        sr_lock_time = encodeLE (lock_time tx)
        sr_hash_type = encodeLE (hashTypeToInt htype :: Word32)
    in
        -- tLn (show [ hash_none, hash_single, hash_anyonecanpay ])
        doubleSHA256 $ mconcat [
            sr_version,
            sr_hash_prevouts,
            sr_hash_sequence,
            sr_outpoint,
            sr_script_code,
            sr_value,
            sr_sequence,
            sr_hash_outputs,
            sr_lock_time,
            sr_hash_type
        ]

txSigHashLegacy :: TxPayload -> Int -> [ScriptOp] -> HashType -> ByteString
txSigHashLegacy tx in_idx subscript htype =
    let hash_none = hasHashType htype SIGHASH_NONE
        hash_single = hasHashType htype SIGHASH_SINGLE
        hash_anyonecanpay = hasHashType htype SIGHASH_ANYONECANPAY

        tx_copy = stripWitness $ tx {
                -- remove all sig_script
                tx_in = map (\inp -> inp {
                    sig_script = BSR.empty,
                    seqn = if hash_none || hash_single then 0 else seqn inp
                }) (tx_in tx)
            }

        raw_htype = hashTypeToInt htype :: Word32

        input = tx_in tx !! in_idx

        -- output = assertT "SIGHASH_SINGLE needs more outputs"
        --                  (length (tx_out tx) > idx)
        --                  (tx_out tx !! idx) -- ONLY used when hash_single
        invalid_sighash_single =
            hash_single && length (tx_out tx) <= in_idx

        output = tx_out tx !! in_idx

        new_input = input {
            -- replace the corresponding sig_script
            -- with pk_script after OP_CODESEPARATOR
            sig_script = encodeLE subscript
        }

        new_inputs =
            if hash_anyonecanpay then
                -- only the current input is included
                [ new_input ]
            else
                replace in_idx new_input (tx_in tx_copy)

        new_outputs =
            if hash_none then [] -- no output is included
            else if hash_single then
                -- only outputs from 0 to idx is included
                -- require length tx_out > idx
                map (const nullTxOutput) (take in_idx (tx_out tx))
                ++ [ output ]
            else
                tx_out tx -- original outputs

        final_tx = tx_copy {
            tx_in = new_inputs,
            tx_out = new_outputs
        }
    in
        if invalid_sighash_single then
            -- when ninput > noutput and sighash_single is declared
            -- the hash value 1 is signed
            encodeLE (intToHash256 (1 :: Int))
        else
            -- tLn (show htype) $
            -- tLn (show (hex $ encodeLE final_tx <> encodeLE raw_htype)) $
            -- tLn (show (hex $ sha256 $ sha256 $ encodeLE final_tx <> encodeLE raw_htype)) $
            doubleSHA256 $ encodeLE final_tx <> encodeLE raw_htype
        -- tLn (show (prev, final_tx, hex final_str)) $
        -- bsToHash256 $ sha256 $ sha256 final_str

encodeTxPayload :: TxPayload -> IO ByteString
encodeTxPayload = return . encodeLE

-- testBuildTx "5K31VmkAYGwaufdSF7osog9SmGNtzxX9ACsXMFrxJ1NsAmzkje9" [ OutPoint "81b4c832d70cb56ff957589752eb4125a4cab78a25a8fc52d6a09e5bd4404d48" 0 ] [ (10, "5K31VmkAYGwaufdSF7osog9SmGNtzxX9ACsXMFrxJ1NsAmzkje9") ]
-- testBuildTx "5HusYj2b2x4nroApgfvaSfKYZhRbKFH41bVyPooymbC6KfgSXdD" [ OutPoint (((!! 0) . unhex) "81b4c832d70cb56ff957589752eb4125a4cab78a25a8fc52d6a09e5bd4404d48") 0 ] [ (91234, "1KKKK6N21XKo48zWKuQKXdvSsCf95ibHFa") ]

-- total 1.3 btc = 130000000 satoshis
-- testBuildTx "933qtT8Ct7rGh29Eyb5gG69QrWmwGein85F1kuoShaGjJFFBSjk" [ OutPoint (((!! 0) . unhex) "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5") 0 ] [ (1000, "miro9ZNPjcLnqvnJpSm8P6CUf1WPU98jET"), (129899000, "mvU2ysD322amhCeCPMhPc3L7hKDGGWSBz7") ] -- tip 0.001

-- encodeTxPayload btc_testnet3 "933qtT8Ct7rGh29Eyb5gG69QrWmwGein85F1kuoShaGjJFFBSjk" [ OutPoint (((!! 0) . unhex) "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5") 0 ] [ (1000, "miro9ZNPjcLnqvnJpSm8P6CUf1WPU98jET"), (129899000, "mvU2ysD322amhCeCPMhPc3L7hKDGGWSBz7") ]

-- dehex v = case unhex v :: Maybe String of
--     Just str -> str
--     Nothing -> error "illegal hex"
