{-# LANGUAGE FlexibleInstances #-}

module Tucker.Msg.Tx where

import Tucker.Enc
import Tucker.Conf
import Tucker.Auth
import Tucker.Error
import Tucker.Msg.Script
import Tucker.Msg.Common
import Tucker.Msg.Hash256

import Data.Hex
import Data.Int
import Data.Char
import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

data OutPoint = OutPoint Hash256 Word32 deriving (Eq, Show, Read)

type Value = Int64

data TxInput =
    TxInput {
        prev_out        :: OutPoint,
        sig_script      :: RawScript,
        seqn            :: Int32 -- sequence, currently not used
    } deriving (Eq, Show, Read)

data TxOutput =
    TxOutput {
        value           :: Value, -- in Satoshis, 10^-8 BTC
        pk_script       :: RawScript
    } deriving (Eq, Show, Read)

data TxWitness = TxWitness deriving (Eq, Show, Read)
    
data TxPayload =
    TxPayload {
        version     :: Int32,
        flag        :: Int16, -- currently only 1 or 0
        
        tx_in       :: [TxInput],
        tx_out      :: [TxOutput],
        tx_witness  :: [TxWitness],

        lock_time   :: Int32 -- the earliest time the tx can be used
                             -- if lock_time < 500,000,000, treat it as a block height
                             -- if lock_time >= 500,000,000, treat it as an unix timestamp
    } deriving (Eq, Show, Read)

data Wallet =
    Wallet {
        keypair :: ECCKeyPair
    }

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
            e (VInt $ fromIntegral $ BSR.length sig_script),
            e sig_script,
            e seqn
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable TxInput where
    decoder = do
        prev_out <- decoder
        (VInt slen) <- decoder
        sig_script <- bsD $ fromIntegral slen
        seqn <- decoder

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
            e (VInt $ fromIntegral $ BSR.length pk_script),
            e pk_script
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable TxOutput where
    decoder = do
        value <- decoder
        (VInt slen) <- decoder
        pk_script <- bsD $ fromIntegral slen
        return $ TxOutput {
            value = value,
            pk_script = pk_script
        }

instance Encodable TxWitness where
    encode _ _ = BSR.empty

instance Decodable TxWitness where
    decoder = return TxWitness

instance MsgPayload TxPayload

instance Encodable TxPayload where
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
            
            if flag == 0 then BSR.empty else e flag,

            encodeVList end tx_in,
            encodeVList end tx_out,
            
            e tx_witness,
            
            e lock_time
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable TxPayload where
    decoder = do
        version <- decoder

        -- TODO: ignoring flags here

        tx_in <- vlistD decoder
        tx_out <- vlistD decoder

        lock_time <- decoder

        return $ TxPayload {
            version = version,
            flag = 0,

            tx_in = tx_in,
            tx_out = tx_out,
            tx_witness = [],

            lock_time = lock_time
        }

-- tx with only 1 input with block hash 0 and n -1
isCoinbase :: TxPayload -> Bool
isCoinbase (TxPayload {
    tx_in = [TxInput {
        prev_out = OutPoint 0 (-1)
    }]
}) = True

isCoinbase _ = False

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

-- generate a standard public key script
stdPkScript :: TCKRConf
            -> Address {- alas String, base58check encoded -}
            -> Either TCKRError ByteString
stdPkScript conf addr = do
    (_, pub_hash) <- addr2pubhash conf addr
    return $ encodeLE [
            OP_DUP,
            OP_HASH160,
            OP_PUSHDATA pub_hash,
            OP_EQUALVERIFY,
            OP_CHECKSIG
        ]

-- sign a raw transaction
signRawTx :: ECCKeyPair -> TxPayload -> IO ByteString
signRawTx pair tx = do
    let
        raw = encodeLE tx
        -- the first sha256 performed here
        hash_raw = ba2bs $ sha256 $ raw <> BSR.pack [ 0x01, 0x00, 0x00, 0x00 ]

    -- another sha256 is performed here
    seq (trace (show $ sha256 $ sha256 raw) 0) $ signSHA256DER pair hash_raw

stdSigScript :: ECCKeyPair -> ByteString -> ByteString
stdSigScript pair sign =
    encodeLE [
        OP_PUSHDATA $ sign <> bchar 0x01,
        OP_PUSHDATA $ pair2pubenc pair
    ]

-- create a standard transaction using the key pair, input, and output given
-- assuming the previous transaction is also standard, so we can generate the
-- pk_script without pulling it from elsewhere
stdTx :: TCKRConf -> ECCKeyPair -> [OutPoint] -> [(Value, Address)] -> Either TCKRError (IO TxPayload)
stdTx conf pair input output = do
    let
        self_addr = pair2addr conf pair

    in_lst <- mapM (\outp -> do
        -- assuming the output point has a standard script
        script <- stdPkScript conf self_addr
        return TxInput {
            prev_out = outp,
            sig_script = script,
            seqn = -1
        }) input

    out_lst <- mapM (\(v, a) -> do
        script <- stdPkScript conf a
        return TxOutput {
            value = v,
            pk_script = script
        }) output

    let
        wrap in_lst out_lst = TxPayload {
            version = 1,
            flag = 0,

            tx_in = in_lst,
            tx_out = out_lst,

            tx_witness = [],
            lock_time = 0
        }

        raw = wrap in_lst out_lst

    return $ do -- IO
        sign <- signRawTx pair raw
        let script = stdSigScript pair sign
        
        -- replace all sig_script

        new_in_lst <- mapM (\outp -> do
            return TxInput {
                prev_out = outp,
                sig_script = script,
                seqn = -1
            }) input
        
        return $ wrap new_in_lst out_lst

unpackEither :: Either TCKRError a -> a
unpackEither (Right v) = v
unpackEither (Left err) = error $ show err

buildTxPayload :: TCKRConf -> WIF -> [OutPoint] -> [(Value, Address)] -> IO TxPayload
buildTxPayload conf wif in_lst out_lst =
    unpackEither $ stdTx conf pair in_lst out_lst
    where pair = unpackEither $ wif2pair conf wif

encodeTxPayload :: TCKRConf -> WIF -> [OutPoint] -> [(Value, Address)] -> IO ByteString
encodeTxPayload conf wif in_lst out_lst = do
    tx <- buildTxPayload conf wif in_lst out_lst
    return $ encodeLE tx

-- testBuildTx "5K31VmkAYGwaufdSF7osog9SmGNtzxX9ACsXMFrxJ1NsAmzkje9" [ OutPoint "81b4c832d70cb56ff957589752eb4125a4cab78a25a8fc52d6a09e5bd4404d48" 0 ] [ (10, "5K31VmkAYGwaufdSF7osog9SmGNtzxX9ACsXMFrxJ1NsAmzkje9") ]
-- testBuildTx "5HusYj2b2x4nroApgfvaSfKYZhRbKFH41bVyPooymbC6KfgSXdD" [ OutPoint (((!! 0) . unhex) "81b4c832d70cb56ff957589752eb4125a4cab78a25a8fc52d6a09e5bd4404d48") 0 ] [ (91234, "1KKKK6N21XKo48zWKuQKXdvSsCf95ibHFa") ]

-- total 1.3 btc = 130000000 satoshis
-- testBuildTx "933qtT8Ct7rGh29Eyb5gG69QrWmwGein85F1kuoShaGjJFFBSjk" [ OutPoint (((!! 0) . unhex) "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5") 0 ] [ (1000, "miro9ZNPjcLnqvnJpSm8P6CUf1WPU98jET"), (129899000, "mvU2ysD322amhCeCPMhPc3L7hKDGGWSBz7") ] -- tip 0.001

-- encodeTxPayload btc_testnet3 "933qtT8Ct7rGh29Eyb5gG69QrWmwGein85F1kuoShaGjJFFBSjk" [ OutPoint (((!! 0) . unhex) "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5") 0 ] [ (1000, "miro9ZNPjcLnqvnJpSm8P6CUf1WPU98jET"), (129899000, "mvU2ysD322amhCeCPMhPc3L7hKDGGWSBz7") ]

-- 0100000001B50418BBEA2416B4C9CED52DCE85FFCA7B486668BD69BBC7C34102E12F82B7BE000000008C4930460221009273528BBBDFF9952604BB495D1E0379B62719B5ADA94F128956CD59B158C32F022100A0CD8FAF9DF0923CBAC413D9379E3ED2B92EF56D2927C300672969E6460452BB014104F789605ECABF791B719B4D0AA911E4EF80010904AA32E37C2B7BF427E6BC2ED40CC21568E7C5AED188E58CF7CF25B3C540FC8B3D20EEC49D967416D755944740FFFFFFFF02E8030000000000001976A91424A90FBE7E852F1C233CFABA9E473F801A5E790A88ACF819BE07000000001976A914A3FC8D07B59B4137BFEE2D4E0CF940A3B656B50C88AC00000000

-- output

-- 01000000
-- 01

-- output hash
-- 484D40D45B9EA0D652FCA8258AB7CAA42541EB52975857F96FB50CD732C8B481
-- output index
-- 00000000

-- 8B
-- 48
-- 30
-- 45
-- 02
-- 20
-- 47AC97D5B0D5BA90A62ADDC49A02A049EFAF9AF8B1B913D2F45504A6B7EDA4C8
-- 02
-- 21
-- 00A43C08DC716628C4E5847996585E16E80F7C2A3FFE9321FF51F0E2319192021B

-- 01
-- 41
-- 04
-- 14E301B2328F17442C0B8310D787BF3D8A404CFBD0704F135B6AD4B2D3EE7513
-- 10F981926E53A6E8C39BD7D3FEFD576C543CCE493CBAC06388F2651D1AACBFCD

-- FFFFFFFF

-- 01
-- 6264010000000000

-- 1A

-- -- pk_script
-- 76A914C8E90996C7C6080EE06284600C684ED904D14C5C88AC

-- 00000000

-- dehex v = case unhex v :: Maybe String of
--     Just str -> str
--     Nothing -> error "illegal hex"

-- test = TxPayload {
--     version = 1,
--     flag = 0,

--     tx_in = [
--         TxInput {
--             prev_out =
--                 OutPoint
--                     (dehex "81b4c832d70cb56ff957589752eb4125a4cab78a25a8fc52d6a09e5bd4404d48")
--                     0,

--             sig_script = BSR.empty,
--             seqn = -1
--         }
--     ],

--     tx_out = [
--         TxOutput {
--             value = 123,
--             pk_script = BSR.empty
--         }
--     ],

--     tx_witness = [],

--     lock_time = 0
-- }
