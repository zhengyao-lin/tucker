
-- SHA-256
-- RIPEMD 160
-- Base 58 Check
-- ECDSA

module Tucker.Auth where

import Crypto.Hash (hash, Digest)
import Crypto.Hash.Algorithms

import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import qualified Data.ByteArray as BA

import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.ECC.Types

import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Types

-- import Data.ByteString.Base58
import Data.List
import Data.Hex
import Data.Word

import Debug.Trace

import Control.Monad.Loops

import Tucker.Enc
import Tucker.Std
import Tucker.Error

sha256 :: BA.ByteArrayAccess a => a -> Digest SHA256
sha256 = hash

ripemd160 :: BA.ByteArrayAccess a => a -> Digest RIPEMD160
ripemd160 = hash

ba2bs :: BA.ByteArrayAccess a => a -> ByteString
ba2bs = BSR.pack . BA.unpack

base58_alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

base58enc :: ByteString -> ByteString
base58enc raw =
    BS.pack $
    (pref ++) $
    reverse $
    map (\(_, m) -> alphabet !! (fromInteger m)) $
    takeWhile (\(a, b) -> a /= 0 || b /= 0) $
    drop 1 $
    iterate (\(i, m) -> i `divMod` 58) (rawi, 0)
    where
        alphabet = base58_alphabet
        numz = BSR.length $ BSR.takeWhile (== 0) raw
        pref = take numz $ repeat '1'
        rawi = bs2vintLE $ BS.reverse $ BS.drop numz raw

base58dec' :: Integer -> String -> Either TCKRError Integer
base58dec' cur [] = Right cur
base58dec' cur (c:str) = do
    let alphabet = base58_alphabet

    case findIndex (== c) alphabet of
        Just i -> base58dec' (cur * 58 + toInteger i) str
        Nothing -> Left $ TCKRError "illegal character in base58 encoding"

base58dec :: ByteString -> Either TCKRError ByteString
base58dec enc = do
    res <- base58dec' (0 :: Integer) rest
    res_enc <- vint2bsLE res

    return $ BS.append (BS.pack pref) (BS.reverse res_enc)
    where
        str = BS.unpack enc
        numz = length $ takeWhile (== '1') str
        pref = take numz $ repeat '\0'
        rest = drop numz str

base58encCheck :: ByteString -> ByteString
base58encCheck raw =
    base58enc (BS.append raw (BS.take 4 digest))
    where
        digest = ba2bs $ sha256 $ sha256 raw

base58decCheck :: ByteString -> Either TCKRError ByteString
base58decCheck enc = do
    dec <- base58dec enc
    let
        len = BS.length dec
        check = BS.drop (len - 4) dec
        orig = BS.take (len - 4) dec
        digest = BS.take 4 $ ba2bs $ sha256 $ sha256 orig

    if digest == check then
        pure orig
    else
        Left $ TCKRError "base58dec check failed"

type WIF = String
type Address = String

data ECCKeyPair = ECCKeyPair Integer Point deriving (Show, Read, Eq)

-- for testnet
-- prefix in wif2priv and prev2wif
-- prefix in pub2addr

priv2wif :: BTCNetwork -> Integer -> Either TCKRError WIF
priv2wif net num = do
    priv_raw <- vint2bsBE num
    let priv_proc = BSR.cons (wifPref net) priv_raw

    return $ BS.unpack $ base58encCheck priv_proc

wif2priv :: BTCNetwork -> WIF -> Either TCKRError Integer
wif2priv net wif = do
    priv_proc <- base58decCheck $ BS.pack wif
    
    if BSR.head priv_proc /= (wifPref net) then
        Left $ TCKRError "illegal WIF"
    else do
        let priv_raw = BSR.drop 1 priv_proc
        return $ bs2vintBE priv_raw

pub2enc :: Point -> ByteString
pub2enc (Point x y) =
    BSR.concat [ BSR.singleton 0x04, x_raw, y_raw ]
    where
        x_raw = encodeInt 32 BigEndian x
        y_raw = encodeInt 32 BigEndian y

enc2pub :: ByteString -> Either TCKRError Point
enc2pub str =
    if BSR.length str == 0 || BSR.head str /= 0x04 then
        Left $ TCKRError "illegal ECC public key encoding"
    else do
        let x_raw = BSR.drop 1 $ BSR.take 33 str
            y_raw = BSR.drop 33 str
            x = decodeInt 32 BigEndian x_raw
            y = decodeInt 32 BigEndian y_raw

        return $ Point x y
        
pub2addr :: BTCNetwork -> Point -> Address
pub2addr net pt =
    BS.unpack $ base58encCheck pub_hash
    where
        pub_raw = pub2enc pt
                         -- main BTCNetwork byte
        pub_hash = BSR.cons (pubPref net) $ ba2bs $ ripemd160 $ sha256 pub_raw

addr2pubhash :: BTCNetwork -> Address -> Either TCKRError (Word8, ByteString)
addr2pubhash net addr = do
    pub_hash_raw <- base58decCheck $ BS.pack addr

    if BSR.head pub_hash_raw /= pubPref net then
        Left $ TCKRError "illegal address"
    else do
        let
            pub_type = BSR.index pub_hash_raw 0
            pub_hash = BSR.drop 1 pub_hash_raw

        return (pub_type, pub_hash)

priv2pub :: Integer -> Point
priv2pub = generateQ btc_curve

wif2addr :: BTCNetwork -> WIF -> Either TCKRError Address
wif2addr net wif = do
    priv <- wif2priv net wif
    return $ pub2addr net $ priv2pub priv

wif2pair :: BTCNetwork -> WIF -> Either TCKRError ECCKeyPair
wif2pair net wif = do
    priv <- wif2priv net wif
    return $ ECCKeyPair priv (priv2pub priv)

pair2pubenc :: ECCKeyPair -> ByteString
pair2pubenc (ECCKeyPair _ pub) = pub2enc pub

pair2addr :: BTCNetwork -> ECCKeyPair -> Address
pair2addr net (ECCKeyPair _ pub) = pub2addr net pub

genRaw :: IO (PublicKey, PrivateKey)
genRaw = generate btc_curve

gen :: BTCNetwork -> IO (Either TCKRError (WIF, Address))
gen net = do
    (PublicKey _ pt, PrivateKey _ num) <- genRaw

    return (do
        let addr = pub2addr net pt
        wif <- priv2wif net num
        return (wif, addr))

-- generate with condition on the (wif, address)
genCond :: BTCNetwork
        -> ((WIF, Address) -> Bool)
        -> IO (Either TCKRError (WIF, Address))
genCond net cond =
    iterateUntil (\res ->
        case res of
            Right ans -> cond ans
            other -> True) (gen net)

-- encode a signature using ASN1 by the following structure
-- SEQUENCE { r INTEGER, s INTEGER }
         -- r        s
signenc :: Signature -> ByteString
signenc (Signature r s) =
    encodeASN1' DER [ Start Sequence, IntVal r, IntVal s, End Sequence ]

signdec :: ByteString -> Either TCKRError Signature
signdec str =
    case decodeASN1' DER str of
        Right ((Start Sequence):(IntVal r):(IntVal s):(End Sequence):[])
            -> Right (Signature r s)
        _ -> Left $ TCKRError "illegal DER encoding"

-- hash & sign
signSHA256 :: ECCKeyPair -> ByteString -> IO Signature
signSHA256 (ECCKeyPair priv _) =
    sign privk SHA256
    where privk = PrivateKey btc_curve priv

verifySHA256 :: ECCKeyPair -> ByteString -> Signature -> Bool
verifySHA256 (ECCKeyPair _ pub) msg sign =
    verify SHA256 pubk sign msg
    where pubk = PublicKey btc_curve pub

signSHA256DER :: ECCKeyPair -> ByteString -> IO ByteString
signSHA256DER pair msg = do
    sign <- signSHA256 pair msg
    return $ signenc sign

verifySHA256DER :: ECCKeyPair -> ByteString -> ByteString -> Either TCKRError Bool
verifySHA256DER pair msg sign_enc = do
    sign <- signdec sign_enc
    return $ verifySHA256 pair msg sign

-- error decoding 0caecf01d74102a28aed6a64dcf1cf7b0e41c4dd6c62f70f46febdc32514f0bd
