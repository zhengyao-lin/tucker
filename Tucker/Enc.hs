
-- SHA-256
-- RIPEMD 160
-- Base 58 Check
-- ECDSA

module Tucker.Enc where

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

import Debug.Trace

import Control.Monad.Loops

import Tucker.Int

sha256 :: BA.ByteArrayAccess a => a -> Digest SHA256
sha256 = hash

ripemd160 :: BA.ByteArrayAccess a => a -> Digest RIPEMD160
ripemd160 = hash

ba2bs :: BA.ByteArrayAccess a => a -> ByteString
ba2bs = BSR.pack . BA.unpack

base58'alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

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
        alphabet = base58'alphabet
        numz = BSR.length $ BSR.takeWhile (== 0) raw
        pref = take numz $ repeat '1'
        rawi = bs2vint'le $ BS.reverse $ BS.drop numz raw

base58dec' :: Integer -> String -> Maybe Integer
base58dec' cur [] = Just cur
base58dec' cur (c:str) = do
    i <- findIndex (== c) alphabet
    base58dec' (cur * 58 + toInteger i) str
    where
        alphabet = base58'alphabet

base58dec :: ByteString -> Maybe ByteString
base58dec enc = do
    res <- base58dec' (0 :: Integer) rest
    return $ BS.append (BS.pack pref) (BS.reverse $ vint2bs'le res)
    where
        str = BS.unpack enc
        numz = length $ takeWhile (== '1') str
        pref = take numz $ repeat '\0'
        rest = drop numz str

base58enc'check :: ByteString -> ByteString
base58enc'check raw =
    base58enc (BS.append raw (BS.take 4 digest))
    where
        digest = ba2bs $ sha256 $ sha256 raw

base58dec'check :: ByteString -> Maybe ByteString
base58dec'check enc = do
    dec <- base58dec enc
    let
        len = BS.length dec
        check = BS.drop (len - 4) dec
        orig = BS.take (len - 4) dec
        digest = BS.take 4 $ ba2bs $ sha256 $ sha256 orig

    if digest == check then
        pure orig
    else
        Nothing

bitcoinCurve = getCurveByName SEC_p256k1

type WIF = String
type Address = String

data ECCKeyPair = ECCKeyPair Integer Point deriving (Show, Read, Eq)

priv2wif :: Integer -> WIF
priv2wif num =
    BS.unpack $ base58enc'check priv'proc
    where
        priv'raw = vint2bs'le num
        priv'proc = BSR.append (BSR.singleton 0x80) priv'raw

wif2priv :: WIF -> Maybe Integer
wif2priv wif = do
    priv'proc <- base58dec'check $ BS.pack wif
    
    if BSR.head priv'proc /= 0x80 then
        Nothing
    else do
        let priv'raw = BSR.drop 1 priv'proc
        return $ bs2vint'le priv'raw

format'Point :: Point -> ByteString
format'Point (Point x y) =
    BSR.concat [ BSR.singleton 0x04, x'raw, y'raw ]
    where
        x'raw = encodeInt 4 BigEndian x
        y'raw = encodeInt 4 BigEndian y

pub2addr :: Point -> Address
pub2addr pt =
    BS.unpack $ base58enc'check pub'hash
    where
        pub'raw = format'Point pt
        pub'hash = BSR.append (BSR.singleton 0x00) $ ba2bs $ ripemd160 $ sha256 pub'raw

priv2pub :: Integer -> Point
priv2pub = generateQ bitcoinCurve

wif2addr :: WIF -> Maybe Address
wif2addr wif = do
    priv <- wif2priv wif
    return $ pub2addr $ priv2pub priv

wif2pair :: WIF -> Maybe ECCKeyPair
wif2pair wif = do
    priv <- wif2priv wif
    return $ ECCKeyPair priv (priv2pub priv)

genRaw :: IO (PublicKey, PrivateKey)
genRaw = generate bitcoinCurve

gen :: IO (WIF, Address)
gen = do
    (PublicKey _ pt, PrivateKey _ num) <- genRaw

    let wif = priv2wif num
        addr = pub2addr pt
    
    return (wif, addr)

-- generate with condition on the (wif, address)
genCond :: ((WIF, Address) -> Bool) -> IO (WIF, Address)
genCond cond = iterateUntil cond gen

-- encode a signature using ASN1 by the following structure
-- SEQUENCE { r INTEGER, s INTEGER }
         -- r        s
signenc :: Signature -> ByteString
signenc (Signature r s) =
    encodeASN1' DER [ Start Sequence, IntVal r, IntVal s, End Sequence ]

signdec :: ByteString -> Maybe Signature
signdec str =
    case decodeASN1' DER str of
        Right ((Start Sequence):(IntVal r):(IntVal s):(End Sequence):[])
            -> Just (Signature r s)
        _ -> Nothing

-- hash & sign
sign'sha256 :: ECCKeyPair -> ByteString -> IO Signature
sign'sha256 (ECCKeyPair priv _) =
    sign privk SHA256
    where privk = PrivateKey bitcoinCurve priv

verify'sha256 :: ECCKeyPair -> ByteString -> Signature -> Bool
verify'sha256 (ECCKeyPair _ pub) msg sign =
    verify SHA256 pubk sign msg
    where pubk = PublicKey bitcoinCurve pub

sign'sha256'der :: ECCKeyPair -> ByteString -> IO ByteString
sign'sha256'der pair msg = do
    sign <- sign'sha256 pair msg
    return $ signenc sign

verify'sha256'der :: ECCKeyPair -> ByteString -> ByteString -> Maybe Bool
verify'sha256'der pair msg sign' = do
    sign <- signdec sign'
    return $ verify'sha256 pair msg sign
