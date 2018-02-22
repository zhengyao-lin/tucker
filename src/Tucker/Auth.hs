
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

import Crypto.Error
import Crypto.PubKey.ECC.P256
import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Generate

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
import Tucker.Conf
import Tucker.Util
import Tucker.Error

ba2bs :: BA.ByteArrayAccess a => a -> ByteString
ba2bs = BSR.pack . BA.unpack

sha256 :: BA.ByteArrayAccess a => a -> Digest SHA256
sha256 = hash

ripemd160 :: BA.ByteArrayAccess a => a -> Digest RIPEMD160
ripemd160 = hash

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

    return $ BS.pack pref <> BS.reverse res_enc
    where
        str = BS.unpack enc
        numz = length $ takeWhile (== '1') str
        pref = take numz $ repeat '\0'
        rest = drop numz str

base58encCheck :: ByteString -> ByteString
base58encCheck raw =
    base58enc $ raw <> BS.take 4 digest
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

data ECCPrivateKey = ECCPrivateKey Integer deriving (Eq, Show)
data ECCPublicKey = ECCPublicKey {
        compressed :: Bool,
        x_coord    :: Integer,
        y_coord    :: Integer
    } | ECCPrivatePointO deriving (Eq, Show)
data ECCSignature = ECCSignature Integer Integer deriving (Eq, Show)

type WIF = String
type Address = String

-- enc/dec of pub/sig
-- priv -> pub
-- priv -> wif
-- wif -> priv
-- pub -> addr

instance Encodable ECCPublicKey where
    -- q = 2^256
    -- m = 256
    -- finite field 𝔽p
    encode _ (ECCPublicKey comp x y) =
        if comp then
            case tucker_curve of
                CurveFP (CurvePrime p curve) ->
                    BSR.cons
                        (if y `mod` 2 == 0 then 0x02 else 0x03)
                        (encodeInt 32 BigEndian x)

                _ -> error "unsupported curve type"
        else
            BSR.cons 0x04
                (encodeInt 32 BigEndian x <>
                 encodeInt 32 BigEndian y)

    encode _ ECCPrivatePointO = bchar 0x00

instance Decodable ECCPublicKey where
    decoder = do
        i <- byteD

        if i == 0x04 then do
            x <- bsD 32
            y <- bsD 32
            return $ ECCPublicKey False (bs2vintBE x) (bs2vintBE y)
        else if i == 0x00 then
            return ECCPrivatePointO
        else do
            y_bit <- case i of
                0x02 -> return 0
                0x03 -> return 1
                _    -> fail "illegal initial byte"
            
            x <- bs2vintBE <$> bsD 32

            y <- case tucker_curve of
                CurveFP (CurvePrime p curve) -> do
                    let a = ecc_a curve
                        b = ecc_b curve
                        alpha = mod (x ^ 3 + a * x + b) p
                    
                    case modSqrt alpha p of
                        Just beta ->
                            if beta `mod` 2 == y_bit then
                                return beta
                            else
                                return (p - beta)

                        Nothing -> fail "failed to find a solution"

                _ -> fail "unsupported curve type"

            return $ ECCPublicKey True x y

instance Encodable ECCSignature where
    -- encode a signature using ASN1 by the following structure
    -- SEQUENCE { r INTEGER, s INTEGER }
    encode _ (ECCSignature r s) =
        encodeASN1' DER [ Start Sequence, IntVal r, IntVal s, End Sequence ]

instance Decodable ECCSignature where
    decoder = do
        all <- allD
        case decodeASN1' DER all of
            Right ((Start Sequence):(IntVal r):(IntVal s):(End Sequence):[])
                -> return $ ECCSignature r s
            Left err -> fail ("illegal DER encoding: " ++ show err)

priv2pub :: ECCPrivateKey -> ECCPublicKey
priv2pub (ECCPrivateKey num) =
    let Point x y = generateQ tucker_curve num in
    ECCPublicKey True x y

priv2wif :: TCKRConf -> ECCPrivateKey -> Either TCKRError WIF
priv2wif conf (ECCPrivateKey num) = do
    priv_raw <- vint2bsBE num
    let priv_proc = BSR.cons (tckr_wif_pref conf) priv_raw

    return $ BS.unpack $ base58encCheck priv_proc

wif2priv :: TCKRConf -> WIF -> Either TCKRError ECCPrivateKey
wif2priv conf wif = do
    priv_proc <- base58decCheck $ BS.pack wif
    
    if BSR.head priv_proc /= (tckr_wif_pref conf) then
        Left $ TCKRError "illegal WIF"
    else do
        let priv_raw = BSR.drop 1 priv_proc
        return $ ECCPrivateKey $ bs2vintBE priv_raw

-- using default compressing encoding
pub2addr :: TCKRConf -> ECCPublicKey -> Address
pub2addr conf pub =
    BS.unpack $ base58encCheck pub_hash
    where
        pub_raw = encodeBE pub
                            -- main TCKRConf byte
        pub_hash = BSR.cons (tckr_pub_pref conf) $ ba2bs $ ripemd160 $ sha256 pub_raw

addr2hash :: TCKRConf -> Address -> Either TCKRError ByteString
addr2hash conf addr = do
    pub_hash_raw <- base58decCheck $ BS.pack addr

    if BSR.head pub_hash_raw /= tckr_pub_pref conf then
        Left $ TCKRError "illegal address"
    else
        return (BSR.drop 1 pub_hash_raw)

genRaw :: IO (ECCPublicKey, ECCPrivateKey)
genRaw = do
    (PublicKey _ pt, PrivateKey _ num) <- generate tucker_curve
    let Point x y = pt
    return (ECCPublicKey True x y, ECCPrivateKey num)

gen :: TCKRConf -> IO (Either TCKRError (WIF, Address))
gen conf = do
    (pub, priv) <- genRaw

    return $ do
        let addr = pub2addr conf pub
        wif <- priv2wif conf priv
        return (wif, addr)

-- hash & sign
signSHA256 :: ECCPrivateKey -> ByteString -> IO ECCSignature
signSHA256 (ECCPrivateKey num) msg = do
    let privk = PrivateKey tucker_curve num
    Signature r s <- sign privk SHA256 msg
    return $ ECCSignature r s

verifySHA256 :: ECCPublicKey -> ByteString -> ECCSignature -> Bool
verifySHA256 (ECCPublicKey _ x y) msg (ECCSignature r s) =
    verify SHA256 pubk (Signature r s) msg
    where pubk = PublicKey tucker_curve (Point x y)

signSHA256DER :: ECCPrivateKey -> ByteString -> IO ByteString
signSHA256DER priv msg = do
    sig <- signSHA256 priv msg
    return $ encodeBE sig

verifySHA256DER :: ECCPublicKey -> ByteString -> ByteString -> Either TCKRError Bool
verifySHA256DER pub msg sig_enc = do
    sig <- decodeAllBE sig_enc
    return $ verifySHA256 pub msg sig
