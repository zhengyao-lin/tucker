{-# LANGUAGE ForeignFunctionInterface #-}

-- SHA-256
-- RIPEMD 160
-- Base 58 Check
-- ECDSA

module Tucker.Crypto where

import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import qualified Data.ByteArray as BA

import Crypto.PubKey.ECC.ECDSA hiding (sign, verify)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA

import Crypto.Hash (hash, Digest)
import Crypto.Hash.Algorithms
import Crypto.PBKDF.ByteString
import qualified Crypto.MAC.HMAC as HMAC

-- import Data.ByteString.Base58
import Data.List
import Data.Hex
import Data.Word

import Foreign.Ptr

import System.IO.Unsafe

import Control.Monad.Loops

import Tucker.Enc
import Tucker.ECC
import Tucker.Conf
import Tucker.Util
import Tucker.ASN1
import Tucker.Error

foreign import ccall "sha256" c_sha256 :: Ptr Word8 -> Word64 -> Ptr Word8 -> IO ()
foreign import ccall "double_sha256" c_double_sha256 :: Ptr Word8 -> Word64 -> Ptr Word8 -> IO ()

ba2bs :: BA.ByteArrayAccess a => a -> ByteString
ba2bs = BA.convert

-- sha256 :: ByteString -> ByteString -- BA.ByteArrayAccess a => a -> Digest SHA256
-- sha256 dat = ba2bs (hash dat :: Digest SHA256)

sha256 :: ByteString -> ByteString
sha256 bs = unsafePerformIO (snd <$> res)
    where
        res =
            -- :: IO ((), ByteString)
            BA.withByteArray bs $ \pdat ->
                -- :: IO ((), ByteString)
                BA.allocRet 32 $ \phash ->
                    c_sha256 pdat (fi (BSR.length bs)) phash

-- doubleSHA256 :: ByteString -> ByteString
-- doubleSHA256 = sha256 . sha256

doubleSHA256 :: ByteString -> ByteString
doubleSHA256 bs = unsafePerformIO (snd <$> res)
    where
        res =
            -- :: IO ((), ByteString)
            BA.withByteArray bs $ \pdat ->
                -- :: IO ((), ByteString)
                BA.allocRet 32 $ \phash ->
                    c_double_sha256 pdat (fi (BSR.length bs)) phash

sha1 :: ByteString -> ByteString
sha1 dat = ba2bs (hash dat :: Digest SHA1)

ripemd160 :: ByteString -> ByteString
ripemd160 dat = ba2bs (hash dat :: Digest RIPEMD160)

hmacSHA512 :: ByteString -> ByteString -> ByteString
hmacSHA512 key msg = ba2bs (HMAC.hmac key msg :: HMAC.HMAC SHA512)

-- using HMAC-SHA256
pbkdf2 = sha512PBKDF2

base58_alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

encodeBase58 :: ByteString -> String
encodeBase58 raw =
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
        rawi = bs2vwordLE $ BS.reverse $ BS.drop numz raw

decodeBase58' :: Integer -> String -> Either TCKRError Integer
decodeBase58' cur [] = Right cur
decodeBase58' cur (c:str) = do
    let alphabet = base58_alphabet

    case findIndex (== c) alphabet of
        Just i -> decodeBase58' (cur * 58 + toInteger i) str
        Nothing -> Left $ TCKRError "illegal character in base58 encoding"

decodeBase58 :: String -> Either TCKRError ByteString
decodeBase58 enc = do
    res <- decodeBase58' (0 :: Integer) rest
    let res_enc = vword2bsLE res

    return $ BS.pack pref <> BS.reverse res_enc
    where
        numz = length $ takeWhile (== '1') enc
        pref = take numz $ repeat '\0'
        rest = drop numz enc

encodeBase58Check :: ByteString -> String
encodeBase58Check raw =
    encodeBase58 $ raw <> BS.take 4 digest
    where digest = sha256 $ sha256 raw

decodeBase58Check :: String -> Either TCKRError ByteString
decodeBase58Check enc = do
    dec <- decodeBase58 enc

    let len = BS.length dec
        check = BS.drop (len - 4) dec
        orig = BS.take (len - 4) dec
        digest = BS.take 4 $ sha256 $ sha256 orig

    if digest == check then
        pure orig
    else
        Left $ TCKRError "base58 decode check failed"

-- enc/dec of pub/sig
-- priv -> pub
-- priv -> wif
-- wif -> priv
-- pub -> addr

{-

3046

0221
002e6f0e8b515b5f25e837592e5e8a834cbe3fabaf98973edf88b19502e0180c2d

0221
00d03cc64f35fb277fe1b69270b542aca5620394ed7b7fae7a3546934dd6fe4288

-}

-- exportPriv Priv -> ByteString
-- importPriv ByteString -> Maybe Priv

-- exportPub Pub -> ByteString
-- importPub ByteString -> Maybe Pub

type WIF = String

privToWIF :: TCKRConf -> ECCPrivateKey -> WIF
privToWIF conf priv =
    let priv_raw = encodeAE priv
        priv_proc = BSR.cons (tckr_wif_pref conf) priv_raw
    in encodeBase58Check priv_proc

wifToPriv :: TCKRConf -> WIF -> Either TCKRError ECCPrivateKey
wifToPriv conf wif = do
    priv_proc <- decodeBase58Check wif
    
    if BSR.head priv_proc /= (tckr_wif_pref conf) then
        Left $ TCKRError "illegal WIF"
    else
        -- let priv_raw = BSR.drop 1 priv_proc in
        decodeAllBE (BSR.take 32 (BSR.drop 1 priv_proc))
        -- case importPriv priv_raw of
        --     Just priv -> Right priv
        --     Nothing -> Left "illegal private key format"

-- gen :: TCKRConf -> IO (WIF, String)
-- gen conf = do
--     (pub, priv) <- genRaw

--     let addr = pubToAddr conf pub
--         wif = privToWIF conf priv

--     return (wif, addr)
