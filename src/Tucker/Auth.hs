{-# LANGUAGE ForeignFunctionInterface #-}

-- SHA-256
-- RIPEMD 160
-- Base 58 Check
-- ECDSA

module Tucker.Auth where

import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import qualified Data.ByteArray as BA

import Crypto.Error
import Crypto.PubKey.ECC.P256
import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.Generate

import Crypto.PubKey.ECC.ECDSA hiding (sign, verify)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA

import Crypto.Hash (hash, Digest)
import Crypto.Hash.NoHash
import Crypto.Hash.Algorithms

import Data.ASN1.Types
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

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
        rawi = bs2vwordLE $ BS.reverse $ BS.drop numz raw

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
    let res_enc = vword2bsLE res

    return $ BS.pack pref <> BS.reverse res_enc
    where
        str = BS.unpack enc
        numz = length $ takeWhile (== '1') str
        pref = take numz $ repeat '\0'
        rest = drop numz str

base58encCheck :: ByteString -> ByteString
base58encCheck raw =
    base58enc $ raw <> BS.take 4 digest
    where digest = sha256 $ sha256 raw

base58decCheck :: ByteString -> Either TCKRError ByteString
base58decCheck enc = do
    dec <- base58dec enc
    let
        len = BS.length dec
        check = BS.drop (len - 4) dec
        orig = BS.take (len - 4) dec
        digest = BS.take 4 $ sha256 $ sha256 orig

    if digest == check then
        pure orig
    else
        Left $ TCKRError "base58dec check failed"

type WIF = String
type Address = String

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

privToWIF :: TCKRConf -> ECCPrivateKey -> WIF
privToWIF conf priv =
    let priv_raw = encodeBE priv
        priv_proc = BSR.cons (tckr_wif_pref conf) priv_raw
    in BS.unpack $ base58encCheck priv_proc

wifToPriv :: TCKRConf -> WIF -> Either TCKRError ECCPrivateKey
wifToPriv conf wif = do
    priv_proc <- base58decCheck $ BS.pack wif
    
    if BSR.head priv_proc /= (tckr_wif_pref conf) then
        Left $ TCKRError "illegal WIF"
    else
        -- let priv_raw = BSR.drop 1 priv_proc in
        decodeAllBE (BSR.drop 1 priv_proc)
        -- case importPriv priv_raw of
        --     Just priv -> Right priv
        --     Nothing -> Left "illegal private key format"

pubToAddr :: TCKRConf -> ECCPublicKey -> Address
pubToAddr conf pub =
    BS.unpack $ base58encCheck pub_hash
    where
        pub_raw = encodeBE pub
                            -- main TCKRConf byte
        pub_hash = BSR.cons (tckr_addr_pref conf) $ ripemd160 $ sha256 pub_raw

addrToPubHash :: TCKRConf -> Address -> Either TCKRError ByteString
addrToPubHash conf addr = do
    pub_hash_raw <- base58decCheck $ BS.pack addr

    if BSR.head pub_hash_raw /= tckr_addr_pref conf then
        Left $ TCKRError "illegal address"
    else
        return (BSR.drop 1 pub_hash_raw)

-- gen :: TCKRConf -> IO (WIF, Address)
-- gen conf = do
--     (pub, priv) <- genRaw

--     let addr = pubToAddr conf pub
--         wif = privToWIF conf priv

--     return (wif, addr)
