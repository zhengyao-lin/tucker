-- implementation for BIP 32 Hierarchical Deterministic Wallets

module Tucker.Wallet.HD where

import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.ECC
import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Error
import Tucker.Crypto

type ChainCode = ByteString
type FingerPrint = ByteString

data HDKey
    = HDPrivateKey Int Word32 FingerPrint ECCPrivateKey ChainCode
    | HDPublicKey Int Word32 FingerPrint ECCPublicKey ChainCode

isHardened :: Word32 -> Bool
isHardened i = i >= 2 ^ 31

toFingerPrint :: ECCPublicKey -> FingerPrint
toFingerPrint = BSR.take 4 . ripemd160 . sha256 . encodeAE

nullFingerPrint = BSR.pack [ 0, 0, 0, 0 ]

toChildKey :: Word32 -> HDKey -> Either TCKRError HDKey

-- parent priv -> child priv
toChildKey i (HDPrivateKey depth _ _ priv code) =
    let pub = privToPub priv
        
        l = if isHardened i then
                hmacSHA512 code (bchar 0 <> encodeAE priv <> encodeBE i)
            else
                hmacSHA512 code (encodeAE pub <> encodeBE i)
        
        ll = decodeVWord BigEndian (BSR.take 32 l)
        lr = BSR.drop 32 l
        ki = ll + privToInt priv
        fp = toFingerPrint pub
    in do
        assertMT "left int >= order" (ll < paramN)
        assertMT "zero derived" (ki /= 0)

        return (HDPrivateKey (depth + 1) i fp (intToPriv ki) lr)

-- parent pub -> parent pub
toChildKey i (HDPublicKey depth _ _ pub code) =
    if isHardened i then fail "unable to derive from hardened keys"
    else let
        l = hmacSHA512 code (encodeAE pub <> encodeBE i)
        ll = decodeVWord BigEndian (BSR.take 32 l)
        lr = BSR.drop 32 l
        p0 = pubToPoint pub
    in do
        let fp = toFingerPrint pub
            ki = pointAdd (pointMul ll paramG) p0

        assertMT "left int >= order" (ll < paramN)
        pub' <- pointToPub ki

        return (HDPublicKey (depth + 1) i fp pub' lr)

toPublicKey (HDPrivateKey d i f priv code) = HDPublicKey d i f (privToPub priv) code

commonHeader pref d i f code =
    pref <> bchar (fi d) <> f <> encodeBE i <> code

serializeKey :: TCKRConf -> HDKey -> String

serializeKey conf (HDPrivateKey d i f priv code) =
    encodeBase58Check $
    commonHeader (tckr_hd_priv_key_prefix conf) d i f code <>
    bchar 0 <>
    encodeAE priv

serializeKey conf (HDPublicKey d i f pub code) =
    encodeBase58Check $
    commonHeader (tckr_hd_pub_key_prefix conf) d i f code <>
    encodeAE (compress pub)

seedToMaskerKey :: TCKRConf -> ByteString -> Either TCKRError HDKey
seedToMaskerKey conf seed =
    let l = hmacSHA512 (BS.pack (tckr_hd_seed_key conf)) seed
        ll = BSR.take 32 l
        lr = BSR.drop 32 l
        int = decodeVWord BigEndian ll
    in do
        assertMT "left int not in range" (int /= 0 && int < paramN)

        return (HDPrivateKey 0 0 nullFingerPrint (decodeFailBE ll) lr)

toECCPrivateKey (HDPrivateKey _ _ _ k _) = k
toECCPublicKey (HDPublicKey _ _ _ k _) = k

-- xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi
-- xprv9s21ZrQH143K4NseeceEeKVPfKhLZAqLJjedpyUmTSMArMJvh2yCTcUMXLDq9UrhRF46AjsZG7f3D2yw3v4DuAozRT9r8xKRmsvXHx24a26
