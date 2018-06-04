{-# LANGUAGE TypeSynonymInstances #-}

module Tucker.ECC where
    
import qualified Data.ByteString as BSR

import Crypto.Error
import Crypto.Secp256k1
import Crypto.PubKey.ECC.Types
import qualified Crypto.PubKey.ECC.Prim as ECC

import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.Error

type ECCPrivateKey = SecKey
data ECCPublicKey = ECCPublicKey Bool PubKey deriving (Eq, Show)
type ECCSignature = Sig

data ECCLaxSignature = ECCLaxSignature ECCSignature

instance Encodable ECCPrivateKey where
    encode _ = getSecKey

instance Decodable ECCPrivateKey where
    decoder = do
        res <- secKey <$> allD

        case res of
            Just priv -> return priv
            Nothing -> fail "failed to decode private key"

instance Encodable ECCPublicKey where
    encode _ (ECCPublicKey comp pub) = exportPubKey comp pub

instance Decodable ECCPublicKey where
    decoder = do
        compressed <- (`notElem` [ 0x04, 0x06, 0x07 ]) <$> peekByteD
        res <- importPubKey <$> allD

        case res of
            Just pub -> return (ECCPublicKey compressed pub)
            Nothing -> fail "failed to decode public key"

instance Encodable ECCSignature where
    encode _ = exportSig

instance Decodable ECCSignature where
    decoder = do
        -- using strict encoding
        res <- importSig <$> allD

        case res of
            Just sig -> return sig
            Nothing -> fail "failed to decode signature"

instance Decodable ECCLaxSignature where
    decoder = do
        res <- laxImportSig <$> allD

        case res of
            Just sig -> return (ECCLaxSignature sig)
            Nothing -> fail "failed to decode non-strict signature"

privToPub :: ECCPrivateKey -> ECCPublicKey
privToPub priv = ECCPublicKey True (derivePubKey priv)

privToInt :: ECCPrivateKey -> Integer
privToInt priv = decodeVWord BigEndian (encodeBE priv)

intToPriv :: Integer -> ECCPrivateKey
intToPriv int = decodeFailBE (encodeInt 32 BigEndian (int `mod` paramN))

compress :: ECCPublicKey -> ECCPublicKey
compress (ECCPublicKey _ k) = ECCPublicKey True k

uncompress :: ECCPublicKey -> ECCPublicKey
uncompress (ECCPublicKey _ k) = ECCPublicKey False k

pubToPoint :: ECCPublicKey -> (Integer, Integer)
pubToPoint pub =
    let raw = encodeBE (uncompress pub)
        x = decodeVWord BigEndian (BSR.take 32 (BSR.drop 1 raw))
        y = decodeVWord BigEndian (BSR.drop 33 raw)
    in (x, y)

pointToPub :: (Integer, Integer) -> Either TCKRError ECCPublicKey
pointToPub (x, y) =
    (compress <$>) $
    decodeAllBE $
    bchar 0x4 <> encodeInt 32 BigEndian x <> encodeInt 32 BigEndian y

-- genRaw :: IO (ECCPublicKey, ECCPrivateKey)
-- genRaw = do
--     (PublicKey _ pt, PrivateKey _ num) <- generate tucker_curve
--     let Point x y = pt
--     return (ECCPublicKey True x y, ECCPrivateKey num)

bsToMsg bs = m
    where 
        Just m = msg (fill bs)
        fill bs =
            let len = BSR.length bs in
            if len < 32 then bs <> BSR.replicate (32 - len) 0
            else BSR.take 32 bs

-- hash & sign
sign :: ECCPrivateKey -> ByteString -> IO ECCSignature
sign priv msg = return (signMsg priv (bsToMsg msg))
    -- let privk = PrivateKey tucker_curve num
    -- Signature r s <- ECDSA.sign privk NoHash256 msg
    -- return $ ECCSignature r s

verify :: ECCPublicKey -> ByteString -> ECCSignature -> Bool
verify (ECCPublicKey _ pub) msg sig' =
    verifySig pub sig (bsToMsg msg)
    where (sig, _) = normalizeSig sig'

    -- ECDSA.verify NoHash256 pubk (Signature r s) msg
    -- where pubk = PublicKey tucker_curve (Point x y)

signDER :: ECCPrivateKey -> ByteString -> IO ByteString
signDER priv msg = do
    sig <- sign priv msg
    return $ encodeBE sig

verifyDER :: ECCPublicKey -> ByteString -> ByteString -> Either TCKRError Bool
verifyDER pub msg sig_enc = do
    sig <- decodeAllBE sig_enc
    return $ verify pub msg sig

verifyLaxDER :: ECCPublicKey -> ByteString -> ByteString -> Either TCKRError Bool
verifyLaxDER pub msg sig_enc = do
    ECCLaxSignature sig <- decodeAllBE sig_enc
    return $ verify pub msg sig

paramN = 115792089237316195423570985008687907852837564279074904382605163141518161494337 :: Integer
paramG = pubToPoint (decodeFailBE (hex2bs "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))
paramA = 0
paramB = 7

pointAdd :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
pointAdd p1 p2 = (x, y)
    where
        curve = getCurveByName SEC_p256k1
        Point x y = ECC.pointAdd curve (uncurry Point p1) (uncurry Point p2)

pointMul :: Integer -> (Integer, Integer) -> (Integer, Integer)
pointMul s p = (x, y)
    where
        curve = getCurveByName SEC_p256k1
        Point x y = ECC.pointMul curve s (uncurry Point p)
