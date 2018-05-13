{-# LANGUAGE TypeSynonymInstances #-}

module Tucker.ECC where
    
import qualified Data.ByteString as BSR

import Crypto.Secp256k1

import Tucker.Enc
import Tucker.Util
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
        compressed <- (not . (`elem` [ 0x04, 0x06, 0x07 ])) <$> peekByteD
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
