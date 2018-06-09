module Tucker.Wallet.Address where

import Data.Word
import Data.Char
import Data.Hashable
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.Enc
import Tucker.Msg
import Tucker.ECC
import Tucker.Conf
import Tucker.Error
import Tucker.Crypto

data Address
    = P2PKHAddr ByteString -- pub key hash(hash160)
    | P2SHAddr ByteString -- script hash(hash160)
    -- | P2WAddr -- naive p2wsh/p2wpkh addr(bech32)
    deriving (Eq, Show)

instance Hashable Address where
    hashWithSalt salt (P2PKHAddr hash) = hashWithSalt salt hash
    hashWithSalt salt (P2SHAddr hash) = hashWithSalt salt hash

encodeAddress :: TCKRConf -> Address -> String
encodeAddress conf (P2PKHAddr hash) =
    encodeBase58Check (BSR.cons (tckr_p2pkh_addr_pref conf) hash)

encodeAddress conf (P2SHAddr hash) =
    encodeBase58Check (BSR.cons (tckr_p2sh_addr_pref conf) hash)
    
decodeAddressFail :: TCKRConf -> String -> Address
decodeAddressFail conf = either (error . show) id . decodeAddress conf

decodeAddress :: TCKRConf -> String -> Either TCKRError Address
decodeAddress conf addr' = do
    addr <- decodeBase58Check addr'
    let pref = BSR.head addr
        hash = BSR.tail addr

    assertMT "address length illegal length" $
        BSR.length addr == 21 -- 1 pref + 20 hash160

    if pref == tckr_p2pkh_addr_pref conf then
        return (P2PKHAddr hash)
    else if pref == tckr_p2sh_addr_pref conf then
        return (P2SHAddr hash)
    else
        throwMT ("unrecognized prefix byte " ++ show pref)

genPubKeyScript :: Address -> [ScriptOp]
genPubKeyScript (P2PKHAddr hash) =
    [ OP_DUP, OP_HASH160, OP_PUSHDATA hash Nothing, OP_EQUALVERIFY, OP_CHECKSIG ]

genPubKeyScript (P2SHAddr hash) =
    [ OP_HASH160, OP_PUSHDATA hash Nothing, OP_EQUAL ]

p2pkhAddress :: TCKRConf -> ECCPublicKey -> Address
p2pkhAddress conf = P2PKHAddr . ripemd160 . sha256 . encodeAE

p2shAddress :: TCKRConf -> [ScriptOp] -> Address
p2shAddress conf = P2SHAddr . ripemd160 . sha256 . encodeAE

pubKeyScriptToAddress :: [ScriptOp] -> Maybe Address

pubKeyScriptToAddress [ OP_PUSHDATA pub _, OP_CHECKSIG ]
    = Just (P2PKHAddr (ripemd160 (sha256 pub)))

pubKeyScriptToAddress [ OP_DUP, OP_HASH160, OP_PUSHDATA hash _, OP_EQUALVERIFY, OP_CHECKSIG ]
    = Just (P2PKHAddr hash)

pubKeyScriptToAddress [ OP_HASH160, OP_PUSHDATA hash _, OP_EQUAL ]
    = Just (P2SHAddr hash)

pubKeyScriptToAddress _ = Nothing
