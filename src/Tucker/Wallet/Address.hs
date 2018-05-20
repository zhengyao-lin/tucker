module Tucker.Wallet.Address where

import Data.Word
import Data.Char
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.Enc
import Tucker.Msg
import Tucker.ECC
import Tucker.Conf
import Tucker.Auth
import Tucker.Error

data Address
    = P2PKHAddr ByteString -- pub key hash(ripemd160)
    | P2SHAddr ByteString -- script hash(ripemd160)
    -- | P2WAddr -- naive p2wsh/p2wpkh addr(bech32)
    deriving (Eq, Show)

encodeAddress :: TCKRConf -> Address -> String
encodeAddress conf (P2PKHAddr hash) =
    BS.unpack (base58encCheck (BSR.cons (tckr_p2pkh_addr_pref conf) hash))

encodeAddress conf (P2SHAddr hash) =
    BS.unpack (base58encCheck (BSR.cons (tckr_p2sh_addr_pref conf) hash))
    
decodeAddressFail :: TCKRConf -> String -> Address
decodeAddressFail conf = either (error . show) id . decodeAddress conf

decodeAddress :: TCKRConf -> String -> Either TCKRError Address
decodeAddress conf addr' = do
    addr <- base58decCheck (BS.pack addr')
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
p2pkhAddress conf = P2PKHAddr . ripemd160 . sha256 . encodeBE

p2shAddress :: TCKRConf -> [ScriptOp] -> Address
p2shAddress conf = P2SHAddr . ripemd160 . sha256 . encodeLE
