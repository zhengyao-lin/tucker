module Tucker.Script where

import Tucker.Enc

import Data.Word
import Data.Int

import Debug.Trace

import qualified Data.ByteString as BSR

data ScriptOp
    = OP_PUSHDATA ByteString
    | OP_DUP
    | OP_HASH160
    | OP_EQUALVERIFY
    | OP_CHECKSIG

instance Encodable ScriptOp where
    encode _ (OP_PUSHDATA dat) =
        if len <= 0x4b then
            BSR.concat [ encodeLE (fromIntegral len :: Word8), dat ]
        else if len <= 0xff then
            BSR.concat [ bchar 0x4c, encodeLE (fromIntegral len :: Word8), dat ]
        else if len <= 0xffff then
            BSR.concat [ bchar 0x4d, encodeLE (fromIntegral len :: Word16), dat ]
        else -- if len <= 0xffffffff then
            BSR.concat [ bchar 0x4e, encodeLE (fromIntegral len :: Word32), dat ]
        where
            len = BSR.length dat

    encode _ OP_DUP         = bchar 0x76
    encode _ OP_HASH160     = bchar 0xa9
    encode _ OP_EQUALVERIFY = bchar 0x88
    encode _ OP_CHECKSIG    = bchar 0xac
