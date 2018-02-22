{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Tucker.Msg.ScriptOp where

import Data.Int
import Data.Word
import Data.List
import Data.List.Split
import qualified Data.ByteString as BSR

import Control.Exception
import Control.Applicative

import Debug.Trace

import Tucker.Enc
import Tucker.Util
import Tucker.Error

data ScriptOp
    -- constant ops
    = OP_PUSHDATA ByteString
    | OP_CONST Word8

    -- flow-control
    | OP_NOP

    -- if <expected value> true_branch false_branch
    | OP_IF Bool [ScriptOp] [ScriptOp]


    | OP_VERIFY
    | OP_RETURN

    -- stack ops
    | OP_DUP

    -- bitwise ops
    | OP_EQUAL
    | OP_EQUALVERIFY

    -- crypto ops
    | OP_HASH160
    | OP_HASH256
    | OP_CHECKSIG
    | OP_CHECKSIGVERIFY
    | OP_CHECKMULTISIG
    | OP_CHECKMULTISIGVERIFY
    
    | OP_CODESEPARATOR

    -- for test ues
    | OP_PRINT String deriving (Eq, Show)

-- constant ops
-- OP_PUSHDATA(can have 4 forms each with different maximum sizes of data)
-- OP_NEG(push 0/1/-1 to the stack)
-- OP_0-OP_16(the number 0-16 is pushed into the stack)

one_byte_op_map :: [(ScriptOp, Word8)]
one_byte_op_map = [
        (OP_NOP,                 0x61),

        (OP_VERIFY,              0x69),
        (OP_RETURN,              0x6a),

        (OP_DUP,                 0x76),

        (OP_EQUAL,               0x87),
        (OP_EQUALVERIFY,         0x88),

        (OP_HASH160,             0xa9),
        (OP_HASH256,             0xaa),
        (OP_CHECKSIG,            0xac),
        (OP_CHECKSIGVERIFY,      0xad),
        (OP_CHECKMULTISIG,       0xae),
        (OP_CHECKMULTISIGVERIFY, 0xaf),
        (OP_CODESEPARATOR,       0xab)
    ]

one_byte_op_index = map fst one_byte_op_map
one_byte_op_map_r = map (\(a, b) -> (b, a)) one_byte_op_map

instance Encodable ScriptOp where
    encode _ (OP_PUSHDATA dat) =
        if len /= 0 && len <= 0x4b then
            BSR.concat [ encodeLE (fromIntegral len :: Word8), dat ]
        else if len <= 0xff then
            BSR.concat [ bchar 0x4c, encodeLE (fromIntegral len :: Word8), dat ]
        else if len <= 0xffff then
            BSR.concat [ bchar 0x4d, encodeLE (fromIntegral len :: Word16), dat ]
        else -- if len <= 0xffffffff then
            BSR.concat [ bchar 0x4e, encodeLE (fromIntegral len :: Word32), dat ]
        where
            len = BSR.length dat

    encode _ (OP_CONST n)
        | n == 0    = bchar 0x00
        | n == -1   = bchar 0x4f
        | n >= 1 && n <= 16
                    = bchar (0x50 + n)
        | otherwise = throw $ TCKRError "op constant value not in range 0-16"

    encode end (OP_IF exp b1 b2) = mconcat [
            -- OP_IF
            bchar $ if exp then 0x63 else 0x64,

            -- true branch
            encode end b1,

            -- optional else branch
            if null b2 then BSR.empty
            else bchar 0x67 <> encode end b2,

            -- OP_ENDIF
            bchar 0x68
        ]

    -- one-byte ops
    encode _ op
        | op `elem` one_byte_op_index =
            let Just i = lookup op one_byte_op_map in bchar i

opPushdataD :: Decoder ScriptOp
opPushdataD = do
    i <- byteD

    len <-
        if i == 0 then fail "OP_PUSHDATA starts with a non-zero byte"
        else if i <= 0x4b then return $ fi i
        else if i == 0x4c then fi <$> (decoder :: Decoder Word8)
        else if i == 0x4d then fi <$> (decoder :: Decoder Word16)
        else if i == 0x4e then fi <$> (decoder :: Decoder Word32)
        else fail "OP_PUSHDATA invalid first byte"

    dat <- bsD len

    return $ OP_PUSHDATA dat

opConstD :: Decoder ScriptOp
opConstD = do
    i <- byteD

    if i == 0    then return $ OP_CONST 0
    else if i == 0x4f then return $ OP_CONST (-1)
    else if i >= 0x51 &&
            i <= 0x60 then return $ OP_CONST (i - 0x50)
    else fail "OP_CONST invalid first byte"

opIfD :: Decoder ScriptOp
opIfD = do
    i <- byteD
    exp <-
        if i == 0x63 then return True
        else if i == 0x64 then return False
        else fail "OP_IF/OP_NOTIF invalid first byte"        

    b1 <- decoder

    i <- byteD
    
    b2 <-
        if i == 0x67 then do
            ops <- decoder
            beginWithByteD 0x68 -- ends with OP_ENDIF
            return ops
        else if i == 0x68 then return []
        else fail "OP_IF invalid syntax"

    return $ OP_IF exp b1 b2

oneByteOpD :: Decoder ScriptOp
oneByteOpD = do
    i <- byteD

    case lookup i one_byte_op_map_r of
        Just op -> return op
        _ -> fail "not a one-byte op"

instance Decodable ScriptOp where
    decoder = opPushdataD <|> opConstD <|> opIfD <|> oneByteOpD <|> fail "invalid op"

-- extract code after the last(if exists) OP_CODESEPARATOR
extractValidCode :: [ScriptOp] -> [ScriptOp]
extractValidCode ops =
    last $ splitOn [OP_CODESEPARATOR] ops
