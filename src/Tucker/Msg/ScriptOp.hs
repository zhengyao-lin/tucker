{-# LANGUAGE FlexibleInstances, ViewPatterns #-}

module Tucker.Msg.ScriptOp where

import Data.Int
import Data.Hex
import Data.Word
import Data.List
import Data.List.Split
import qualified Data.ByteString as BSR

import Control.Exception
import Control.Applicative

import Tucker.Enc
import Tucker.Auth
import Tucker.Util
import Tucker.Conf
import Tucker.Error

type ScriptPc = Int

data ScriptOp
    -- constant ops
    = OP_PUSHDATA ByteString (Maybe ByteString)
    -- the second bytestring is the original data
    | OP_CONST Int8

    -- flow-control
    | OP_NOP

    | OP_IF Bool ScriptPc -- position of the next OP_ELSE/OP_ENDIF
    | OP_ELSE ScriptPc -- position of the next OP_ELSE/OP_ENDIF
    | OP_ENDIF

    | OP_VERIFY
    | OP_RETURN

    -- stack ops
    | OP_TOALTSTACK
    | OP_FROMALTSTACK
    | OP_IFDUP
    | OP_DEPTH
    | OP_DROP
    | OP_DUP
    | OP_NIP
    | OP_OVER
    | OP_PICK
    | OP_ROLL
    | OP_ROT
    | OP_SWAP
    | OP_TUCK
    | OP_2DROP
    | OP_2DUP
    | OP_3DUP
    | OP_2OVER
    | OP_2ROT
    | OP_2SWAP

    -- splice
    | OP_CAT
    | OP_SUBSTR
    | OP_LEFT
    | OP_RIGHT
    | OP_SIZE

    -- bitwise ops
    | OP_INVERT
    | OP_AND
    | OP_OR
    | OP_XOR
    | OP_EQUAL
    | OP_EQUALVERIFY

    -- arithmetic
    | OP_1ADD
    | OP_1SUB
    | OP_2MUL
    | OP_2DIV
    | OP_NEGATE
    | OP_ABS
    | OP_NOT
    | OP_0NOTEQUAL
    | OP_ADD
    | OP_SUB
    | OP_MUL
    | OP_DIV
    | OP_MOD
    | OP_LSHIFT
    | OP_RSHIFT
    | OP_BOOLAND
    | OP_BOOLOR
    | OP_NUMEQUAL
    | OP_NUMEQUALVERIFY
    | OP_NUMNOTEQUAL
    | OP_LESSTHAN
    | OP_GREATERTHAN
    | OP_LESSTHANOREQUAL
    | OP_GREATERTHANOREQUAL
    | OP_MIN
    | OP_MAX
    | OP_WITHIN

    -- crypto ops
    | OP_RIPEMD160
    | OP_SHA1
    | OP_SHA256
    | OP_HASH160
    | OP_HASH256
    | OP_CODESEPARATOR
    | OP_CHECKSIG
    | OP_CHECKSIGVERIFY
    | OP_CHECKMULTISIG
    | OP_CHECKMULTISIGVERIFY
    
    | OP_CHECKLOCKTIMEVERIFY
    | OP_CHECKSEQUENCEVERIFY

    | OP_RESERVED
    | OP_VER
    | OP_VERIF
    | OP_VERNOTIF
    | OP_RESERVED1
    | OP_RESERVED2

    | OP_NOP1
    | OP_NOP4
    | OP_NOP5
    | OP_NOP6
    | OP_NOP7
    | OP_NOP8
    | OP_NOP9
    | OP_NOP10

    | OP_EOC -- end of code

    | OP_UNKNOWN Word8 -- unknown byte(should not fail on the decoding phase)

    -- for test ues
    | OP_PRINT String deriving (Eq, Show)

-- constant ops
-- OP_PUSHDATA(can have 5 forms each with different maximum sizes of data)
-- OP_NEG(push 0/1/-1 to the stack)
-- OP_1-OP_16(the number 0-16 is pushed into the stack)

one_byte_op_map :: [(ScriptOp, Word8)]
one_byte_op_map = [
        -- (OP_PUSHDATA BSR.empty,  0x00),
        (OP_NOP,                 0x61),

        (OP_VERIFY,              0x69),
        (OP_RETURN,              0x6a),

        (OP_TOALTSTACK,          0x6b),
        (OP_FROMALTSTACK,        0x6c),
        (OP_IFDUP,               0x73),
        (OP_DEPTH,               0x74),
        (OP_DROP,                0x75),
        (OP_DUP,                 0x76),
        (OP_NIP,                 0x77),
        (OP_OVER,                0x78),
        (OP_PICK,                0x79),
        (OP_ROLL,                0x7a),
        (OP_ROT,                 0x7b),
        (OP_SWAP,                0x7c),
        (OP_TUCK,                0x7d),
        (OP_2DROP,               0x6d),
        (OP_2DUP,                0x6e),
        (OP_3DUP,                0x6f),
        (OP_2OVER,               0x70),
        (OP_2ROT,                0x71),
        (OP_2SWAP,               0x72),

        (OP_CAT,                 0x7e),
        (OP_SUBSTR,              0x7f),
        (OP_LEFT,                0x80),
        (OP_RIGHT,               0x81),
        (OP_SIZE,                0x82),

        (OP_INVERT,              0x83),
        (OP_AND,                 0x84),
        (OP_OR,                  0x85),
        (OP_XOR,                 0x86),
        (OP_EQUAL,               0x87),
        (OP_EQUALVERIFY,         0x88),
        
        (OP_1ADD,                0x8b),
        (OP_1SUB,                0x8c),
        (OP_2MUL,                0x8d),
        (OP_2DIV,                0x8e),
        (OP_NEGATE,              0x8f),
        (OP_ABS,                 0x90),
        (OP_NOT,                 0x91),
        (OP_0NOTEQUAL,           0x92),
        (OP_ADD,                 0x93),
        (OP_SUB,                 0x94),
        (OP_MUL,                 0x95),
        (OP_DIV,                 0x96),
        (OP_MOD,                 0x97),
        (OP_LSHIFT,              0x98),
        (OP_RSHIFT,              0x99),
        (OP_BOOLAND,             0x9a),
        (OP_BOOLOR,              0x9b),
        (OP_NUMEQUAL,            0x9c),
        (OP_NUMEQUALVERIFY,      0x9d),
        (OP_NUMNOTEQUAL,         0x9e),
        (OP_LESSTHAN,            0x9f),
        (OP_GREATERTHAN,         0xa0),
        (OP_LESSTHANOREQUAL,     0xa1),
        (OP_GREATERTHANOREQUAL,  0xa2),
        (OP_MIN,                 0xa3),
        (OP_MAX,                 0xa4),
        (OP_WITHIN,              0xa5),

        (OP_RIPEMD160,           0xa6),
        (OP_SHA1,                0xa7),
        (OP_SHA256,              0xa8),
        (OP_HASH160,             0xa9),
        (OP_HASH256,             0xaa),
        (OP_CODESEPARATOR,       0xab),
        (OP_CHECKSIG,            0xac),
        (OP_CHECKSIGVERIFY,      0xad),
        (OP_CHECKMULTISIG,       0xae),
        (OP_CHECKMULTISIGVERIFY, 0xaf),

        (OP_CHECKLOCKTIMEVERIFY, 0xb1),
        (OP_CHECKSEQUENCEVERIFY, 0xb2),

        (OP_RESERVED,            0x50),
        (OP_VER,                 0x62),
        (OP_VERIF,               0x65),
        (OP_VERNOTIF,            0x66),
        (OP_RESERVED1,           0x89),
        (OP_RESERVED2,           0x8a),

        (OP_NOP1,                0xb0),
        (OP_NOP4,                0xb3),
        (OP_NOP5,                0xb4),
        (OP_NOP6,                0xb5),
        (OP_NOP7,                0xb6),
        (OP_NOP8,                0xb7),
        (OP_NOP9,                0xb8),
        (OP_NOP10,               0xb9)
    ]

one_byte_op_index = map fst one_byte_op_map
one_byte_op_map_r = map (\(a, b) -> (b, a)) one_byte_op_map

disabled_ops = [
        OP_CAT, OP_SUBSTR, OP_LEFT,
        OP_RIGHT, OP_INVERT, OP_AND,
        OP_OR, OP_XOR,
        OP_2MUL, OP_2DIV,
        OP_MUL, OP_DIV, OP_MOD,
        OP_LSHIFT, OP_RSHIFT
    ]

instance Encodable ScriptOp where
    encode _ (OP_PUSHDATA _ (Just cache)) = cache

    encode _ (OP_PUSHDATA dat Nothing) =
        if len <= 0x4b then
            BSR.concat [ encodeLE (fi len :: Word8), dat ]
        else if len <= 0xff then
            BSR.concat [ bchar 0x4c, encodeLE (fi len :: Word8), dat ]
        else if len <= 0xffff then
            BSR.concat [ bchar 0x4d, encodeLE (fi len :: Word16), dat ]
        else -- if len <= 0xffffffff then
            BSR.concat [ bchar 0x4e, encodeLE (fi len :: Word32), dat ]
        where
            len = BSR.length dat

    encode _ (OP_CONST n)
        | n == -1   = bchar 0x4f
        | n >= 1 && n <= 16
                    = bchar (0x50 + n)
        | otherwise = throw $ TCKRError "op constant value not in range 0-16"

    encode end (OP_IF exp _) =
        bchar $ if exp then 0x63 else 0x64 -- OP_IF or OP_NOTIF
        {-
        mconcat [
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
        -}
    
    -- OP_ELSE and OP_ENDIF can
    -- be separately encoded from OP_IF
    -- but cannot be decoded separately
    encode _ (OP_ELSE _)  = bchar 0x67
    encode _ OP_ENDIF = bchar 0x68

    encode _ (OP_UNKNOWN b) = bchar b

    -- one-byte ops
    encode _ op
        | op `elem` one_byte_op_index =
            let Just i = lookup op one_byte_op_map in bchar i

opPushdataD :: Decoder ScriptOp
opPushdataD = do
    orig <- getD
    len1 <- lenD

    i <- byteD

    len <-
        if i == 0 then return 0 -- fail "OP_PUSHDATA starts with a non-zero byte"
        else if i <= 0x4b then return $ fi i
        else if i == 0x4c then fi <$> (decoder :: Decoder Word8)
        else if i == 0x4d then fi <$> (decoder :: Decoder Word16)
        else if i == 0x4e then fi <$> (decoder :: Decoder Word32)
        else fail "OP_PUSHDATA invalid first byte"

    dat <- bsD len

    len2 <- lenD
    let cache = BSR.take (len1 - len2) orig

    return $ OP_PUSHDATA dat (Just cache)

opConstD :: Decoder ScriptOp
opConstD = do
    i <- byteD

    if i == 0x4f then return $ OP_CONST (-1)
    else if i >= 0x51 &&
            i <= 0x60 then return $ OP_CONST (fi (i - 0x50))
    else fail "OP_CONST invalid first byte"

opIfD :: Decoder [ScriptOp]
opIfD = do
    i <- byteD
    exp <-
        if i == 0x63 then return True
        else if i == 0x64 then return False
        else fail "OP_IF/OP_NOTIF invalid first byte"

    main_body <- stmtsD
    
    -- NOTE: multiple OP_ELSE branch is allowed!
    -- the else branch is executed iff the preceding
    -- branch is NOT executed
    else_bodies <- many $ do
        beginWithByteD 0x67
        ops <- stmtsD
        return ops

    beginWithByteD 0x68 -- OP_ENDIF

    let if_branch = OP_IF exp (length main_body + 1) : main_body
        else_branches =
            flip map else_bodies $ \else_body ->
                OP_ELSE (length else_body + 1) : else_body

    return (if_branch ++ concat else_branches ++ [ OP_ENDIF ])

oneByteOpD :: Decoder ScriptOp
oneByteOpD = do
    i <- byteD

    case lookup i one_byte_op_map_r of
        Just op -> return op
        _ -> fail "not a one-byte op"

unknownD :: Decoder ScriptOp
unknownD = do
    i <- byteD

    if i `notElem` [ 0x67, 0x68 ] then
        return (OP_UNKNOWN i)
    else
        fail "OP_ELSE/OP_ENDIF without an OP_IF/OP_NOTIF"

stmtD :: Decoder [ScriptOp]
stmtD =
    opIfD <|> do
        other <- opPushdataD <|> opConstD <|> oneByteOpD <|> unknownD
        return [other]

stmtsD :: Decoder [ScriptOp]
stmtsD = concat <$> many stmtD

instance Decodable [ScriptOp] where
    decoder = do
        all <- getD
        res <- stmtsD
        len <- lenD

        rest <- getD

        if len /= 0 then
            fail $ "failed to parse " ++
                   show (hex all) ++ ", " ++
                   show (hex rest) ++ ", " ++
                   show res -- reparse and output the last errors
        else return res

-- -- extract code after the last(if exists) OP_CODESEPARATOR
-- extractValidCode :: [ScriptOp] -> [ScriptOp]
-- extractValidCode ops =
--     last $ splitOn [OP_CODESEPARATOR] ops

-- error when adding block Block 000000000017538a71d012de3a73fcc22415b2aabf7329133c0109aa0a75b4b4: tucker error: tucker error: failed to parse "63BAC0D0E0F0F1F2F3F3F4FF675168"
-- 63 BAC0D0E0F0F1F2F3F3F4FF6751 68

-- types of script
data ScriptType
    = SCRIPT_P2PKH
    | SCRIPT_P2PK
    | SCRIPT_P2SH ByteString -- redeem script
    | SCRIPT_P2MULTISIG
    | SCRIPT_P2WPKH
    | SCRIPT_P2WSH
    | SCRIPT_NONSTD deriving (Show)

instance Eq ScriptType where
    SCRIPT_P2PKH == SCRIPT_P2PKH = True
    SCRIPT_P2PK == SCRIPT_P2PK = True
    (SCRIPT_P2SH _) == (SCRIPT_P2SH _) = True
    SCRIPT_P2MULTISIG == SCRIPT_P2MULTISIG = True
    SCRIPT_P2WPKH == SCRIPT_P2WPKH = True
    SCRIPT_P2WSH == SCRIPT_P2WSH = True
    SCRIPT_NONSTD == SCRIPT_NONSTD = True
    _ == _ = False

allPush :: [ScriptOp] -> Bool
allPush [] = True
allPush (OP_PUSHDATA _ _:rst) = allPush rst
allPush _ = False

parseVersionByte :: ScriptOp -> Maybe Word8
parseVersionByte (OP_CONST n) = if n >= 0 then Just (fi n) else Nothing -- 1 - 16
parseVersionByte (OP_PUSHDATA dat _) = if BSR.null dat then Just 0 else Nothing -- 0
parseVersionByte _ = Nothing

parseWitnessProgram :: [ScriptOp] -> Maybe (ByteString)
parseWitnessProgram [ parseVersionByte -> Just 0, OP_PUSHDATA wit _ ] =
    if BSR.length wit == 20 ||
       BSR.length wit == 32 then
        Just wit
    else
        Nothing

parseWitnessProgram _ = Nothing

-- (sig script, pub key script)
getScriptType :: [[ScriptOp]] -> ScriptType

-- P2PKH
-- sig_script: <signature> <public key>
--  pk_script: OP_DUP OP_HASH160 <public key hash> OP_EQUALVERIFY OP_CHECKSIG
getScriptType
    [ [ OP_PUSHDATA _ _, OP_PUSHDATA _ _ ],
      [ OP_DUP, OP_HASH160, OP_PUSHDATA _ _, OP_EQUALVERIFY, OP_CHECKSIG ] ]
    = SCRIPT_P2PKH

-- P2PK
-- sig_script: <signature>
--  pk_script: <public key> OP_CHECKSIG
getScriptType
    [ [ OP_PUSHDATA _ _ ], [ OP_PUSHDATA _ _, OP_CHECKSIG ] ]
    = SCRIPT_P2PK

-- P2SH
-- sig_script: just OP_PUSHDATA's
--  pk_script: OP_HASH160 <hash160(redeem script)> OP_EQUAL
getScriptType
    [ reverse -> OP_PUSHDATA redeem _ : (allPush -> True),
      [ OP_HASH160, OP_PUSHDATA _ _, OP_EQUAL ] ]
    = SCRIPT_P2SH redeem

-- P2MULTISIG
-- sig_script: OP_0 <signature 1> <signature 2>
--  pk_script: M <public key 1> <public key 2> ... <public key N> N OP_CHECKMULTISIG
getScriptType
    [ OP_PUSHDATA _ _ : (allPush -> True),
      OP_CONST _:(reverse -> OP_CHECKMULTISIG:OP_CONST _:(allPush -> True)) ]
    = SCRIPT_P2MULTISIG

getScriptType [ [], parseWitnessProgram -> Just wit ] =
    case BSR.length wit of
        20 -> SCRIPT_P2WPKH
        32 -> SCRIPT_P2WSH
        _ -> SCRIPT_NONSTD

getScriptType _ = SCRIPT_NONSTD

stdPkScriptP2PKH :: TCKRConf -> Address -> [ScriptOp]
stdPkScriptP2PKH conf addr =
    [
        OP_DUP, OP_HASH160,
        OP_PUSHDATA (either (error . show) id (addr2hash conf addr)) Nothing,
        OP_EQUALVERIFY, OP_CHECKSIG
    ]
