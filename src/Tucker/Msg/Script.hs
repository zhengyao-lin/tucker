{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Tucker.Msg.Script where

import Data.Hex
import Data.List
import Data.Word
import Data.Bits
import qualified Data.ByteString as BSR

import Control.Monad
import Control.Monad.State
import Control.Monad.Loops
import Control.Monad.Except

import Debug.Trace

import Tucker.Enc
import Tucker.Util
import Tucker.Auth
import Tucker.Error

import Tucker.Msg.Tx
import Tucker.Msg.Common
import Tucker.Msg.Hash256
import Tucker.Msg.ScriptOp

data ScriptConf =
    ScriptConf {
        script_enable_p2sh :: Bool
    } deriving (Show)

instance Default ScriptConf where
    def = ScriptConf { script_enable_p2sh = True }

type StackItem = ByteString

bsToItem = id
itemToBS = id

itemToBool :: StackItem -> Bool
itemToBool item =
    if is_null || is_zero then False
    else True
    where
        tail = BSR.init item
        head = BSR.last item

        is_null = BSR.null item
        is_zero = BSR.all (== 0) tail && (head == 0x00 || head == 0x80)

boolToItem :: Bool -> StackItem
boolToItem bool =
    if bool then trueS
    else falseS

-- when an item is interpreted as an integer
-- it's little-endian and the highest bit of the last byte is the sign
-- (not 2's complement)
itemToInt :: StackItem -> Integer
itemToInt item =
    if BSR.null item then 0
    else (if sign == 1 then negate else id) (bs2vintLE unsigned)
    where
        last_byte = BSR.last item
        sign = last_byte `shiftR` 7 -- 1 or 0
        -- clear the highest bit
        unsigned = BSR.init item <> bchar (last_byte .&. 0x7f)

intToItem :: Integer -> StackItem
intToItem int =
    if int < 0 then
        if last_byte .&. 0x80 /= 0 then
            -- sign bit already occupied, append one more byte
            raw <> bchar sign_mask
        else
            BSR.init raw <> bchar (last_byte .|. sign_mask)
    else raw
    where
        sign_mask =
            if int < 0 then 0x80
            else 0x00

        Right raw = vint2bsLE (abs int)
        last_byte = BSR.last raw

data ScriptState =
    ScriptState {
        script_conf :: ScriptConf,
        eval_stack  :: [StackItem],

        prog_code   :: [ScriptOp],
        prog_count  :: ScriptPc,
        last_cs_op  :: ScriptPc,

        -- tx_prev_out :: TxOutput, -- previous tx output point
        tx_in_idx   :: Word32,

        tx_body     :: TxPayload
    } deriving (Show)

type EvalState v = StateT ScriptState TCKRErrorM v

{-

- OP_PUSHDATA ByteString
- OP_CONST Int8

-- flow-control
- OP_NOP

-- if <expected value> true_branch false_branch
- OP_IF Bool [ScriptOp] [ScriptOp]

- OP_VERIFY
- OP_RETURN

-- stack ops
- OP_DUP

-- bitwise ops
- OP_EQUAL
- OP_EQUALVERIFY

-- crypto ops
- OP_HASH160
- OP_HASH256
- OP_CHECKSIG
- OP_CHECKSIGVERIFY
- OP_CHECKMULTISIG
- OP_CHECKMULTISIGVERIFY

-}

invalidTx = failT "invalid transaction"

trueS = bsToItem $ bchar 1
falseS = bsToItem $ bchar 0

pushS :: StackItem -> EvalState ()
pushS item =
    modify $ \s ->
        s { eval_stack = item : eval_stack s }

pushBSS = pushS . bsToItem
pushBoolS = pushS . boolToItem
pushIntS = pushS . intToItem

peekS :: EvalState StackItem
peekS = do
    es@(ScriptState { eval_stack = stack }) <- get

    assertT (not (null stack)) "peek on an empty stack"

    return $ head stack

peekBSS = itemToBS <$> peekS

popS :: EvalState StackItem
popS = do
    es@(ScriptState { eval_stack = stack }) <- get

    assertT (not (null stack)) "pop on an empty stack"

    put (es { eval_stack = tail stack })
    return $ head stack

pop2S :: EvalState (StackItem, StackItem)
pop2S = (,) <$> popS <*> popS

popNS :: Int -> EvalState [StackItem]
popNS = (`replicateM` popS)

popConvS :: (StackItem -> t) -> EvalState t
popConvS = (<$> popS)

pop2ConvS :: (StackItem -> t) -> EvalState (t, t)
pop2ConvS conv =
    (,) <$> popConvS conv <*> popConvS conv

popNConvS :: (StackItem -> t) -> Int -> EvalState [t]
popNConvS conv n = map conv <$> popNS n

popBSS = popConvS itemToBS
pop2BSS = pop2ConvS itemToBS
popNBSS = popNConvS itemToBS

popBoolS = popConvS itemToBool
pop2BoolS = pop2ConvS itemToBool
popNBoolS = popNConvS itemToBool

popIntS = popConvS itemToInt
pop2IntS = pop2ConvS itemToInt
popNIntS = popNConvS itemToInt

pickS :: Int -> EvalState ()
pickS n = do
    es@(ScriptState { eval_stack = stack }) <- get

    assertT (n >= 0 && n < length stack) "illegal pick range"

    pushS (stack !! n)

dupS :: EvalState ()
dupS = do
    s@(ScriptState { eval_stack = stack }) <- get

    assertT (not (null stack)) "dup on an empty stack"

    put (s { eval_stack = head stack : stack })

txS :: EvalState TxPayload
txS = tx_body <$> get

-- ONE time SHA256 hash of the raw tx body for signature
-- require the raw signature with the htype byte appended
rawSigHashS :: ByteString -> EvalState ByteString
rawSigHashS sig_raw = do
    cur_tx   <- txS
    in_idx   <- tx_in_idx <$> get
    cs_op    <- last_cs_op <$> get
    code     <- prog_code <$> get

    let -- turn the current code(should be pk_script) to subscript
        -- that is used in signature
        -- defined as from the last EXECUTED OP_CODESEPARATOR to the end
        -- with all OP_CODESEPARATOR's removed
        -- (https://en.bitcoin.it/w/images/en/7/70/Bitcoin_OpCheckSig_InDetail.png)
        subscr = filter (/= OP_CODESEPARATOR) . drop (cs_op + 1)
         
        htype = intToHashType (BSR.last sig_raw)
        rawtx = sigRawTx cur_tx in_idx (subscr code) htype
        hash = ba2bs $ sha256 rawtx
        -- NOTE: only sha256 it ONCE because there's another
        -- hashing in the verification process

    -- traceShowM (hex rawtx)

    return hash

incPcS :: EvalState ()
incPcS =
    modify' (\s -> s { prog_count = prog_count s + 1 })

ofsPcS :: ScriptPc -> EvalState ()
ofsPcS ofs =
    modify' (\s -> s { prog_count = prog_count s + ofs })

curOpS :: EvalState ScriptOp
curOpS = do
    ScriptState {
        prog_code = code,
        prog_count = pc
    } <- get

    assertT (pc < length code) "counter has reached the end"

    return (code !! pc)

-- is end-of-code
eocS :: EvalState Bool
eocS = do
    ScriptState {
        prog_code = code,
        prog_count = pc
    } <- get

    return (pc >= length code)

-- verifyFail public_key_encoded message signature_encoded
verifyFail :: ByteString -> ByteString -> ByteString -> Bool
verifyFail pub msg sig =
    -- NOTE: final hash is encoded in big-endian
    verifySHA256DER (decodeFailBE pub) msg sig == Right True

evalOpS :: ScriptOp -> EvalState ()
evalOpS (OP_PUSHDATA dat) = pushBSS dat
evalOpS (OP_CONST v) = pushIntS (fi v)

evalOpS OP_NOP = return ()
evalOpS (OP_IF exp ofs) = do
    top <- popS

    if itemToBool top == exp then
        -- main branch
        return ()
    else -- jump to the next OP_ELSE/OP_ENDIF
        ofsPcS ofs

evalOpS (OP_ELSE ofs) = ofsPcS ofs
evalOpS OP_ENDIF = return ()

evalOpS OP_VERIFY = do
    top <- popS

    if itemToBool top then return ()
    else invalidTx

evalOpS OP_RETURN = invalidTx
evalOpS OP_DUP = dupS

evalOpS OP_EQUAL = do
    (a, b) <- pop2S
    pushBoolS (a == b)

evalOpS OP_EQUALVERIFY =
    evalOpS OP_EQUAL >>
    evalOpS OP_VERIFY

evalOpS OP_SHA256 = do
    val <- popBSS
    pushBSS $ ba2bs $ sha256 val

evalOpS OP_HASH160 = do
    val <- popBSS
    pushBSS $ ba2bs $ ripemd160 $ sha256 val

evalOpS OP_HASH256 = do
    val <- popBSS
    pushBSS $ ba2bs $ sha256 $ sha256 val

evalOpS OP_CHECKSIG = do
    (pub', sig') <- pop2BSS
    msg <- rawSigHashS sig'

    -- there is one byte(hash type) appended to the signature
    pushBoolS (verifyFail pub' msg (BSR.init sig'))

    -- tx <- encodeLE <$> txS
    -- traceShowM (hex $ encodeBE ((decodeFailBE pub') { compressed = False }), hex $ ba2bs $ sha256 msg, hex tx, hex $ BSR.init sig')

    -- traceShowM (decodeFailBE pub' :: ECCPublicKey)

    -- magic prefix 3056 301006072 300CA00706052B8104000AA144034200

    -- 3056 301006072A8648CE3D020106052B8104000A 034200 04085C6600657566ACC2D6382A47BC3F324008D2AA10940DD7705A48AA2A5A5E33A726DD7E88D4C03086E19122A8350F82CF7FFF16D8111B48FD82D9DBD07B374C

    -- 02085C6600657566ACC2D6382A47BC3F324008D2AA10940DD7705A48AA2A5A5E33
    -- 4EB4DCCD727E81315A9FF801C205EFC62635471CF8668E42C1C8AEBFB51500A3
    -- 3044022045D08719828FBD93E49C9223E63F4D2DAB2DE6C568E1FAA2CCCB33ADF2575D2C02200C00126CB0105275040A963D91E45460147E40451B590485CF438606D3C784CF

    -- 03F5D0FB955F95DD6BE6115CE85661DB412EC6A08ABCBFCE7DA0BA8297C6CC0EC4
    -- 4EB4DCCD727E81315A9FF801C205EFC62635471CF8668E42C1C8AEBFB51500A3
    -- 30450221009A29101094B283AE62A6FED68603C554CA3A624B9A78D83E8065EDCF97AE231B02202CBED6E796EE6F4CAF30EDEF8F5597A08A6BE265D6601AD92283990B55C038FA

evalOpS OP_CHECKSIGVERIFY =
    evalOpS OP_CHECKSIG >>
    evalOpS OP_VERIFY

evalOpS OP_CHECKMULTISIG = do
    n' <- popBSS -- total number of possible keys
    let n = decodeFailLE n' :: Word8
    assertT (BSR.length n' == 1 && n > 0 && n <= 16) "illegal n in checkmultisig"

    pub's <- popNBSS $ fi n

    m' <- popBSS -- number of signatures must be provided
    let m = decodeFailLE m' :: Word8
    assertT (BSR.length m' == 1 && m > 0 && m <= 16) "illegal m in checkmultisig"

    sig's <- popNBSS $ fi m

    -- messages for signing
    msgs <- mapM rawSigHashS sig's

    -- traceM (show (pub's, sig's))

    let sigs = map BSR.init sig's

        -- indices of successful match of public keys
        match =
            flip map (zip msgs sigs) $ \(msg, sig) ->
                findIndex (\pub' -> verifyFail pub' msg sig) pub's

        final = maybeCat match

        succ = length final == length match &&
               ascending final -- public keys are in the right order
    
    popS -- for compatibility with a historical bug

    pushBoolS succ

evalOpS OP_CHECKMULTISIGVERIFY =
    evalOpS OP_CHECKMULTISIG >>
    evalOpS OP_VERIFY

-- remove the 2nd-to-top item
evalOpS OP_NIP =
    pop2S >>= (pushS . fst)

evalOpS OP_PICK =
    popIntS >>= (pickS . fi)

evalOpS OP_SWAP =
    popNS 2 >>= mapM_ pushS

evalOpS OP_SIZE =
    peekBSS >>= (pushIntS . fi . BSR.length)

evalOpS OP_BOOLAND = do
    (a, b) <- pop2BoolS
    pushBoolS (b && a)

evalOpS OP_BOOLOR = do
    (a, b) <- pop2BoolS
    pushBoolS (b || a)

evalOpS OP_WITHIN = do
    (max, min) <- pop2IntS
    val <- popIntS
    pushBoolS (val >= min && val < max)

evalOpS OP_CODESEPARATOR =
    modify' (\s -> s {
        last_cs_op = prog_count s
    })

evalOpS (OP_PRINT msg) = traceM msg

-- end of code
evalOpS OP_EOC = return ()

evalOpS op = error $ "unimplemented " ++ show op

-- execute the current op and increment the pc
execOneS = curOpS >>= evalOpS >> incPcS

-- exec all
execS :: EvalState ()
execS = execOneS `untilM_` eocS

initState :: ScriptConf -> TxPayload -> Word32 -> ScriptState
initState conf tx idx =
    ScriptState {
        script_conf = conf,

        eval_stack = [],

        prog_code = [],
        last_cs_op = -1,
        prog_count = 0,

        -- tx_prev_out = out,
        tx_in_idx = idx,
        tx_body = tx
    }

execEval :: ScriptState -> [ScriptOp] -> Either TCKRError ScriptState
execEval init ops =
    execStateT execS (init {
        prog_code = ops,
        last_cs_op = -1,
        prog_count = 0
    }) `catchError` Left

-- whether the exec result means a positive result
isValidStack :: ScriptState -> Bool
isValidStack (ScriptState { eval_stack = top:_ }) = itemToBool top
isValidStack _ = False

data ScriptResult =
    ValidTx | InvalidTx | ExecError TCKRError deriving (Show)

instance Eq ScriptResult where
    ValidTx == ValidTx             = True
    InvalidTx == InvalidTx         = True
    (ExecError _) == (ExecError _) = True
    _ == _                         = False

eitherToResult :: Either TCKRError Bool -> ScriptResult
eitherToResult (Right True) = ValidTx
eitherToResult (Right False) = InvalidTx
eitherToResult (Left err) = ExecError err

-- NOTE: sig_script and pk_script should be executed
-- SEPARATELY, otherwise the signature verification may fail
-- because the OP_CODESEPARATOR

-- return Right res for successful execution
-- return Left err for unsuccessful exec
-- Left err | Right False -> invalid
-- Right True -> valid
runEval :: ScriptState -> [[ScriptOp]] -> ScriptResult -- Either TCKRError Bool
runEval s scripts =
        -- eval each script in order in the script list
    let exec = foldl' (>>=) (return s) . map (flip execEval) in
    eitherToResult $ do
        state <- exec scripts
         
        if isValidStack state then
            -- special cases e.g. P2SH
            isValidStack <$> specialScript state scripts
        else
            return False

specialScript :: ScriptState -> [[ScriptOp]] -> Either TCKRError ScriptState

-- P2SH
specialScript s (getScriptType -> SCRIPT_P2SH redeem) =
    if script_enable_p2sh (script_conf s) then do
        redeem_script <- decodeAllLE redeem
        ns <- execStateT popS s -- pop out result first
        execEval ns redeem_script -- exec on redeem script
    else
        -- p2sh not enabled
        return s

-- no change in state
specialScript s _ = return s

-- types of script
data ScriptType
    = SCRIPT_P2PKH
    | SCRIPT_P2PK
    | SCRIPT_P2SH ByteString -- redeem script
    | SCRIPT_P2MULTISIG
    | SCRIPT_NONSTD deriving (Show)

instance Eq ScriptType where
    SCRIPT_P2PKH == SCRIPT_P2PKH = True
    SCRIPT_P2PK == SCRIPT_P2PK = True
    (SCRIPT_P2SH _) == (SCRIPT_P2SH _) = True
    SCRIPT_P2MULTISIG == SCRIPT_P2MULTISIG = True
    SCRIPT_NONSTD == SCRIPT_NONSTD = True
    _ == _ = False

allPush :: [ScriptOp] -> Bool
allPush [] = True
allPush (OP_PUSHDATA _:rst) = allPush rst
allPush _ = False

-- (sig script, pub key script)
getScriptType :: [[ScriptOp]] -> ScriptType

-- P2PKH
-- sig_script: <signature> <public key>
--  pk_script: OP_DUP OP_HASH160 <public key hash> OP_EQUALVERIFY OP_CHECKSIG
getScriptType
    [ [ OP_PUSHDATA _, OP_PUSHDATA _ ],
      [ OP_DUP, OP_HASH160, OP_PUSHDATA _, OP_EQUALVERIFY, OP_CHECKSIG ] ]
    = SCRIPT_P2PKH

-- P2PK
-- sig_script: <signature>
--  pk_script: <public key> OP_CHECKSIG
getScriptType
    [ [ OP_PUSHDATA _ ], [ OP_PUSHDATA _, OP_CHECKSIG ] ]
    = SCRIPT_P2PK

-- P2SH
-- sig_script: just OP_PUSHDATA's
--  pk_script: OP_HASH160 <hash160(redeem script)> OP_EQUAL
getScriptType
    [ reverse -> OP_PUSHDATA redeem:(allPush -> True),
      [ OP_HASH160, OP_PUSHDATA _, OP_EQUAL ] ]
    = SCRIPT_P2SH redeem

-- P2MULTISIG
-- sig_script: OP_0 <signature 1> <signature 2>
--  pk_script: M <public key 1> <public key 2> ... <public key N> N OP_CHECKMULTISIG
getScriptType
    [ OP_PUSHDATA _:(allPush -> True),
      OP_CONST _:(reverse -> OP_CHECKMULTISIG:OP_CONST _:(allPush -> True)) ]
    = SCRIPT_P2MULTISIG

getScriptType _ = SCRIPT_NONSTD
