module Tucker.Msg.Script where

import Data.Hex
import Data.List
import Data.Word
import qualified Data.ByteString as BSR

import Control.Monad
import Control.Monad.State
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

type StackItem = ByteString

bsToItem = id

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

data ScriptState =
    ScriptState {
        eval_stack  :: [StackItem],

        tx_prev_out :: TxOutput, -- previous tx output point
        tx_in_idx   :: Word32,

        tx_body     :: TxPayload
    } deriving (Show)

type EvalState v = StateT ScriptState TCKRErrorM v

{-

- OP_PUSHDATA ByteString
- OP_CONST Word8

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
| OP_CHECKMULTISIG
| OP_CHECKMULTISIGVERIFY

-}

invalidTx = failT "invalid transaction"

trueS = bsToItem $ bchar 1
falseS = bsToItem $ bchar 0

pushS :: StackItem -> EvalState ()
pushS item =
    modify $ \s ->
        s { eval_stack = item : eval_stack s }

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
    cur_tx <- txS
    in_idx <- tx_in_idx <$> get
    prev_out <- tx_prev_out <$> get

    let htype = intToHashType (BSR.last sig_raw)
        rawtx = sigRawTx cur_tx in_idx prev_out htype
        hash = ba2bs $ sha256 rawtx
        -- NOTE: only sha256 it ONCE because there's another
        -- hashing in the verification process

    return hash

-- verifyFail public_key_encoded message signature_encoded
verifyFail :: ByteString -> ByteString -> ByteString -> Bool
verifyFail pub msg sig =
    -- NOTE: final hash is encoded in big-endian
    verifySHA256DER (decodeFailBE pub) msg sig == Right True

evalOpS :: ScriptOp -> EvalState ()
evalOpS (OP_PUSHDATA dat) = pushS $ bsToItem dat
evalOpS (OP_CONST v) = pushS $ bsToItem (bchar v)

evalOpS OP_NOP = return ()
evalOpS (OP_IF exp b1 b2) = do
    top <- popS

    if itemToBool top == exp then
        evalS b1
    else
        evalS b2

evalOpS OP_VERIFY = do
    top <- popS

    if itemToBool top then return ()
    else invalidTx

evalOpS OP_RETURN = invalidTx
evalOpS OP_DUP = dupS

evalOpS OP_EQUAL = do
    (a, b) <- pop2S

    if a == b then pushS trueS
    else pushS falseS

evalOpS OP_EQUALVERIFY =
    evalOpS OP_EQUAL >>
    evalOpS OP_VERIFY

evalOpS OP_HASH160 = do
    val <- popS
    pushS $ bsToItem $ ba2bs $ ripemd160 $ sha256 val

evalOpS OP_HASH256 = do
    val <- popS
    pushS $ bsToItem $ ba2bs $ sha256 $ sha256 val

evalOpS OP_CHECKSIG = do
    (pub', sig') <- pop2S
    msg <- rawSigHashS sig'
    -- there is one byte(hash type) appended to the signature
    pushS $ boolToItem (verifyFail pub' msg (BSR.init sig'))

evalOpS OP_CHECKSIGVERIFY =
    evalOpS OP_CHECKSIG >>
    evalOpS OP_VERIFY

evalOpS OP_CHECKMULTISIG = do
    n' <- popS -- total number of possible keys
    let n = decodeFailLE n' :: Word8
    assertT (BSR.length n' == 1 && n > 0 && n <= 16) "illegal n in checkmultisig"

    pub's <- popNS $ fi n

    m' <- popS -- number of signatures must be provided
    let m = decodeFailLE m' :: Word8
    assertT (BSR.length m' == 1 && m > 0 && m <= 16) "illegal m in checkmultisig"

    sig's <- popNS $ fi m

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

    pushS $ boolToItem succ

evalOpS OP_CHECKMULTISIGVERIFY =
    evalOpS OP_CHECKMULTISIG >>
    evalOpS OP_VERIFY

evalOpS (OP_PRINT msg) = traceM msg

evalS :: [ScriptOp] -> EvalState ()
evalS ops =
    foldl' (>>) (return ()) (map evalOpS ops)

initState :: TxPayload -> Word32 -> TxOutput -> ScriptState
initState tx idx out =
    ScriptState {
        eval_stack = [],
        tx_prev_out = out,
        tx_in_idx = idx,
        tx_body = tx
    }

execEval :: ScriptState -> [ScriptOp] -> Either TCKRError ScriptState
execEval init ops =
    execStateT (evalS ops) init `catchError` Left

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

-- return Right res for successful execution
-- return Left err for unsuccessful exec
-- Left err | Right False -> invalid
-- Right True -> valid
runEval :: ScriptState -> [ScriptOp] -> ScriptResult -- Either TCKRError Bool
runEval s ops = eitherToResult (isValidStack <$> execEval s ops)
