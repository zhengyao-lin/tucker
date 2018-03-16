{-# LANGUAGE ViewPatterns, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Tucker.Msg.Script where

import Data.Hex
import Data.List
import Data.Word
import Data.Bits
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Exception
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
        script_enable_disabled_op        :: Bool,
        script_enable_p2sh               :: Bool,
        script_enable_trace              :: Bool,
        script_enable_csv                :: Bool,
        script_max_pubkeys_per_multisig  :: Int
    } deriving (Show)

instance Default ScriptConf where
    def = ScriptConf {
        script_enable_disabled_op = False,
        script_enable_p2sh = True,
        script_enable_trace = False,
        script_enable_csv = False,
        script_max_pubkeys_per_multisig = 20
    }

type StackItem = ByteString

class StackItemValue t where
    toItem :: t -> StackItem
    fromItem :: StackItem -> t

instance StackItemValue BSR.ByteString where
    toItem = id
    fromItem = id

instance StackItemValue Bool where
    toItem bool =
        if bool then toItem $ bchar 1
        else toItem BSR.empty -- to the shortest form

    fromItem item =
        if is_null || is_zero then False
        else True
        where
            tail = BSR.init item
            head = BSR.last item

            is_null = BSR.null item
            is_zero = BSR.all (== 0) tail && (head == 0x00 || head == 0x80)

instance StackItemValue Integer where
    -- when an item is interpreted as an integer
    -- it's little-endian and the highest bit of the last byte is the sign
    -- (not 2's complement)

    toItem int =
        if BSR.null raw then raw -- empty string
        else if last_byte .&. 0x80 /= 0 then
            -- sign bit already occupied, append one more byte
            raw <> bchar sign_mask
        else
            -- set sign
            BSR.init raw <> bchar (last_byte .|. sign_mask)
        where
            sign_mask =
                if int < 0 then 0x80
                else 0x00
    
            raw = vword2bsLE (abs int)
            last_byte = BSR.last raw

    fromItem item =
        if BSR.null item then 0
        else (if sign == 1 then negate else id) (bs2vwordLE unsigned)
        where
            last_byte = BSR.last item
            sign = last_byte `shiftR` 7 -- 1 or 0
            -- clear the highest bit
            unsigned = BSR.init item <> bchar (last_byte .&. 0x7f)
    
data ScriptState =
    ScriptState {
        script_conf :: ScriptConf,
        eval_stack  :: [StackItem],
        alt_stack   :: [StackItem],

        prog_code   :: [ScriptOp],
        prog_count  :: ScriptPc,
        last_cs_op  :: ScriptPc,

        -- tx_prev_out :: TxOutput, -- previous tx output point
        tx_in_idx   :: Word32,

        out_tx      :: TxPayload,
        cur_tx      :: TxPayload
    } deriving (Show)

type EvalState v = StateT ScriptState (Either SomeException) v

invalidTx = throwMT "invalid transaction"

confS :: (ScriptConf -> t) -> EvalState t
confS c = (c . script_conf) <$> get

modifyS :: ([StackItem] -> EvalState ([StackItem], a)) -> EvalState a
modifyS f = do
    es@(ScriptState { eval_stack = stack }) <- get
    (nstack, ret) <- f stack
    put (es { eval_stack = nstack })
    return ret

modifyAltS :: ([StackItem] -> EvalState ([StackItem], a)) -> EvalState a
modifyAltS f = do
    es@(ScriptState { alt_stack = stack }) <- get
    (nstack, ret) <- f stack
    put (es { alt_stack = nstack })
    return ret

pushS :: StackItemValue t => t -> EvalState ()
pushS v = modifyS $ \s -> return (toItem v : s, ())

pushNS :: StackItemValue t => [t] -> EvalState ()
pushNS vs = modifyS $ \s -> return (map toItem vs ++ s, ())

pushAltS :: StackItemValue t => t -> EvalState ()
pushAltS v = modifyAltS $ \s -> return (toItem v : s, ())

peekS :: StackItemValue t => EvalState t
peekS = do
    es@(ScriptState { eval_stack = stack }) <- get
    assertMT "peek on an empty stack" (not (null stack))
    return $ fromItem $ head stack

popS :: StackItemValue t => EvalState t
popS = modifyS $ \s -> do
    assertMT "pop on an empty stack" (not (null s))
    return (tail s, fromItem (head s))

popAltS :: StackItemValue t => EvalState t
popAltS = modifyAltS $ \s -> do
    assertMT "pop on an empty stack" (not (null s))
    return (tail s, fromItem (head s))

pop2S :: StackItemValue t => EvalState (t, t)
pop2S = (,) <$> popS <*> popS

popNS :: StackItemValue t => Int -> EvalState [t]
popNS = (`replicateM` popS)

pushS' = pushS :: ByteString -> EvalState ()
peekS' = peekS :: EvalState ByteString
popS'  = popS  :: EvalState ByteString
pop2S' = pop2S :: EvalState (ByteString, ByteString)
popNS' = popNS :: Int -> EvalState [ByteString]

pickS :: Int -> EvalState ()
pickS n = modifyS $ \s -> do
    assertMT "illegal pick range" (n >= 0 && n < length s)
    return ((s !! n) : s, ())

-- copy the nth item to the top
-- and remove the original item
rollS :: Int -> EvalState ()
rollS n = modifyS $ \s -> do
    assertMT "illegal pick range" (n >= 0 && n < length s)
    return ((s !! n) : take n s ++ drop (n + 1) s, ())

dupS :: EvalState ()
dupS = do
    s@(ScriptState { eval_stack = stack }) <- get

    assertMT "dup on an empty stack" (not (null stack))

    put (s { eval_stack = head stack : stack })

depthS :: EvalState Int
depthS = (length . eval_stack) <$> get

curTxS :: EvalState TxPayload
curTxS = cur_tx <$> get

outTxS :: EvalState TxPayload
outTxS = out_tx <$> get

stackS :: EvalState [StackItem]
stackS = eval_stack <$> get

pkScriptS :: EvalState [ScriptOp]
pkScriptS = do
    cur_tx   <- curTxS
    out_tx   <- outTxS
    in_idx   <- tx_in_idx <$> get

    let OutPoint _ out_idx = prev_out (tx_in cur_tx !! fi in_idx)
        script = pk_script (tx_out out_tx !! fi out_idx)

    case decodeAllLE script of
        Right ops -> return ops
        Left err -> throwMT ("wrong decoding for pk_script " ++ show err)

-- ONE time SHA256 hash of the raw tx body for signature
-- require the raw signature with the htype byte appended
txSigHashS :: [ByteString] -> ByteString -> EvalState ByteString
txSigHashS all_sigs sig_raw = do
    cur_tx   <- curTxS
    in_idx   <- tx_in_idx <$> get
    cs_op    <- last_cs_op <$> get
    code     <- prog_code <$> get

    -- traceM ("wulala " ++ show (intToHashType (BSR.last sig_raw)))

    let -- turn the current code(should be pk_script) to subscript
        -- that is used in signature
        -- defined as from the last EXECUTED OP_CODESEPARATOR to the end
        -- with all OP_CODESEPARATORs and signatures removed
        -- (https://en.bitcoin.it/w/images/en/7/70/Bitcoin_OpCheckSig_InDetail.png)

        should_keep OP_CODESEPARATOR = False
        should_keep (OP_PUSHDATA dat) = dat `notElem` all_sigs
        should_keep _ = True

        subscr = filter should_keep . drop (cs_op + 1)
         
        htype = intToHashType (BSR.last sig_raw)
        hash = txSigHash cur_tx in_idx (subscr code) htype
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

    return $
        if pc < length code then code !! pc
        else OP_EOC

    -- assertMT "counter has reached the end" (pc < length code)
    -- return (code !! pc)

-- is end-of-code
eocS :: EvalState Bool
eocS = do
    ScriptState {
        prog_code = code,
        prog_count = pc
    } <- get

    return (pc >= length code)

-- checkSig public_key_encoded message signature_encoded
checkSig :: ByteString -> ByteString -> ByteString -> Bool
checkSig pub' msg sig =
    -- trace ("CHECKSIG " ++ show (pub', msg, sig)) $
    -- NOTE: final hash is encoded in big-endian
    case decodeAllBE pub' of
        Right pub -> verifyDER pub msg sig == Right True
        Left err -> False

unaryOpS :: (StackItemValue a, StackItemValue b)
         => (a -> b) -> EvalState ()
unaryOpS f = fmap f popS >>= pushS

binaryOpS :: (StackItemValue a, StackItemValue b, StackItemValue c)
          => (a -> b -> c) -> EvalState ()
binaryOpS f = (flip f <$> popS <*> popS) >>= pushS

shiftL' a b = shiftL a (fi b) :: Integer
shiftR' a b = shiftR a (fi b) :: Integer

evalOpS :: ScriptOp -> EvalState ()

evalOpS (OP_PUSHDATA dat) = pushS dat
evalOpS (OP_CONST v) = pushS (fi v)

evalOpS OP_NOP = return ()
evalOpS (OP_IF exp ofs) = do
    top <- popS

    if top == exp then
        -- main branch
        return ()
    else -- jump to the op after the next OP_ELSE/OP_ENDIF
        ofsPcS ofs

evalOpS (OP_ELSE ofs) = ofsPcS ofs
evalOpS OP_ENDIF = return ()

evalOpS OP_VERIFY = do
    top <- popS

    if top then return ()
    else invalidTx

evalOpS OP_RETURN = invalidTx
evalOpS OP_DUP = dupS

evalOpS OP_EQUAL = binaryOpS ((==) :: ByteString -> ByteString -> Bool)

evalOpS OP_EQUALVERIFY =
    evalOpS OP_EQUAL >>
    evalOpS OP_VERIFY

evalOpS OP_RIPEMD160 = unaryOpS ripemd160
evalOpS OP_SHA1 = unaryOpS sha1
evalOpS OP_SHA256 = unaryOpS sha256
evalOpS OP_HASH160 = unaryOpS (ripemd160 . sha256)
evalOpS OP_HASH256 = unaryOpS (sha256 . sha256)

evalOpS OP_CHECKSIG = do
    (pub', sig') <- pop2S
    msg <- txSigHashS [sig'] sig'

    -- there is one byte(hash type) appended to the signature
    pushS (checkSig pub' msg (BSR.init sig'))

evalOpS OP_CHECKSIGVERIFY =
    evalOpS OP_CHECKSIG >>
    evalOpS OP_VERIFY

evalOpS OP_CHECKMULTISIG = do
    max_pubkeys <- fi <$> confS script_max_pubkeys_per_multisig

    n <- popS -- total number of possible keys
    assertMT "illegal n in checkmultisig"
        (n >= 0 && n <= max_pubkeys)

    pub's <- reverse <$> (popNS (fi n))

    m <- popS -- number of signatures must be provided
    assertMT "illegal m in checkmultisig"
        (m >= 0 && m <= max_pubkeys)

    sig's <- reverse <$> (popNS (fi m))

    -- messages for signing
    msgs <- mapM (txSigHashS sig's) sig's

    let sigs = map BSR.init sig's

        match r@(idx, rest_pub's) =
            let msg = msgs !! idx
                sig = sigs !! idx
                nres =
                    -- trace (show (hex sig, map hex rest_pub's)) $
                    dropWhile (\pub' -> not (checkSig pub' msg sig)) rest_pub's

            in if idx < length sigs then
                if null nres then (idx, []) -- no matching, failed
                else (idx + 1, tail nres) -- matched, continue
            else
                (idx, []) -- all matched, return

        -- indices of successful match of public keys
        (matched, _) =
            head $ dropWhile (not . null . snd) $
            iterate match (0, pub's)

            -- flip map (zip msgs sigs) $ \(msg, sig) ->
            --     findIndex (\pub' -> checkSig pub' msg sig) pub's

    -- traceM (show (m, n, map hex pub's, map hex sigs))
    -- traceM (show matched)

    -- for compatibility with a historical bug
    popS :: EvalState ByteString

    pushS (matched == length sigs)

evalOpS OP_CHECKMULTISIGVERIFY =
    evalOpS OP_CHECKMULTISIG >>
    evalOpS OP_VERIFY

-- remove the 2nd-to-top item
evalOpS OP_NIP = pop2S' >>= (pushS . fst)
evalOpS OP_PICK = popS >>= (pickS . fi)
evalOpS OP_SWAP = popNS' 2 >>= mapM_ pushS

evalOpS OP_TOALTSTACK = popS' >>= pushAltS
evalOpS OP_FROMALTSTACK = popAltS >>= pushS'

evalOpS OP_IFDUP = do
    b <- peekS
    if b then pushS b
    else return ()

evalOpS OP_DEPTH = depthS >>= (pushS . fi)
evalOpS OP_DROP = popS' >> return ()
evalOpS OP_OVER = pickS 1 -- second-to-top item

evalOpS OP_ROLL = popS >>= (rollS . fi)
evalOpS OP_ROT = rollS 2
evalOpS OP_TUCK = do
    (a, b) <- pop2S'
    pushNS [ a, b, a ]

evalOpS OP_2DROP = pop2S' >> return ()

evalOpS OP_2DUP = do
    (a, b) <- pop2S'
    pushNS [ a, b, a, b ]

evalOpS OP_3DUP = do
    [ a, b, c ] <- popNS' 3
    pushNS [ a, b, c, a, b, c ]

evalOpS OP_2OVER = do
    [ a, b, c, d ] <- popNS' 4
    pushNS [ c, d, a, b, c, d ]

evalOpS OP_2ROT = do
    [ a, b, c, d, e, f ] <- popNS' 6
    pushNS [ e, f, a, b, c, d ]

evalOpS OP_2SWAP = do
    [ a, b, c, d ] <- popNS' 4
    pushNS [ c, d, a, b ]

evalOpS OP_CAT = binaryOpS (BSR.append)
evalOpS OP_SUBSTR = do
    (size, begin) <- pop2S :: EvalState (Integer, Integer)
    str <- popS

    assertMT "illegal OP_SUBSTR range" $
        begin >= 0 && size >= 0 && fi (begin + size) <= BSR.length str

    pushS (BSR.take (fi size) $ BSR.drop (fi begin) str)

evalOpS OP_LEFT = do
    size <- popS :: EvalState Integer
    str <- popS

    assertMT "illegal OP_LEFT size" (fi size <= BSR.length str)

    pushS (BSR.take (fi size) str)

evalOpS OP_RIGHT = do
    size <- popS :: EvalState Integer
    str <- popS

    let len = BSR.length str
    assertMT "illegal OP_RIGHT size" (fi size <= len)

    pushS (BSR.drop (len - fi size) str)

evalOpS OP_SIZE = evalOpS OP_DUP >> unaryOpS (fi . BSR.length)

evalOpS OP_BOOLAND = binaryOpS (&&)
evalOpS OP_BOOLOR = binaryOpS (||)

evalOpS OP_WITHIN = do
    (max, min) <- pop2S
    val <- popS
    pushS (val >= min && val < max)

evalOpS OP_1ADD = unaryOpS (\x -> x + 1)
evalOpS OP_1SUB = unaryOpS (\x -> x - 1)
evalOpS OP_2MUL = unaryOpS (*2)
evalOpS OP_2DIV = unaryOpS half
    
evalOpS OP_NEGATE = unaryOpS negate
evalOpS OP_ABS = unaryOpS abs
    
evalOpS OP_NOT = unaryOpS not
evalOpS OP_0NOTEQUAL = unaryOpS (id :: Bool -> Bool)

evalOpS OP_ADD = binaryOpS (+)
evalOpS OP_SUB = binaryOpS (-)
evalOpS OP_MUL = binaryOpS (*)
evalOpS OP_DIV = binaryOpS div
evalOpS OP_MOD = binaryOpS mod

evalOpS OP_LSHIFT = binaryOpS shiftL'
evalOpS OP_RSHIFT = binaryOpS shiftR'

evalOpS OP_NUMEQUAL = binaryOpS (==)
evalOpS OP_NUMNOTEQUAL = binaryOpS (/=)
evalOpS OP_NUMEQUALVERIFY = evalOpS OP_NUMEQUAL >> evalOpS OP_VERIFY

evalOpS OP_LESSTHAN = binaryOpS (<)
evalOpS OP_GREATERTHAN = binaryOpS (>)
evalOpS OP_LESSTHANOREQUAL = binaryOpS (<=)
evalOpS OP_GREATERTHANOREQUAL = binaryOpS (>=)

evalOpS OP_MIN = binaryOpS min
evalOpS OP_MAX = binaryOpS max

evalOpS OP_CODESEPARATOR =
    modify' (\s -> s {
        last_cs_op = prog_count s
    })

evalOpS OP_CHECKLOCKTIMEVERIFY = do
    csv <- confS script_enable_csv

    if csv then do
        return ()
        -- lt <- peekS
        -- in_idx <- tx_in_idx <$> get
        -- cur_tx <- curTxS

        -- let lt1 = lock_time cur_tx
        --     sequence = seqn (tx_in cur_tx !! fi in_idx)

        -- assertMT "invalid locktime value" $ not $
        --     lt < 0 ||
        --     ((lt <= 500000000) /= (lt1 <= 500000000)) ||
        --     sequence == 0xffffffff

        -- assertMT "unmatched locktime" $
        --     fi lt <= lt1
    else
        -- treated as OP_NOP2
        return ()

evalOpS OP_CHECKSEQUENCEVERIFY = do
    csv <- confS script_enable_csv
    
    if csv then do
        return ()
        -- in_idx <- tx_in_idx <$> get
        -- cur_tx <- curTxS
        -- out_tx <- outTxS
        -- span   <- peekS
        
        -- let sequence = seqn (tx_in cur_tx !! fi in_idx)
        --     lt0 = lock_time out_tx
        --     lt1 = lock_time cur_tx

        -- assertMT "invalid span/locktime" $ not $
        --     span < 0 ||
        --     not (span .&. (1 `shiftL` 31) == 0 &&
        --         (version cur_tx < 2 ||
        --         sequence .&. (1 `shiftL` 31) /= 0 ||
        --         (lt0 <= 500000000) /= (lt1 <= 500000000) ||
        --         fi span > lt1 - lt0))
    else
        return ()

evalOpS (OP_PRINT msg) = traceM msg

evalOpS OP_NOP1 = return ()
evalOpS OP_NOP4 = return ()
evalOpS OP_NOP5 = return ()
evalOpS OP_NOP6 = return ()
evalOpS OP_NOP7 = return ()
evalOpS OP_NOP8 = return ()
evalOpS OP_NOP9 = return ()
evalOpS OP_NOP10 = return ()

-- end of code
evalOpS OP_EOC = return ()

evalOpS (OP_UNKNOWN byte) =
    throwMT ("OP_UNKNOWN " ++ show byte ++ " executed")

evalOpS op = error $ "unimplemented " ++ show op

checkValidOp :: ScriptOp -> EvalState ScriptOp
checkValidOp op = do
    -- depth <- depthS
    -- top <- if depth > 0 then peekS else return (BS.pack "no elem")
    -- stack <- eval_stack <$> get
    -- traceM $ "exec " ++ show op ++ ": " ++ show stack

    enable_trace <- confS script_enable_trace

    if enable_trace then do
        stack <- stackS
        traceM $ "[trace] exec " ++ show op ++ " " ++ show stack
    else
        return ()

    use_disabled <- confS script_enable_disabled_op

    if use_disabled && op `elem` disabled_ops then
        throwMT ("(non-strict mode)disabled op " ++ show op)
    else
        return op

-- execute the current op and increment the pc
execOneS = curOpS >>= checkValidOp >>= evalOpS >> incPcS

-- exec all
execS :: EvalState ()
execS = execOneS `untilM_` eocS

initState :: ScriptConf -> TxPayload -> TxPayload -> Word32 -> ScriptState
initState conf out_tx cur_tx idx =
    ScriptState {
        script_conf = conf,

        eval_stack = [],
        alt_stack = [],

        prog_code = [],
        last_cs_op = -1,
        prog_count = 0,

        -- tx_prev_out = out,
        tx_in_idx = idx,
        out_tx = out_tx,
        cur_tx = cur_tx
    }

execEval :: ScriptState -> [ScriptOp] -> Either TCKRError ScriptState
execEval init ops =
    toTCKRErrorM $
    execStateT execS (init {
        prog_code = ops,
        last_cs_op = -1,
        prog_count = 0
    }) `catchT` (Left . toException)

-- whether the exec result means a positive result
isValidStack :: ScriptState -> Bool
isValidStack (ScriptState { eval_stack = top:_ }) = fromItem top
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

        -- traceM (show redeem_script)
        
        ns <- toTCKRErrorM (execStateT popS' s) -- pop out result first
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
