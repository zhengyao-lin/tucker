{-# LANGUAGE ViewPatterns, ExtendedDefaultRules #-}

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
        script_enable_segwit             :: Bool,
        script_enable_v0_wit_sig         :: Bool, -- use version 0 witness program signature serialization
        script_max_pubkeys_per_multisig  :: Int
    } deriving (Show)

instance Default ScriptConf where
    def = ScriptConf {
        script_enable_disabled_op = False,
        script_enable_p2sh = True,
        script_enable_trace = False,
        script_enable_csv = False,
        script_enable_segwit = False,
        script_enable_v0_wit_sig = False,
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

        prev_tx_out :: TxOutput,
        cur_tx      :: TxPayload
    } deriving (Show)

type EvalState v = StateT ScriptState (Either SomeException) v

popM' :: ScriptState -> TCKRErrorM (ByteString, ScriptState)
popM' s = toTCKRErrorM (runStateT popS' s)

popM :: StackItemValue v => ScriptState -> TCKRErrorM (v, ScriptState)
popM s = toTCKRErrorM (runStateT popS s)

pushM :: StackItemValue v => ScriptState -> v -> TCKRErrorM ScriptState
pushM s v = toTCKRErrorM (execStateT (pushS v) s)

-- NOTE: the order of push is from right to left
pushNM :: StackItemValue v => ScriptState -> [v] -> TCKRErrorM ScriptState
pushNM s vs = toTCKRErrorM (execStateT (pushNS vs) s)

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

-- outTxS :: EvalState TxPayload
-- outTxS = out_tx <$> get

stackS :: EvalState [StackItem]
stackS = eval_stack <$> get

pkScriptS :: EvalState [ScriptOp]
pkScriptS = do
    cur_tx      <- curTxS
    prev_tx_out <- prev_tx_out <$> get
    in_idx      <- tx_in_idx <$> get

    let OutPoint _ out_idx = prev_out (tx_in cur_tx !! fi in_idx)
        script = pk_script prev_tx_out

    case decodeAllLE script of
        Right ops -> return ops
        Left err -> throwMT ("wrong decoding for pk_script " ++ show err)

txSigHashS :: [ByteString] -> ByteString -> EvalState ByteString
txSigHashS all_sigs sig_raw = do
    v0_wit <- confS script_enable_v0_wit_sig

    if v0_wit then
        -- use v0 witness signature
        txSigHashV0WitS sig_raw
    else
        -- use te legacy signature
        txSigHashLegacyS all_sigs sig_raw

txSigHashV0WitS :: ByteString -> EvalState ByteString
txSigHashV0WitS sig_raw = do
    cur_tx   <- curTxS
    in_idx   <- tx_in_idx <$> get
    prev_out <- prev_tx_out <$> get
    cs_op    <- last_cs_op <$> get
    code     <- prog_code <$> get

    let htype = intToHashType (BSR.last sig_raw)
        pk_script = encodeLE (drop (cs_op + 1) code)
        script_code = encodeLE (VInt $ fi $ BSR.length pk_script) <> pk_script
        -- note the script code signed here is serialized with
        -- its length prepended as a VInt
        hash = txSigHashV0Wit cur_tx (fi in_idx) prev_out htype script_code

    return hash

-- ONE time SHA256 hash of the raw tx body for signature
-- require the raw signature with the htype byte appended
txSigHashLegacyS :: [ByteString] -> ByteString -> EvalState ByteString
txSigHashLegacyS all_sigs sig_raw = do
    cur_tx <- curTxS
    in_idx <- tx_in_idx <$> get
    cs_op  <- last_cs_op <$> get
    code   <- prog_code <$> get

    -- tLnM ("wulala " ++ show (intToHashType (BSR.last sig_raw)))

    let -- turn the current code(should be pk_script) to subscript
        -- that is used in signature
        -- defined as from the last EXECUTED OP_CODESEPARATOR to the end
        -- with all OP_CODESEPARATORs and signatures removed
        -- (https://en.bitcoin.it/w/images/en/7/70/Bitcoin_OpCheckSig_InDetail.png)

        should_keep OP_CODESEPARATOR = False
        should_keep (OP_PUSHDATA dat _) = dat `notElem` all_sigs
        should_keep _ = True

        subscr = filter should_keep . drop (cs_op + 1)
         
        htype = intToHashType (BSR.last sig_raw)
        hash = txSigHashLegacy cur_tx (fi in_idx) (subscr code) htype
        -- NOTE: only sha256 it ONCE because there's another
        -- hashing in the verification process

    -- tLnM (show (hex rawtx))

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
    -- tLn ("CHECKSIG " ++ show (pub', msg, sig)) $
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

evalOpS (OP_PUSHDATA dat _) = pushS dat
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
evalOpS OP_HASH256 = unaryOpS doubleSHA256

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
                    -- tLn (show (hex sig, map hex rest_pub's)) $
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

    -- tLnM (show (m, n, map hex pub's, map hex sigs))
    -- tLnM (show matched)

    -- for compatibility with a historical bug
    dummy <- popS :: EvalState ByteString
    
    -- tLnM ("n: " ++ show n ++ ", m: " ++ show m)

    -- dummy == null
    segwit <- confS script_enable_segwit
    assertMT ("nulldummy rule not satisfied(" ++ show dummy ++ " is on the stack)") $
        not segwit || BSR.null dummy

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
        -- error "OP_CHECKLOCKTIMEVERIFY used"
        in_idx <- tx_in_idx <$> get
        cur_tx <- curTxS

        let in_seqn = seqn (tx_in cur_tx !! fi in_idx)

        expect_lt <- numToLockTime <$> fi <$> peekS

        assertMT "sequence should not be max int" $
            in_seqn /= maxBound

        assertMT "lock-time not match" $
            txLockTime cur_tx `isLockTimeAtLeast` expect_lt
    else
        -- treated as OP_NOP2
        return ()

evalOpS OP_CHECKSEQUENCEVERIFY = do
    csv <- confS script_enable_csv
    
    if csv then do
        m_expect_lt <- sequenceToRelLockTime <$> fi <$> peekS

        in_idx <- tx_in_idx <$> get
        cur_tx <- curTxS
        let m_in_lt = inputRelLockTime (tx_in cur_tx !! fi in_idx)

        case m_expect_lt of
            Just expect_lt ->
                case m_in_lt of
                    Just in_lt -> do
                        assertMT "tx version not met" $
                            version cur_tx >= 2
                        
                        assertMT ("relative lock-times not match. expect " ++ show expect_lt ++
                                  ", given " ++ show in_lt) $
                            in_lt `isRelLockTimeAtLeast` expect_lt

                        -- assertMT "comparing relative lock-times with different types" $
                        --     rlockTimeType expect_lt == rlockTimeType in_lt

                        -- assertMT "relative lock time requirement not met" $
                        --     rlockTimeValue expect_lt <= rlockTimeValue in_lt

                    Nothing ->
                        throwMT "relative lock time not used in tx"

            Nothing -> return ()
    else
        return ()

evalOpS (OP_PRINT msg) = tLnM msg

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
    -- tLnM $ "exec " ++ show op ++ ": " ++ show stack

    enable_trace <- confS script_enable_trace

    if enable_trace then do
        stack <- stackS
        tLnM $ "[trace] exec " ++ show op ++ " " ++ show stack
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

initState :: ScriptConf -> TxOutput -> TxPayload -> Word32 -> ScriptState
initState conf prev_out cur_tx idx =
    ScriptState {
        script_conf = conf,

        eval_stack = [],
        alt_stack = [],

        prog_code = [],
        last_cs_op = -1,
        prog_count = 0,

        -- tx_prev_out = out,
        tx_in_idx = idx,
        prev_tx_out = prev_out,
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

shouldEnableWitness :: ScriptState -> Bool
shouldEnableWitness s =
    script_enable_segwit (script_conf s) &&
    isJust (getWitness (cur_tx s) (fi (tx_in_idx s)))

enableV0Wit :: ScriptState -> ScriptState
enableV0Wit s =
    s {
        script_conf = (script_conf s) {
            script_enable_v0_wit_sig = True
        }
    }

-- assuming the previous result on the stack is cleaned
verifyWitness :: ScriptState -> ByteString -> Either TCKRError ScriptState

-- P2WPKH
verifyWitness s wit@(BSR.length -> 20) = do
    let Just (TxWitness items) = getWitness (cur_tx s) (fi (tx_in_idx s))
        sig' = items !! 0
        pub' = items !! 1

    assertMT "no enough stack items for P2WPKH validation(require 2)" $
        length items >= 2

    -- assertMT "hash160 of the public key does not match the required address" $
    --     (ripemd160 . sha256) pub' == wit

    -- set v0 witness signature support
    let ns = enableV0Wit s

    -- push back two witness data
    ns <- pushM ns sig'
    ns <- pushM ns pub'
    execEval ns [
            OP_DUP, OP_HASH160, OP_PUSHDATA wit Nothing,
            OP_EQUALVERIFY, OP_CHECKSIG
        ]

    -- fail "P2WPKH verification not supported yet"

-- P2WSH
verifyWitness s wit@(BSR.length -> 32) = do
    let Just (TxWitness items) = getWitness (cur_tx s) (fi (tx_in_idx s))
        wit_script_raw = last items
        args = init items

    assertMT "no enough stack items for P2WSH validation(require at least 1)" $
        length items >= 1

    assertMT "witness script hash not match" $
        sha256 wit_script_raw == wit

    wit_script <- decodeAllLE wit_script_raw

    let ns = enableV0Wit s

    -- tLnM ("P2WSH arguments: " ++ show args)
    -- tLnM ("P2WSH wit script: " ++ show wit_script)
    
    ns <- pushNM ns (reverse args)
    execEval ns wit_script

verifyWitness _ _ = fail "illegal witness program length"

specialScript :: ScriptState -> [[ScriptOp]] -> Either TCKRError ScriptState

-- P2SH
specialScript s (getScriptType -> SCRIPT_P2SH redeem) =
    if script_enable_p2sh (script_conf s) then do
        redeem_script <- decodeAllLE redeem

        (use_wit, wit) <-
            if shouldEnableWitness s then
                case parseWitnessProgram redeem_script of
                    Just wit -> return (True, wit)
                    Nothing -> return (False, undefined)
            else
                return (False, undefined)

        (_, ns) <- popM' s -- pop out result first

        if use_wit then
            verifyWitness ns wit
        else
            execEval ns redeem_script -- exec on redeem script
    else
        -- p2sh not enabled
        return s

-- sig_script is empty and pk_script is a witness program
specialScript s@(shouldEnableWitness -> True)
              ([ [], parseWitnessProgram -> Just wit ]) = do
    (_, ns) <- popM' s -- pop out result first
    verifyWitness ns wit
    
-- no change in state
specialScript s _ = return s

{-

some notes on segwit

https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki

1. a new structure in tx called tx_witness is introduced(encoding/decoding has already been implemented)
2. tx_witness is essentially a list of stacks each of which corresponds to an input
    [ [ stack items for input 0 ], [ stack items for input 1 ] ]
   and is used later

3. P2PKH and P2SH are `lifted` to their new version, P2WPKH and P2WSH

4. an essential part called the 'witness program' can be extracted from every segwit tx
   (note: it's not a program/script(but it should have a valid script syntax), but a specific structure for validation use)

5. a valid witness program consists ONLY of
    <1 byte push opcode> <20- or 32-byte data>

6. witness programs can be extracted in two ways(for different usage):
    1) pk_script is a witness program and sig_script is empty
    2) in a P2SH tx, the redeem script is a witness program, and sig_script can only have one push(redeem script)

7. VERIFICATION:
    1) if witness program data is 20 bytes
        a) the witness stack must be 2 pushes corresponding to a signature and a pubkey
        b) the public key matches the witness program data
        c) check signature and the public key

    2) if the witness program data is 32 bytes
        a) the witness stack can have multiple pushes but with last one as the new redeem script
        b) the SHA256 of the redeem script must match the witness program data
        c) run the redeem with the other stack items and the result must be a SINGLE TRUE

8. Block weight(see the bip page for more detail)

9. ONLY COMPRESSED PUBLIC KEYS ARE ALLOWED in P2WPKH and P2WSH

10. and a freaking NEW tx signing/checking process!
    at https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki

-}

{-

Notes on BIP 68, CHECKSEQUENCEVERIFY

1. redesigned purpose of sequence field

disable                type
flag 31                flag 22              value(0-15)
[     ][][][][][][][][][     ][][][][][][]  [][][][][][][][][][][][][][][]

if type == 1 then
    value has unit 512 sec
else
    value is the number of blocks


the tx must not be a coinbase

-}

{-

NOTES for lightening network

1) Unidirectional channel(Alice to Bob)
    1. Alice creates a funding tx(multisig 2-2) and a refund tx(with a timelock in the future)
    2. Alice gives the two txns to Bob and asks him to sign the refund tx
    3. And from this point, the channel is open
    4. Alice can repeatedly create commitment txns(with no locktime) indicating
       the final allocation of tx and send them to Bob for his signature.
    5. And if anyone wants to close the channel, he/she just broadcast the final commitment
    6. Bob has no incentive to cheat because the worst case for Alice is to get her money back in
       time indicated by the timelock in the refund tx

    * the point of this channel is that there could only be Alice sending funds to Bob but not the
      other way around. So Bob has no incentive to use an older commitment tx to close the channel

2) Bidirectional channel(Alice to Bob or the reversed way)
    1. Similar to an unidirectional channel, except that the commitment txns has decreasing timelocks
       (older commitment txns have smaller timelocks, so that if any party cheats, the other can send
        the newest tx and it can acts faster)

3) RSMC(Revocable Sequence Maturity Contract) works in a similar way to Bidirectional channel

4) Above these bi-party channels, we can have a route across indefinitely many middle parties
   And HTLC can ensure no party across this route cheats

-}
