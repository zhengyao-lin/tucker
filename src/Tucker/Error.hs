{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Tucker.Error where

import Control.Exception
import Control.Monad.Fail
import Control.Monad.Except

import Tucker.DeepSeq

newtype TCKRError = TCKRError String deriving (Eq)

type TCKRErrorM = Either TCKRError

instance MonadFail TCKRErrorM where
    fail = Left . TCKRError

instance Show TCKRError where
    show (TCKRError err) = "tucker error: " ++ err

instance Exception TCKRError

instance NFData TCKRError where
    rnf (TCKRError err) = rnf err

wrapError :: TCKRError -> String -> TCKRError
wrapError (TCKRError err) prep =
    TCKRError (prep ++ ", " ++ err)

failT :: MonadError TCKRError m => String -> m a
failT = throwError . TCKRError

assertT :: MonadError TCKRError m => String -> Bool -> m ()
assertT msg cond =
    if cond then return ()
    else failT msg
