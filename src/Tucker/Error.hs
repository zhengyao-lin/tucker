module Tucker.Error where

import Control.Exception

import Tucker.DeepSeq

newtype TCKRError = TCKRError String deriving (Eq)

instance Show TCKRError where
    show (TCKRError err) = "tucker error: " ++ err

instance Exception TCKRError

instance NFData TCKRError where
    rnf (TCKRError err) = rnf err

wrapError :: TCKRError -> String -> TCKRError
wrapError (TCKRError err) prep =
    TCKRError (prep ++ ": " ++ err)
