{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Tucker.Error where

import Control.DeepSeq
import Control.Exception

import GHC.Generics (Generic)

newtype TCKRError = TCKRError String deriving (Eq, Generic, NFData)

instance Show TCKRError where
    show (TCKRError err) = "tucker error: " ++ err

instance Exception TCKRError

wrapError :: TCKRError -> String -> TCKRError
wrapError (TCKRError err) prep =
    TCKRError (prep ++ ": " ++ err)
