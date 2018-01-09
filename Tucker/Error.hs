module Tucker.Error where

newtype TCKRError = TCKRError String deriving (Eq)

instance Show TCKRError where
    show (TCKRError err) = "tucker error: " ++ err
