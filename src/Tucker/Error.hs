{-# LANGUAGE FlexibleInstances, CPP #-}

module Tucker.Error where

import Data.Maybe

import Control.Exception hiding (catch, try)
-- import Control.Monad.Fail
import Control.Monad.Catch
-- import Control.Monad.Except

import System.Exit

import Tucker.DeepSeq

newtype TCKRError = TCKRError String deriving (Eq)

type TCKRErrorM = Either TCKRError

-- instance MonadFail TCKRErrorM where
--     fail = Left . TCKRError

instance Show TCKRError where
    show (TCKRError err) = "tucker error: " ++ err

instance Exception TCKRError

instance NFData TCKRError where
    rnf (TCKRError err) = rnf err

wrapError :: TCKRError -> String -> TCKRError
wrapError (TCKRError err) prep =
    TCKRError (prep ++ ", " ++ err)

toTCKRErrorM :: Either SomeException v -> TCKRErrorM v
toTCKRErrorM (Right v) = Right v
toTCKRErrorM (Left err) = Left (TCKRError (show err))

#define IS_EXCEPTION(e, t) (isJust (fromException (e) :: Maybe (t)))

-- catches all errors and turn them into TCKRError
-- except ErrorCall and ExitCode
catchT :: MonadCatch m => m a -> (TCKRError -> m a) -> m a
catchT action handler =
    catch action proc
    where
        fail e = error ("catch failed with exception: " ++ show e)
        proc e
            | shouldExitOn e = fail e
            | otherwise = handler (TCKRError (show e))

shouldExitOn :: SomeException -> Bool
shouldExitOn e
    | IS_EXCEPTION(e, ErrorCall) = True
    | IS_EXCEPTION(e, ExitCode) = True
    | otherwise = False

tryT :: MonadCatch m => m a -> m (Either TCKRError a)
tryT action = catchT (Right <$> action) (return . Left)

-- NOTE: throwT is a stronger throw that
-- is generally not safe because it can
-- only be caught by the catch in IO monad
throwT :: String -> a
throwT = throw . TCKRError

-- a more gentle monad throw
throwMT :: MonadThrow m => String -> m a
throwMT = throwM . TCKRError

assertT :: String -> Bool -> a -> a
assertT msg cond code =
    if cond then code
    else throwT msg

assertMT :: MonadThrow m => String -> Bool -> m ()
assertMT msg cond =
    if cond then return ()
    else throwMT msg
