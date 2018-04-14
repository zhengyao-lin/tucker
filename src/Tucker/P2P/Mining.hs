{-# LANGUAGE ForeignFunctionInterface #-}

-- mining utils

module Tucker.P2P.Mining where

import Data.Word
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BSR

import Control.Monad
import Control.Concurrent

import Foreign.Ptr

import Debug.Trace

import Tucker.Msg
import Tucker.Enc
import Tucker.Auth
import Tucker.Util
import Tucker.Atom

foreign import ccall "do_mine" c_do_mine :: Ptr Word8 -> Word64 -> Ptr Word8 -> Int -> Word32

doMineBlock :: Block -> IO Block
doMineBlock block = do
    let fixed = blockFixedHeader block
        target = encodeLE (hash_target block)

    nonce <-
        BA.withByteArray fixed $ \dat -> do
            BA.withByteArray target $ \target -> do
                return (c_do_mine dat (fi (BSR.length fixed)) target 2)

    return (updateBlockHashes $ block { nonce = nonce })
    