-- amateur extension to cryptonite

{-# LANGUAGE TypeFamilies, DataKinds #-}

module Crypto.Hash.NoHash where

import Data.Word
import Data.Monoid
import qualified Data.ByteArray as BA

import Foreign.Ptr
import Foreign.Storable

import Control.Monad

import Crypto.Hash
import Crypto.Hash.IO
import Crypto.Hash.Algorithms

-- do nothing but trim the data to 32 bytes
data NoHash256 = NoHash256

instance HashAlgorithm NoHash256 where
    type HashBlockSize           NoHash256 = 64
    type HashDigestSize          NoHash256 = 32
    type HashInternalContextSize NoHash256 = 32
    hashBlockSize  _          = 64
    hashDigestSize _          = 32
    hashInternalContextSize _ = 32

    hashInternalInit pctx = do
        let ptr = castPtr pctx
        pokeElemOff ptr 0 (0 :: Word64)
        pokeElemOff ptr 1 (0 :: Word64)
        pokeElemOff ptr 2 (0 :: Word64)
        pokeElemOff ptr 3 (0 :: Word64)

    -- final = last 32 bytes of (mconcat (all updates))
    hashInternalUpdate pctx pbuf size =
        if size == 0 then return ()
        else do
            let ptr = castPtr pctx

            ndat <-
                (take 32 . reverse)
                <$> mapM (peekElemOff pbuf . fromIntegral) [ 0 .. size - 1 ]

            forM_ (zip [ 31, 30 .. ] ndat) $ \(i, byte) ->
                pokeByteOff ptr i byte

    hashInternalFinalize pctx pdgst = do
        let ptr = castPtr pctx :: Ptr Word64
            buf = castPtr pdgst :: Ptr Word64
        
        peekElemOff ptr 0 >>= pokeElemOff buf 0
        peekElemOff ptr 1 >>= pokeElemOff buf 1
        peekElemOff ptr 2 >>= pokeElemOff buf 2
        peekElemOff ptr 3 >>= pokeElemOff buf 3
