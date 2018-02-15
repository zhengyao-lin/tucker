module Main where

import Control.Monad
import Control.Concurrent

import System.Mem

import Tucker.Conf
import Tucker.P2P.Init
import Tucker.Test

main = do
    -- tmp

    -- performMajorGC

    -- forever yield

    tucker_default_conf_testnet3 >>= mainLoop
    forever yield
