module Main where

import Control.Monad
import Control.Concurrent

import Tucker.Conf
import Tucker.P2P.Init

main = do
    env <- tucker_default_conf_testnet3 >>= mainLoop
    forever yield
