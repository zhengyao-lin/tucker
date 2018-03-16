module Main where

import Control.Monad
import Control.Concurrent

import System.Mem

import Tucker.Conf
import Tucker.P2P.Init

import Test.All

main = do
    tucker_default_conf_testnet3 >>= mainLoop
    forever yield
