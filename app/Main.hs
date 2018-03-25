module Main where

import Control.Monad
import Control.Concurrent

import System.Mem
import System.Environment

import Tucker.Conf
import Tucker.Util
import Tucker.P2P.Init

import Test.All

main = do
    args <- getArgs
    
    let path =
            if null args then Nothing
            else Just (head args)

    conf <- tucker_default_conf_testnet3 path

    mainLoop conf
    forever yield
