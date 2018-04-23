module Main where

import Data.Word

import Control.Monad
import Control.Concurrent

import System.Mem
import System.Environment

import Tucker.Conf
import Tucker.Util
import Tucker.Auth
import Tucker.P2P.Init
import Tucker.Storage.Util

import Test.All
import Test.Chain
import Test.HUnit

main = do
    -- conf <- tucker_default_conf_testnet3 (Just "/media/rodlin/2A9967F720ACE685/tucker-data")
    -- withBlockChain conf $ \bc -> fallbackToHeight bc 827380

    args <- getArgs
    
    let path =
            if null args then Nothing
            else Just (head args)

    conf <- tucker_default_conf_testnet3 path

    mainLoop conf
    forever yield

    -- runTestTT chainTest3
