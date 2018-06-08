module Main where

import Data.Word
import Data.List

import Control.Monad
import Control.Concurrent

import System.Environment

import Tucker.Conf
import Tucker.Util
import Tucker.Crypto
import Tucker.Signal
import Tucker.Console
import Tucker.P2P.Init
import Tucker.State.Util

import Flag
import Tool

main = do
    -- runTestTT chainTest4

    args <- getArgs

    case args of
        "tool":rst -> findAndExecTool rst
        args ->
            case parseFlags args chain_opts of
                Right (flags, non_opt) ->
                    if ShowHelp `elem` flags then
                        showHelp [] [] chain_opts
                    else
                        flagsToConf flags >>= mainLoopForever
        
                Left err -> do
                    tLnM (show err)
                    tLnM ""
                    showHelp [] [] chain_opts
