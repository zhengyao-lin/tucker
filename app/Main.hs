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

    args' <- getArgs

    let (mtool, args) =
            case args' of
                "tool":tool:rst ->
                    if not ("-" `isPrefixOf` tool) then (Just tool, rst)
                    else (Nothing, args')
                _ -> (Nothing, args')

    case parseFlags args opts of
        Right (flags, non_opt) ->
            if ShowHelp `elem` flags then
                showHelp
            else do
                conf <- flagsToConf flags

                case mtool of
                    Nothing -> void (mainLoop conf)
                    Just tool -> execTool conf tool non_opt

        Left err -> do
            tLnM (show err)
            tLnM ""
            showHelp
