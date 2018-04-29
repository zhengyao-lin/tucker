module Main where

import Data.Word

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Thread.Delay

import System.Mem
import System.Environment

import Tucker.Conf
import Tucker.Util
import Tucker.Auth
import Tucker.Atom
import Tucker.Signal
import Tucker.Console
import Tucker.P2P.Init
import Tucker.Storage.Util

import Test.All
import Test.Chain
import Test.HUnit

data Flag
    = TuckerPath String
    | UseMainNet
    | UseTestNet
    | ShowHelp
    | SetJob Int
    deriving (Eq, Show)

opts :: [Option Flag]
opts = [
        WithArg [ "p", "path" ] TuckerPath "set tucker path",
        
        NoArg ["mainnet"] UseMainNet "use mainnet",
        NoArg ["testnet"] UseTestNet "use testnet",

        WithArg [ "j", "job" ] (SetJob . read) "set the number of native threads to use",

        NoArg [ "h", "help" ] ShowHelp "show this help message"
    ]

flagsToConf :: [Flag] -> IO TCKRConf
flagsToConf flags = do
    -- decide which network to use first
    let net_flag = reverse $ filter (\f -> f == UseMainNet || f == UseTestNet) flags
        mpath = reverse $ filter (\f -> case f of TuckerPath _ -> True; _ -> False) flags

    net_flag <- case listToMaybe net_flag of
        Just flag -> return flag
        Nothing -> return UseTestNet

    mpath <- case listToMaybe mpath of
        Just (TuckerPath path) -> return (Just path)
        Nothing -> return Nothing

    conf <- case net_flag of
        UseTestNet -> tucker_default_conf_testnet3 mpath
        UseMainNet -> tucker_default_conf_mainnet mpath

    conf_var <- newA conf

    forM_ flags $ \flag ->
        case flag of
            SetJob job -> setNumCapabilities job
            _ -> return ()

    getA conf_var

showHelp :: IO ()
showHelp = do
    prog_name <- getProgName

    tLnM ("usage: " ++ prog_name ++ " [options]")
    tLnM "options:"
    tLnM (genHelp opts)

main = do
    -- conf <- tucker_default_conf_testnet3 (Just "/media/rodlin/2A9967F720ACE685/tucker-data")
    -- withBlockChain conf $ \bc -> fallbackToHeight bc 827380

    args <- getArgs

    case parseFlags args opts of
        Right (flags, _) -> do
            if ShowHelp `elem` flags then
                showHelp
            else do
                conf <- flagsToConf flags
                mainLoop conf

                -- delay (600 * 1000 * 1000)
                forever yieldWait

        Left err -> do
            tLnM (show err)
            tLnM ""
            showHelp

    -- tLnM (show $ parseFlags args opts)

    -- let path =
    --         if null args then Nothing
    --         else Just (head args)

    -- conf <- tucker_default_conf_testnet3 path

    -- mainLoop conf
    -- forever yieldWait

    -- runTestTT chainTest3
