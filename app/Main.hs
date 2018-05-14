module Main where

import Data.Word

import Control.Monad
import Control.Concurrent

import System.Environment

import Tucker.Conf
import Tucker.Util
import Tucker.Auth
import Tucker.Atom
import Tucker.Signal
import Tucker.Console
import Tucker.P2P.Init
import Tucker.Storage.Util

data Flag
    = TuckerPath String
    | UseMainNet
    | UseTestNet
    | EnableMiner Bool
    | EnableMinDiff Bool
    | EnableMemPool Bool
    | ShowHelp
    | SetJob Int
    deriving (Eq, Show)

opts :: [Option Flag]
opts = [
        WithArg [ "p", "path" ] TuckerPath "set tucker path",
        
        NoArg ["mainnet"] UseMainNet "use mainnet",
        NoArg ["testnet"] UseTestNet "use testnet(in default)",

        WithArg ["enable-miner"] EnableMiner "enable miner[TRUE/false]",
        WithArg ["enable-min-diff"] EnableMinDiff
            "use min-diff rule to mine(only available when min-diff is enabled on the net)[true/false]",

        WithArg ["enable-mempool"] EnableMemPool
            "enable tx mem pool(usually for mining)[TRUE/false]",

        WithArg [ "j", "job" ] SetJob "set the number of native threads to use",

        NoArg [ "h", "help" ] ShowHelp "show this help message"
    ]

def_flags =
    [ EnableMiner True, EnableMemPool True ]

flagsToConf :: [Flag] -> IO TCKRConf
flagsToConf flags' = do
    let flags = def_flags ++ flags'

        -- decide which network to use first
        net_flag = reverse $ filter (\f -> f == UseMainNet || f == UseTestNet) flags
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
            SetJob job -> void $
                appA (\conf -> conf { tckr_job_number = job }) conf_var

            EnableMiner bool -> void $
                appA (\conf -> conf { tckr_enable_miner = bool }) conf_var

            EnableMinDiff bool ->
                when_ (tckr_use_special_min_diff conf) $
                    appA (\conf -> conf { tckr_use_special_min_diff_mine = bool }) conf_var

            EnableMemPool bool -> void $
                appA (\conf -> conf { tckr_enable_mempool = bool }) conf_var

            _ -> return ()

    getA conf_var

showHelp :: IO ()
showHelp = do
    prog_name <- getProgName

    tLnM ("usage: " ++ prog_name ++ " [options]")
    tLnM "options:"
    tLnM (genHelp opts)

main = do
    -- runTestTT chainTest4

    args <- getArgs

    case parseFlags args opts of
        Right (flags, _) ->
            if ShowHelp `elem` flags then
                showHelp
            else void $ do
                conf <- flagsToConf flags
                mainLoop conf

        Left err -> do
            tLnM (show err)
            tLnM ""
            showHelp
