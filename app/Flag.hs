module Flag where

import Control.Monad

import System.Environment

import Tucker.Conf
import Tucker.Util
import Tucker.Atom
import Tucker.Console

-- all possible flags that can be used
data Flag
    = TuckerPath String
    | WalletPath String
    | UseMainNet
    | UseTestNet
    | EnableMiner Bool
    | EnableMinDiff Bool
    | EnableMemPool Bool
    | EnableWallet Bool
    | MinFee Int
    | ShowHelp
    | SetJob Int
    deriving (Eq, Show)

hasHelp :: [String] -> Bool
hasHelp args =
    case parseFlags args [ opt_help ] of
        Right (_:_, _) -> True
        _ -> False

def_flags = [ EnableMiner True, EnableMemPool True ]

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
            WalletPath path -> void $
                appA (\conf -> conf { tckr_wallet_path = path }) conf_var

            SetJob job -> void $
                appA (\conf -> conf { tckr_job_number = job }) conf_var

            EnableMiner bool -> void $
                appA (\conf -> conf { tckr_enable_miner = bool }) conf_var

            EnableMinDiff bool ->
                when_ (tckr_use_special_min_diff conf) $
                    appA (\conf -> conf { tckr_use_special_min_diff_mine = bool }) conf_var

            EnableMemPool bool -> void $
                appA (\conf -> conf { tckr_enable_mempool = bool }) conf_var

            EnableWallet bool -> void $
                appA (\conf -> conf { tckr_enable_wallet = bool }) conf_var

            MinFee fr -> void $
                appA (\conf -> conf { tckr_min_tx_fee_rate = fi fr }) conf_var

            _ -> return ()

    getA conf_var

showHelp :: [String] -> [String] -> [Option Flag] -> IO ()
showHelp tool_path args opts = do
    prog_name <- getProgName

    if null tool_path then do
        tLnM ("usage: " ++ prog_name ++ " [options]")
        tLnM ("       " ++ prog_name ++ " tool <tool> [subtool1 [subtool2]...] [options]")
    else
        let arg_string = if null args then "" else " " ++ unwords args in
        tLnM ("usage: " ++ prog_name ++ " " ++ unwords tool_path ++ arg_string ++ " [options]")

    unless (null opts) $ do
        tLnM ""
        tLnM "options:"
        tM (genHelp opts)

opt_help = NoArg [ "h", "help" ] ShowHelp "show this help message"
opt_chain_path = WithArg [ "p", "path" ] TuckerPath "set tucker path"
opt_wallet_path = WithArg [ "w", "wallet" ] WalletPath "set wallet path"
opt_mainnet = WithArg [ "p", "path" ] TuckerPath "set tucker path"
opt_testnet = NoArg ["testnet"] UseTestNet "use testnet(in default)"
opt_enable_miner = WithArg ["enable-miner"] EnableMiner "enable miner[TRUE/false]"

opt_enable_min_diff =
    WithArg ["enable-min-diff"] EnableMinDiff
            "use min-diff rule to mine(only available when min-diff is enabled on the net)[true/false]"

opt_enable_mempool =
    WithArg ["enable-mempool"] EnableMemPool
            "enable tx mem pool(usually for mining)[TRUE/false]"

opt_enable_wallet =
    WithArg ["enable-wallet"] EnableWallet "enable wallet[FALSE/true]"

opt_min_fee = WithArg ["min-fee"] MinFee "min tx fee required for mem pool txns(in satoshi/kb)"

opt_job = WithArg [ "j", "job" ] SetJob "set the number of native threads to use"

-- main proc opts
main_opts = [
        opt_chain_path,
        opt_wallet_path,
        opt_mainnet,
        opt_testnet,
        opt_enable_miner,
        
        opt_enable_min_diff,
        opt_enable_mempool,
        opt_enable_wallet,
        
        opt_min_fee,
        
        opt_job,
        opt_help
    ]
