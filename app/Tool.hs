{-# LANGUAGE ViewPatterns #-}

module Tool where

import Data.List

import Control.Monad
import Control.Exception

import System.Exit

import Tucker.Conf
import Tucker.Util
import Tucker.Error
import Tucker.Console

import Tucker.Wallet.Wallet
import Tucker.Wallet.Mnemonic

import Flag

type ToolProc = Tool -> [String] -> [Flag] -> [String] -> IO ()

data Tool
    = ToolGroup String String [Tool]
    | Tool {
        tool_name :: String,
        tool_desc :: String,
        tool_proc :: ToolProc,
        tool_opts :: [Option Flag]
    }

toolName (ToolGroup name _ _) = name
toolName (Tool { tool_name = name }) = name

toolDesc (ToolGroup _ desc _) = desc
toolDesc (Tool { tool_desc = desc }) = desc

toolNewWallet tool path flags (sent:rst) = do
    conf <- flagsToConf flags
    newWalletFromMnemonic
        conf (words sent)
        (case rst of [] -> Nothing; [pass] -> Just pass)

toolNewWallet tool path _ _ =
    showToolHelp tool path

-- toolNewWallet conf _ =
--     throwMT "new-wallet is expecting at least one argument(new-wallet mnemonic-sentence [password])"

-- toolMnemonic conf (entropy:_) =
--     case entropyToMnemonic def (hex2bs entropy) of
--         Right words -> tLnM (unwords words)
--         Left err -> throw err

-- toolMnemonic conf _ = do
--     throwMT "mnemonic is expecting at least one argument(entropy)"

root_tool =
    ToolGroup "tool" "root tool group" [
        ToolGroup "new" "a tool collection to creating new objects" [
            Tool {
                tool_name = "wallet",
                tool_desc = "generate a wallet from mnemonic sentence, tool new wallet <mnemonic> [password]",
                tool_proc = toolNewWallet,
                tool_opts = chain_opts
            }
        ]
    ]

parseError tool path msg = do
    tLnM msg
    tLnM ""
    showToolHelp tool path

    exitWith (ExitFailure 1)
    
showToolHelp tool@(ToolGroup _ _ group) path = do
    tLnM (unwords path ++ ": " ++ toolDesc tool)
    showHelp (path ++ ["<subtool>"]) []

    let col0_cont = "subtool"
        max_len = foldl max (length col0_cont) $ map (length . toolName) group
        col0_len = max_len + 3

    tLnM ""
    tLnM (col0_cont ++
          replicate (col0_len - length col0_cont) ' ' ++
          "description")

    forM_ group $ \tool -> do
        let name = toolName tool

        tLnM (name ++ replicate (col0_len - length name) ' ' ++ toolDesc tool)

showToolHelp tool@(Tool { tool_opts = opts }) path = do
    tLnM (unwords path ++ ": " ++ toolDesc tool)
    showHelp (path ++ ["<subtool>"]) opts

execTool :: Tool -> [String] -> [String] -> IO ()
execTool tool@(Tool {
    tool_proc = proc,
    tool_opts = opts
}) path args = do
    case parseFlags args opts of
        Right (flags, non_opt) ->
            if ShowHelp `elem` flags then
                showToolHelp tool path
            else
                proc tool path flags non_opt

        Left err -> do
            tLnM (show err)
            tLnM ""
            showToolHelp tool path

execTool tool@(ToolGroup name _ group) path args =
    case args of
        subtool@(isOption -> False):rst ->
            case first ((== subtool) . toolName) group of
                Just tool -> execTool tool (path ++ [subtool]) rst

                Nothing ->
                    parseError tool path
                               ("failed to find subtool '" ++ subtool ++
                                "' in tool group '" ++ unwords path ++ "'")

        _ ->
            parseError tool path
                       ("expecting subtool specified for tool group '" ++ unwords path ++ "'")

findAndExecTool :: [String] -> IO ()
findAndExecTool args = execTool root_tool ["tool"] args
