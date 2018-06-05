module Tool where

import Control.Exception

import Tucker.Conf
import Tucker.Util
import Tucker.Error

import Tucker.Wallet.Wallet
import Tucker.Wallet.Mnemonic

import Flag

type ToolProc = TCKRConf -> [String] -> IO ()

toolNewWallet conf (sent:pass:_) =
    newWalletFromMnemonic conf (words sent) (Just pass)

toolNewWallet conf (sent:_) =
    newWalletFromMnemonic conf (words sent) Nothing

toolNewWallet conf _ =
    throwMT "new-wallet is expecting at least one argument(new-wallet mnemonic-sentence [password])"

toolMnemonic conf (entropy:_) =
    case entropyToMnemonic def (hex2bs entropy) of
        Right words -> tLnM (unwords words)
        Left err -> throw err

toolMnemonic conf _ = do
    throwMT "mnemonic is expecting at least one argument(entropy)"

tool_map = [
        ("new-wallet", toolNewWallet),
        ("mnemonic", toolMnemonic)
    ]

execTool :: TCKRConf -> String -> [String] -> IO ()
execTool conf tool args =
    case lookup tool tool_map of
        Just proc -> proc conf args
        Nothing -> do
            tLnM ("unrecognized tool '" ++ tool ++ "'")
            tLnM ""
            showHelp
