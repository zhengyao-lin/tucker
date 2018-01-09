module Main where

import Tucker.Enc
import Tucker.Std
import Data.List

main = do
	(wif, addr) <- genCond btc_mainnet (("23456" `isPrefixOf`) . (drop 1) . snd)
	putStrLn (wif ++ ", " ++ addr)
