module Main where

import Tucker.Enc
import Data.List

main = do
	(wif, addr) <- genCond (("23456" `isPrefixOf`) . (drop 1) . snd)
	putStrLn (wif ++ ", " ++ addr)
