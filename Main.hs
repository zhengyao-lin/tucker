module Main where

import Tucker.Enc

main = do
	(wif, addr) <- genWithPrefix "23456"
	putStrLn (wif ++ ", " ++ addr)
