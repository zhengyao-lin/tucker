module Test.All where

import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Test.HUnit
import Test.Common

-- import Tucker.Chain.Cached

import Test.DB
import Test.Enc
import Test.Util
import Test.Chain
import Test.Wallet
import Test.Script
import Test.Validation

allTests = TestList [
        encTests, dbTests, chainTests, scriptTests,
        validationTests, walletTests
    ]

testAll = runTestTT allTests

{-

WE NEED 1THash/s

1984 cycles for 1 SHA256

16MHz for Arduino Nano

4032.258 Hash/s for one arduino nano

base mem = 50mb

max 64 blocks in memory
64mb * 2 = 128mb

2 dbs, each opens 48 files, each max 2mb
2 * 48 * 2 = 192

-}
