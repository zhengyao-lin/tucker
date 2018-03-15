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
import Test.Script
import Test.Validation

allTests = TestList [
        encTests, dbTests, chainTests, scriptTests,
        validationTests
    ]

testAll = runTestTT allTests
