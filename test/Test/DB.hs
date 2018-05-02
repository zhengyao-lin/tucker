module Test.DB where

import Test.Util
import Test.HUnit
import Test.Common

bucketTest = TestCase $ do
    withDB def (test_db_path </> "bucket-test") $ \db -> do
        b1_raw <- openBucket db "bucket1" :: IO (DBBucket String String)
        b1 <- wrapCacheMap b1_raw

        insertIO b1 "hi" "yeah"

        res <- lookupIO b1_raw "hi"
        assertEqual "cached value should not be stored"
            Nothing res

        syncCacheMap b1

        res <- lookupIO b1_raw "hi"
        assertEqual "sync value should be stored"
            (Just "yeah") res

        deleteIO b1 "hi"

        res <- lookupIO b1_raw "hi"
        assertEqual "cached value should not be deleted"
            (Just "yeah") res

        syncCacheMap b1

        res <- lookupIO b1_raw "hi"
        assertEqual "sync value should be deleted"
            Nothing res

dbTests = TestList [
        TestLabel "bucket test" bucketTest
    ]
