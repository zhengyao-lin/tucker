{-# LANGUAGE TypeSynonymInstances #-}

-- key-value db wrapper with keyspace

module Tucker.DB where

import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import qualified Database.LevelDB as D

import System.FilePath
import System.Directory

import Control.Monad.Morph
import Control.Monad.Loops
import Control.Monad.Trans.Resource

import Debug.Trace

import Tucker.Enc
import Tucker.Util

type Database = D.DB

type DBOption = D.Options
type DBOptionR = D.ReadOptions
type DBOptionW = D.WriteOptions

-- type DBKeySpace = ByteString

data DBAny

instance Default DBOption where
    def = D.defaultOptions {
        D.createIfMissing = True,
        D.maxOpenFiles = 32
        -- D.cacheSize = 4, -- 16k
        -- D.writeBufferSize = 4 -- 512K
    }

instance Default DBOptionR where
    def = D.defaultReadOptions

instance Default DBOptionW where
    def = D.defaultWriteOptions

setWithOption :: DBOptionW -> Database -> ByteString -> ByteString -> IO ()
setWithOption opt db key val = D.put db opt key val

getWithOption :: DBOptionR -> Database -> ByteString -> IO (Maybe ByteString)
getWithOption opt db key = D.get db opt key

hasWithOption :: DBOptionR -> Database -> ByteString -> IO Bool
hasWithOption opt db key =
    maybeToBool <$> D.get db opt key

deleteWithOption :: DBOptionW -> Database -> ByteString -> IO ()
deleteWithOption opt db key = D.delete db opt key

openDB :: DBOption -> FilePath -> ResIO Database
openDB opt path = do
    lift $
        if D.createIfMissing opt then
            createDirectoryIfMissing False path
        else
            return ()

    D.open path opt

withDB :: DBOption -> FilePath -> (Database -> IO a) -> IO a
withDB opt path proc = runResourceT $
    openDB opt path >>= (lift . proc)

set = setWithOption def
set' = setWithOption (def { D.sync = True })
get = getWithOption def
has = hasWithOption def
delete = deleteWithOption def

setAs :: Encodable v => Database -> ByteString -> v -> IO ()
setAs db key val = set db key (encodeLE val)

setAs' :: Encodable v => Database -> ByteString -> v -> IO ()
setAs' db key val = set' db key (encodeLE val)

getAs :: Decodable v => Database -> ByteString -> IO (Maybe v)
getAs db key = do
    res <- get db key
    return $ case res of
        Nothing -> Nothing
        Just bs ->
            case decodeLE bs of
                (Right v, _) -> Just v
                _ -> fail "db decode failure"

countWith :: Integral t => Database -> (ByteString -> Bool) -> IO t
countWith db pred = runResourceT $ do
    iter <- D.iterOpen db (def { D.fillCache = False })

    lift $ D.iterFirst iter

    res <-
        lift $ flip (iterateUntilM snd) (0, False) $
            \(count, valid) -> do
                -- valid <- D.iterValid iter
                mkey <- D.iterKey iter
                let next =
                        if (pred <$> mkey) == Just True then
                            count + 1
                        else
                            count

                D.iterNext iter

                return (next, not $ maybeToBool mkey)

    return $ fi (fst res)

count db = countWith db (const True)

data DBBucket k v = DBBucket ByteString Database

withPrefix :: [Word32] -> String -> ByteString
withPrefix prefs name =
    encodeLE prefs <> BS.pack name

key_bucket_count = withPrefix [ 0, 0 ] "bucket_count"
key_bucket name = withPrefix [ 0, 1 ] name

-- buckets are virtual keyspaces across in a database
-- bucket names are encoded to a 4-byte prefix for each entry
-- the prefix starts with 0x1(all in little endian)
-- 0x0000 ++ 0x0000 ++ "bucket_count" -> total bucket count
-- 0x0000 ++ 0x0001 ++ <bucket_name> -> prefix for bucket <bucket_name>
openBucket :: Database -> String -> IO (DBBucket k v)
openBucket db name = do
    pref <- get db (key_bucket name)

    pref <- case pref of
        Just pref -> return pref
        Nothing -> do
            -- bucket does not exist
            count <- maybe 0 id <$> getAs db key_bucket_count
            
            let pref = encodeLE (count + 1 :: Word32)

            set' db key_bucket_count pref
            set' db (key_bucket name) pref

            return pref

    return $ DBBucket pref db

setB :: (Encodable k, Encodable v) => DBBucket k v -> k -> v -> IO ()
setB (DBBucket pref db) key val = set db (pref <> encodeLE key) (encodeLE val)

setB' :: (Encodable k, Encodable v) => DBBucket k v -> k -> v -> IO ()
setB' (DBBucket pref db) key val = set' db (pref <> encodeLE key) (encodeLE val)

getB :: (Encodable k, Decodable v) => DBBucket k v -> k -> IO (Maybe v)
getB (DBBucket pref db) key = getAs db (pref <> encodeLE key)

-- return type can be different as the one declared in bucket type
getAsB :: (Encodable k, Decodable v) => DBBucket k v' -> k -> IO (Maybe v)
getAsB (DBBucket pref db) key = getAs db (pref <> encodeLE key)

hasB :: Encodable k => DBBucket k v -> k -> IO Bool
hasB (DBBucket pref db) key = has db (pref <> encodeLE key)

deleteB :: Encodable k => DBBucket k v -> k -> IO ()
deleteB (DBBucket pref db) key = delete db (pref <> encodeLE key)

countB :: Integral t => DBBucket k v -> IO t
countB (DBBucket pref db) =
    countWith db (pref `BSR.isPrefixOf`)
