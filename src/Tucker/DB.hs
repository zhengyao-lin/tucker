{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- key-value db wrapper with keyspace

module Tucker.DB where

import Data.Hex
import Data.Word
import qualified Data.Map.Strict as MP
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import qualified Database.LevelDB as D

import System.FilePath
import System.Directory

import Control.Monad
import Control.Monad.Morph
import Control.Monad.Loops
import Control.Monad.Trans.Resource

import Debug.Trace

import Tucker.Enc
import Tucker.Util
import Tucker.Atom
import Tucker.IOMap

type Database = D.DB

type DBOption = D.Options
type DBOptionR = D.ReadOptions
type DBOptionW = D.WriteOptions

-- type DBKeySpace = ByteString

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

openDB :: DBOption -> FilePath -> ResIO Database
openDB opt path = do
    lift $
        if D.createIfMissing opt then
            createDirectoryIfMissing True path
        else
            return ()

    D.open path opt

withDB :: DBOption -> FilePath -> (Database -> IO a) -> IO a
withDB opt path proc = runResourceT $
    openDB opt path >>= (lift . proc)

instance IOMap Database ByteString ByteString where
    lookupIO = lookupWithOption def
    insertIO = insertWithOption def
    deleteIO = deleteWithOption def

    foldKeyIO db init proc = runResourceT $ do
        iter <- D.iterOpen db (def { D.fillCache = False })
    
        lift $ D.iterFirst iter
    
        (res, _) <- lift $ flip (iterateUntilM snd) (init, False) $
            \(init, valid) -> do
                -- valid <- D.iterValid iter
                mkey <- D.iterKey iter

                next <- case mkey of
                    Nothing -> return init
                    Just key -> proc init key

                D.iterNext iter

                return (next, not $ maybeToBool mkey)
    
        return res

lookupWithOption :: DBOptionR -> Database -> ByteString -> IO (Maybe ByteString)
lookupWithOption opt db key = D.get db opt key

insertWithOption :: DBOptionW -> Database -> ByteString -> ByteString -> IO ()
insertWithOption opt db key val = D.put db opt key val

deleteWithOption :: DBOptionW -> Database -> ByteString -> IO ()
deleteWithOption opt db key = D.delete db opt key

data DBBucket k v =
    DBBucket {
        bucket_pref :: ByteString,
        raw_db      :: Database

        -- use buffer map if present
        -- buffer_map  :: Atom (Maybe (MP.Map ByteString (Maybe ByteString)))
    }

instance Show (DBBucket k v) where
    show bucket = "Bucket " ++ show (hex (bucket_pref bucket))

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
    pref <- lookupIO db (key_bucket name)

    pref <- case pref of
        Just pref -> return pref
        Nothing -> do
            -- bucket does not exist
            count <- maybe 0 decodeFailLE <$> lookupIO db key_bucket_count
            
            let pref = encodeLE (count + 1 :: Word32)

            insertIO db key_bucket_count pref
            insertIO db (key_bucket name) pref

            return pref

    -- buffer_map <- newA Nothing

    return $ DBBucket {
        bucket_pref = pref,
        raw_db = db
        -- buffer_map = buffer_map
    }

instance (Encodable k, Decodable k, Encodable v, Decodable v)
         => IOMap (DBBucket k v) k v where

    lookupIO = lookupAsIO
    insertIO (DBBucket pref db) k v = insertIO db (pref <> encodeLE k) (encodeLE v)
    deleteIO (DBBucket pref db) k = deleteIO db (pref <> encodeLE k)

    foldKeyIO (DBBucket pref db) init proc =
        foldKeyIO db init $ \init k ->
            if pref `BSR.isPrefixOf` k then
                proc init (decodeFailLE (BSR.drop (BSR.length pref) k))
            else
                return init

-- lookup a key and return differently decoded value
-- ONLY supported for DBBucket
lookupAsIO :: (Encodable k, Decodable v') => DBBucket k v -> k -> IO (Maybe v')
lookupAsIO (DBBucket pref db) k = do
    mres <- lookupIO db (pref <> encodeLE k)
    return (decodeFailLE <$> mres)

type DBEntry = DBBucket Placeholder
