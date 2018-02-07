{-# LANGUAGE TypeSynonymInstances, ConstraintKinds #-}

-- key-value db wrapper with keyspace

module Tucker.DB where

import qualified Data.ByteString as BSR

import qualified Database.LevelDB as D

import System.FilePath
import System.Directory

import Control.Monad.Morph
import Control.Monad.Trans.Resource

import Tucker.Enc
import Tucker.Util

type Database k v = D.DB

type DBOption = D.Options
type DBOptionR = D.ReadOptions
type DBOptionW = D.WriteOptions

type KeyValueType t = (Encodable t, Decodable t)

type DBKeySpace = String

instance Default DBOption where
    def = D.defaultOptions {
        D.createIfMissing = True
    }

instance Default DBOptionR where
    def = D.defaultReadOptions

instance Default DBOptionW where
    def = D.defaultWriteOptions

data DBBatchOp k v
    = DBSet k v
    | DBDel k

setWithOption :: (KeyValueType k, KeyValueType v)
              => DBOptionW -> Database k v -> k -> v -> IO ()
setWithOption opt db key val = D.put db opt (encodeLE key) (encodeLE val)

getWithOption :: (KeyValueType k, KeyValueType v)
              => DBOptionR -> Database k v -> k -> IO (Maybe v)
getWithOption opt db key = do
    res <- D.get db opt (encodeLE key)
    return $ case res of
        Nothing -> Nothing
        Just bs ->
            case decodeLE bs of
                (Right v, _) -> Just v
                _ -> fail "db decode failure"

deleteWithOption :: KeyValueType k => DBOptionW -> Database k v -> k -> IO ()
deleteWithOption opt db key = D.delete db opt (encodeLE key)

batchWithOption :: (KeyValueType k, KeyValueType v)
                => DBOptionW -> Database k v -> [DBBatchOp k v] -> IO ()
batchWithOption opt db = D.write db opt . map toLDBOp
    where
        toLDBOp (DBSet k v) = D.Put (encodeLE k) (encodeLE v)
        toLDBOp (DBDel k) = D.Del (encodeLE k)

openDB :: DBOption -> FilePath -> DBKeySpace -> ResIO (Database k v)
openDB opt path space = do
    lift $
        if D.createIfMissing opt then
            createDirectoryIfMissing False path
        else
            return ()

    D.open (path </> space) opt

withDB :: DBOption -> FilePath -> DBKeySpace -> (Database k v -> IO a) -> IO a
withDB opt path space proc = runResourceT $
    openDB opt path space >>= (lift . proc)

set :: (KeyValueType k, KeyValueType v) => Database k v -> k -> v -> IO ()
set = setWithOption def

get :: (KeyValueType k, KeyValueType v) => Database k v -> k -> IO (Maybe v)
get = getWithOption def

delete :: KeyValueType k => Database k v -> k -> IO ()
delete = deleteWithOption def

batch :: (KeyValueType k, KeyValueType v) =>Database k v -> [DBBatchOp k v] -> IO ()
batch = batchWithOption def
