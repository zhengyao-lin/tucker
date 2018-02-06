{-# LANGUAGE TypeSynonymInstances #-}

-- key-value db wrapper with keyspace

module Tucker.DB where

import qualified Database.LevelDB as D

import System.FilePath
import System.Directory

import Control.Monad.Morph
import Control.Monad.Trans.Resource

import Tucker.Enc
import Tucker.Util

type Database = D.DB

type DBOption = D.Options
type DBOptionR = D.ReadOptions
type DBOptionW = D.WriteOptions

type DBKeySpace = String

instance Default DBOption where
    def = D.defaultOptions {
        D.createIfMissing = True
    }

instance Default DBOptionR where
    def = D.defaultReadOptions

instance Default DBOptionW where
    def = D.defaultWriteOptions

type DBKey = ByteString
type DBValue = ByteString

data DBBatchOp
    = DBSet DBKey DBValue
    | DBDel DBKey

setWithOption :: DBOptionW -> Database -> DBKey -> DBValue -> IO ()
setWithOption opt db = D.put db opt

getWithOption :: DBOptionR -> Database -> DBKey -> IO (Maybe DBValue)
getWithOption opt db = D.get db opt

deleteWithOption :: DBOptionW -> Database -> DBKey -> IO ()
deleteWithOption opt db = D.delete db opt

batchWithOption :: DBOptionW -> Database -> [DBBatchOp] -> IO ()
batchWithOption opt db = D.write db opt . map toLDBOp
    where
        toLDBOp (DBSet k v) = D.Put k v
        toLDBOp (DBDel k) = D.Del k

withDB :: DBOption -> FilePath -> DBKeySpace -> (Database -> IO a) -> ResIO a
withDB opt path space proc = do
    lift $
        if D.createIfMissing opt then
            createDirectoryIfMissing False path
        else
            return ()

    db <- D.open (path </> space) opt
    lift $ proc db

withDBIO :: DBOption -> FilePath -> DBKeySpace -> (Database -> IO a) -> IO a
withDBIO opt path space proc =
    runResourceT $ withDB opt path space proc

set = setWithOption def
get = getWithOption def
delete = deleteWithOption def
batch = batchWithOption def
