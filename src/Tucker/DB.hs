{-# LANGUAGE TypeSynonymInstances #-}

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
            createDirectoryIfMissing True path
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

countWith :: Integral t => Database -> (ByteString -> IO Bool) -> IO t
countWith db pred = runResourceT $ do
    iter <- D.iterOpen db (def { D.fillCache = False })

    lift $ D.iterFirst iter

    res <-
        lift $ flip (iterateUntilM snd) (0, False) $
            \(count, valid) -> do
                -- valid <- D.iterValid iter
                mkey <- D.iterKey iter

                accept <- case mkey of
                    Nothing -> return False
                    Just key -> pred key

                let next = if accept then count + 1
                           else count

                D.iterNext iter

                return (next, not $ maybeToBool mkey)

    return $ fi (fst res)

count db = countWith db (const (pure True))

data DBBucket k v =
    DBBucket {
        bucket_pref :: ByteString,
        raw_db      :: Database,

        -- use buffer map if present
        buffer_map  :: Atom (Maybe (MP.Map ByteString (Maybe ByteString)))
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

    buffer_map <- newA Nothing

    return $ DBBucket {
        bucket_pref = pref,
        raw_db = db,
        buffer_map = buffer_map
    }

-- init buffer map if not present
bufferizeB :: DBBucket k v -> IO ()
bufferizeB bucket = do
    bmap <- getA (buffer_map bucket)

    case bmap of
        Just _ -> return ()
        Nothing -> setA (buffer_map bucket) (Just MP.empty)

syncB :: DBBucket k v -> IO ()
syncB bucket = do
    buffered <- hasBufferB bucket

    if buffered then do
        Just bmap <- appA_ (const (Just MP.empty)) (buffer_map bucket)

        forM_ (MP.toList bmap) $ \(k, mv) ->
            case mv of
                Nothing -> delete (raw_db bucket) k
                Just v -> set (raw_db bucket) k v
    else
        return ()

hasBufferB :: DBBucket k v -> IO Bool
hasBufferB bucket = maybeToBool <$> getA (buffer_map bucket)

no_buffer_error = error "no buffer map present"

setBMap :: DBBucket k v -> ByteString -> ByteString -> IO ()
setBMap bucket k v =
    appA (Just . MP.insert k (Just v) . maybe no_buffer_error id)
         (buffer_map bucket) >> return ()

-- Nothing -> no such key in buffer map -> lookup in raw db
-- Just _ -> has the pair -> return the value
getBMap :: DBBucket k v -> ByteString -> IO (Maybe (Maybe ByteString))
getBMap bucket k =
    (MP.lookup k . maybe no_buffer_error id) <$> getA (buffer_map bucket)

-- note delete here sets the value to Nothing
-- instead of deleting the entry
deleteBMap :: DBBucket k v -> ByteString -> IO ()
deleteBMap bucket k =
    appA (Just . MP.insert k Nothing . maybe no_buffer_error id)
         (buffer_map bucket) >> return ()

setB :: (Encodable k, Encodable v) => DBBucket k v -> k -> v -> IO ()
setB bucket key val = do
    buffered <- hasBufferB bucket

    if buffered then
        setBMap bucket k v
    else
        set (raw_db bucket) k v

    where
        k = bucket_pref bucket <> encodeLE key
        v = encodeLE val

-- setB' :: (Encodable k, Encodable v) => DBBucket k v -> k -> v -> IO ()
-- setB' bucket key val =
--     set' (raw_db bucket) (bucket_pref bucket <> encodeLE key) (encodeLE val)

getB :: (Encodable k, Decodable v) => DBBucket k v -> k -> IO (Maybe v)
getB = getAsB

-- return type can be different as the one declared in bucket type
getAsB :: (Encodable k, Decodable v) => DBBucket k v' -> k -> IO (Maybe v)
getAsB bucket key = do
    buffered <- hasBufferB bucket

    if buffered then do
        mres <- getBMap bucket k
        case mres of
            Just res -> return (decodeFailLE <$> res)
            Nothing -> getAs (raw_db bucket) k
    else
        getAs (raw_db bucket) k

    where k = bucket_pref bucket <> encodeLE key

hasB :: Encodable k => DBBucket k v -> k -> IO Bool
hasB bucket key =
    maybeToBool <$> (getAsB bucket key :: IO (Maybe Placeholder))

deleteB :: Encodable k => DBBucket k v -> k -> IO ()
deleteB bucket key = do
    buffered <- hasBufferB bucket

    if buffered then
        deleteBMap bucket k
    else
        delete (raw_db bucket) k

    where k = bucket_pref bucket <> encodeLE key

countB :: Integral t => DBBucket k v -> IO t
countB bucket = do
    buffered <- hasBufferB bucket
    bmap <- maybe no_buffer_error id <$> getA (buffer_map bucket)

    bmap_var <- newA bmap

    if buffered then
        countWith (raw_db bucket) $ \key -> do
            let pref = bucket_pref bucket `BSR.isPrefixOf` key
    
            if pref then do
                bmap <- getA bmap_var

                case MP.lookup key bmap of
                    Nothing -> return True       -- not present in bmap, fine
                    Just Nothing -> return False -- deleted in bmap, don't count in
                    Just (Just _) -> return True -- may be altered but not deleted
            else
                return False
    else
        countWith (raw_db bucket) (return . BSR.isPrefixOf (bucket_pref bucket))
