{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

-- general map

module Tucker.IOMap where

import qualified Data.Map.Strict as MP
import qualified Data.ByteString as BSR

import Control.Monad

import Tucker.Enc
import Tucker.Atom

class IOMap a k v | a -> k, a -> v where
    lookupIO :: a -> k -> IO (Maybe v)
    insertIO :: a -> k -> v -> IO ()
    deleteIO :: a -> k -> IO ()

    applyIO :: a -> (v -> v) -> k -> IO ()
    applyIO a f k = do
        mv <- lookupIO a k
        case mv of
            Just v -> insertIO a k (f v)
            Nothing -> return ()

    foldKeyIO :: a -> b -> (b -> k -> IO b) -> IO b

    mapKeyIO :: a -> (k -> IO b) -> IO [b]
    mapKeyIO a f = foldKeyIO a [] $ \lst k -> do
        r <- f k
        return (lst ++ [r])

    mapKeyIO_ :: a -> (k -> IO a) -> IO ()
    mapKeyIO_ a f = foldKeyIO a () (\_ k -> f k >> return ())

    countIO :: Integral t => a -> IO t
    countIO a = foldKeyIO a 0 (\n _ -> return (n + 1))

data CacheMap a k v = CacheMap (Atom (MP.Map ByteString (Maybe v))) a
type CacheMapWrap a k v = CacheMap (a k v) k v

wrapCacheMap :: a -> IO (CacheMap a k v)
wrapCacheMap parent = do
    mmap <- newA MP.empty
    return (CacheMap mmap parent)

syncCacheMap :: (Decodable k, IOMap a k v) => CacheMap a k v -> IO ()
syncCacheMap (CacheMap mmap parent) = do
    map <- appA_ (const MP.empty) mmap

    forM_ (MP.toList map) $ \(k, mv) ->
        case mv of
            Nothing -> deleteIO parent (decodeFailLE k)
            Just v -> insertIO parent (decodeFailLE k) v

unwrapCacheMap :: (Decodable k, IOMap a k v) => CacheMap a k v -> IO a
unwrapCacheMap bmap@(CacheMap _ parent) =
    syncCacheMap bmap >> return parent

instance (Encodable k, Decodable k, IOMap a k v) => IOMap (CacheMap a k v) k v where
    lookupIO (CacheMap mmap parent) k = do
        map <- getA mmap
        case MP.lookup (encodeLE k) map of
            Nothing -> lookupIO parent k
            Just res -> return res

    insertIO (CacheMap mmap _) k v =
        appA (MP.insert (encodeLE k) (Just v)) mmap >> return ()

    deleteIO (CacheMap mmap _) k =
        appA (MP.insert (encodeLE k) Nothing) mmap >> return ()

    foldKeyIO (CacheMap mmap parent) init proc = do
        map_var <- getA mmap >>= newA

        foldKeyIO parent init $ \init k -> do
            map <- getA map_var

            case MP.lookup (encodeLE k) map of
                Nothing -> proc init k
                Just Nothing -> return init -- deleted already
                Just (Just _) -> proc init k

    -- sync (GeneralCacheMap mmap parent) = do
    --     map <- appA_ (const MP.empty) mmap

    --     forM_ (MP.toList map) $ \(k, mv) ->
    --         case mv of
    --             Nothing -> delete parent (decodeFailLE k)
    --             Just v -> set parent (decodeFailLE k) v

-- instance (Encodable k, Decodable k, IOMap ByteString v a)
--          => IOMap k v (CacheMap k v a) where

--     lookupIO a k = lookupIO a (encodeLE k)
--     insertIO a k v = insertIO a (encodeLE K) v
--     deleteIO a k = deleteIO a (encodeLE k)
