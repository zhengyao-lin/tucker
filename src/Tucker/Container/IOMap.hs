{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

-- general map

module Tucker.Container.IOMap where

import qualified Data.ByteString as BSR

import Control.Monad

import Tucker.Enc
import Tucker.Atom

import qualified Tucker.Container.Map as MAP

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

    mapKeyIO_ :: a -> (k -> IO c) -> IO ()
    mapKeyIO_ a f = foldKeyIO a () (\_ k -> f k >> return ())

    countIO :: Integral t => a -> IO t
    countIO a = foldKeyIO a 0 (\n _ -> return (n + 1))

data CacheMap a k v = CacheMap (Atom (MAP.TMap k (Maybe v))) a
type CacheMapWrap a k v = CacheMap (a k v) k v

wrapCacheMap :: a -> IO (CacheMap a k v)
wrapCacheMap parent = do
    mmap <- newA MAP.empty
    return (CacheMap mmap parent)

syncCacheMap :: IOMap a k v => CacheMap a k v -> IO ()
syncCacheMap (CacheMap mmap parent) = do
    map <- appA_ (const MAP.empty) mmap

    forM_ (MAP.toList map) $ \(k, mv) ->
        case mv of
            Nothing -> deleteIO parent k
            Just v -> insertIO parent k v

unwrapCacheMap :: IOMap a k v => CacheMap a k v -> IO a
unwrapCacheMap bmap@(CacheMap _ parent) =
    syncCacheMap bmap >> return parent

instance (MAP.Constraint k, IOMap a k v) => IOMap (CacheMap a k v) k v where
    lookupIO (CacheMap mmap parent) k = do
        map <- getA mmap
        case MAP.lookup k map of
            Nothing -> lookupIO parent k
            Just res -> return res

    insertIO (CacheMap mmap _) k v =
        appA (MAP.insert k (Just v)) mmap >> return ()

    deleteIO (CacheMap mmap _) k =
        appA (MAP.insert k Nothing) mmap >> return ()

    foldKeyIO (CacheMap mmap parent) init proc = do
        map_var <- getA mmap >>= newA

        foldKeyIO parent init $ \init k -> do
            map <- getA map_var

            case MAP.lookup k map of
                Nothing -> proc init k
                Just Nothing -> return init -- deleted already
                Just (Just _) -> proc init k
