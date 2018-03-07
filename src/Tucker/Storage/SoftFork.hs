{-# LANGUAGE FlexibleInstances #-}

module Tucker.Storage.SoftFork where

import Data.Word
import Data.List

import Control.Monad
import Control.Applicative

import Debug.Trace

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.IOMap

instance Decodable [SoftFork] where
    decoder = many decoder

instance Decodable [Word32] where
    decoder = many decoder

data SoftForkState =
    SoftForkState {
        fork_conf   :: TCKRConf,

        bucket_fork :: CacheMapWrap DBBucket SoftForkId [SoftFork],
        bucket_stat :: CacheMapWrap DBBucket SoftForkId Word32
    }

initForkState :: TCKRConf -> Database -> IO SoftForkState
initForkState conf@(TCKRConf {
    tckr_bucket_fork_name = bucket_fork_name,
    tckr_bucket_stat_name = bucket_stat_name,
    tckr_soft_forks = soft_forks
}) db = do
    bucket_fork <- openBucket db bucket_fork_name >>= wrapCacheMap
    bucket_stat <- openBucket db bucket_stat_name >>= wrapCacheMap

    cur_forks <- concat . maybeCat <$> mapKeyIO bucket_fork (lookupIO bucket_fork)

    -- filter out existing forks
    let new_forks = filter (`notElem` cur_forks) soft_forks

    traceM ("adding new soft forks deployment " ++ show new_forks)

    forM_ ([0..] `zip` sortForkById new_forks) $ \(i, forks) ->
        if null forks then return ()
        else do
            cur_forks <- maybe [] id <$> lookupIO bucket_fork i

            -- append new forks
            insertIO bucket_fork i (cur_forks ++ forks)

    return $ SoftForkState {
        fork_conf = conf,
        bucket_fork = bucket_fork,
        bucket_stat = bucket_stat
    }

sortForkById :: [SoftFork] -> [[SoftFork]]
sortForkById =
    foldl (\map fork -> replaceApp (fi (fork_bit fork)) (fork:) map) (replicate 32 [])

-- changeStatus
-- lookupSoftForks :: block -> related deployments
-- recordBlock :: add block -> record stat
-- clearRecord :: clear stat

changeForkStatus :: SoftForkState -> SoftFork -> SoftForkStatus -> IO ()
changeForkStatus (SoftForkState {
    bucket_fork = bucket_fork
}) fork status = do
    traceM ("!!! changing the status fork " ++ show fork ++ " to " ++ show status)

    forks <- maybe [] id <$> lookupIO bucket_fork bit

    case elemIndex fork forks of
        Just i -> do
            let nlist = replaceApp i (\f -> f { fork_status = status }) forks
            insertIO bucket_fork bit nlist

        Nothing -> return ()

    where bit = fork_bit fork

lookupNonFinalForks :: SoftForkState -> Block -> IO [SoftFork]
lookupNonFinalForks (SoftForkState {
    bucket_fork = bucket_fork
}) block =
    (filter (not . isFinalSoftFork) . concat . maybeCat) <$>
    mapM (lookupIO bucket_fork) idxs
    where idxs = getSoftForkIds block

recordBlock :: SoftForkState -> Block -> IO ()
recordBlock (SoftForkState {
    bucket_stat = bucket_stat
}) block =
    forM_ idxs $ \idx -> do
        num <- maybe 0 id <$> lookupIO bucket_stat idx
        insertIO bucket_stat idx (num + 1)

    where idxs = getSoftForkIds block

-- get record of how many appearance of a particular version bit
getRecord :: SoftForkState -> SoftFork -> IO Word
getRecord (SoftForkState {
    bucket_stat = bucket_stat
}) fork =
    maybe 0 fi <$> lookupIO bucket_stat (fork_bit fork)

-- delete all keys
clearRecord :: SoftForkState -> IO ()
clearRecord (SoftForkState {
    bucket_stat = bucket_stat
}) =
    foldKeyIO bucket_stat () (const (deleteIO bucket_stat))

syncForkState :: SoftForkState -> IO ()
syncForkState (SoftForkState {
    bucket_fork = bucket_fork,
    bucket_stat = bucket_stat
}) =
    syncCacheMap bucket_fork >> syncCacheMap bucket_stat
