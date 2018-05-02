{-# LANGUAGE FlexibleInstances #-}

module Tucker.Storage.SoftFork where

import Data.Word
import Data.List
import qualified Data.Map.Strict as MP

import Control.Monad
import Control.Applicative

import Tucker.DB
import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.Atom
import Tucker.IOMap

instance Decodable [SoftFork] where
    decoder = many decoder

instance Decodable [Word32] where
    decoder = many decoder

data SoftForkState =
    SoftForkState {
        fork_conf   :: TCKRConf,

        sf_status   :: Atom (MP.Map String SoftFork),

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

    cur_forks <- concat . maybeCat <$> mapM (lookupIO bucket_fork) soft_fork_id_range

    -- filter out existing forks
    let new_forks = filter (`notElem` cur_forks) soft_forks
        all_forks = cur_forks ++ new_forks

    tLnM ("current forks: " ++ show cur_forks)

    tLnM ("adding new soft forks deployment " ++ show new_forks)

    forM_ ([0..] `zip` sortForkById new_forks) $ \(i, forks) ->
        unless (null forks) $ do
            cur_forks <- maybe [] id <$> lookupIO bucket_fork i

            -- append new forks
            insertIO bucket_fork i (cur_forks ++ forks)

    sf_status <- newA MP.empty

    -- set up name -> fork mappings
    forM_ all_forks $ \fork -> do
        appA (MP.insert (fork_name fork) fork) sf_status
        return ()

    return $ SoftForkState {
        fork_conf = conf,
        sf_status = sf_status,
        bucket_fork = bucket_fork,
        bucket_stat = bucket_stat
    }

-- sort forks to bit:fork mappings
sortForkById :: [SoftFork] -> [[SoftFork]]
sortForkById =
    foldl (\map fork -> replaceApp (fi (fork_bit fork)) (fork:) map) (replicate 32 [])

-- changeStatus
-- lookupSoftForks :: block -> related deployments
-- recordBlock :: add block -> record stat
-- clearRecord :: clear stat

getForkStatus :: SoftForkState -> String -> IO SoftForkStatus
getForkStatus (SoftForkState {
    sf_status = sf_status
}) name =
    maybe FORK_STATUS_UNDEFINED fork_status <$>
    MP.lookup name <$>
    getA sf_status

getForkByName :: SoftForkState -> String -> IO (Maybe SoftFork)
getForkByName (SoftForkState {
    sf_status = sf_status
}) name =
    MP.lookup name <$> getA sf_status

-- csv 768095(locked in)
--     770112(active)
-- segwit around 830000 (locked in)
--        834624(active)
changeForkStatus :: SoftForkState -> SoftFork -> SoftForkStatus -> IO ()
changeForkStatus (SoftForkState {
    sf_status = sf_status,
    bucket_fork = bucket_fork
}) fork status = do
    tLnM ("!!! changing status to " ++ show status ++ " for " ++ show fork)

    -- if status == FORK_STATUS_ACTIVE ||
    --    status == FORK_STATUS_FAILED then
    --     error "i wanna die!!!"
    -- else
    --     return ()

    forks <- maybe [] id <$> lookupIO bucket_fork bit

    let new_fork = fork { fork_status = status }

    case elemIndex fork forks of
        Just i -> do
            let nlist = replace i new_fork forks
            appA (MP.insert (fork_name fork) new_fork) sf_status

            insertIO bucket_fork bit nlist

        Nothing -> return ()

    where bit = fork_bit fork

isFinalSoftFork :: SoftFork -> Bool
isFinalSoftFork d =
    status == FORK_STATUS_ACTIVE ||
    status == FORK_STATUS_FAILED
    where status = fork_status d

lookupNonFinalForks :: SoftForkState -> IO [SoftFork]
lookupNonFinalForks (SoftForkState {
    bucket_fork = bucket_fork
}) =
    (filter (not . isFinalSoftFork) . concat . maybeCat) <$>
    mapM (lookupIO bucket_fork) soft_fork_id_range

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
    -- foldKeyIO bucket_stat () (const (deleteIO bucket_stat))
    mapM_ (deleteIO bucket_stat) soft_fork_id_range

syncForkState :: SoftForkState -> IO ()
syncForkState (SoftForkState {
    bucket_fork = bucket_fork,
    bucket_stat = bucket_stat
}) =
    syncCacheMap bucket_fork >> syncCacheMap bucket_stat
