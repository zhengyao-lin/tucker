module Tucker.Thread where

import Data.List

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Monad.Morph
import Control.Monad.Loops
import Control.Monad.Trans.Resource

import Tucker.Atom
import Tucker.Conf
import Tucker.Util
import Tucker.Error

{-

thread allocation

0: base services(gc, etc.)
1: node services(all node thread runs on this level)
2: trivial(temporary threads, etc.)
3 to (3 + j - 1): validation threads

-}

data ThreadState =
    ThreadState {
        main_tid    :: ThreadId, -- the thread that called initThread
        thread_caps :: [Atom Int] -- number of threads running on each capability
    }

data ThreadType
    = THREAD_BASE
    | THREAD_NODE
    | THREAD_OTHER
    | THREAD_VALIDATION

initThread :: TCKRConf -> IO ThreadState
initThread conf = do
    let job = tckr_job_number conf
        -- mextra is the number of thread for validation
        total = max 1 job + 3

    tid <- myThreadId

    setNumCapabilities total

    cap <- replicateM total (newA 0)

    return $ ThreadState {
        main_tid = tid,
        thread_caps = cap
    }

threadStatus :: ThreadState -> IO [Int]
threadStatus = mapM getA . thread_caps

assignCap :: ThreadState -> ThreadType -> IO Int
assignCap state ttype =
    case ttype of
        THREAD_BASE -> return 0
        THREAD_NODE -> return 1
        THREAD_OTHER -> return 2
        THREAD_VALIDATION ->
            -- even out on the validation threads
            fst <$>
            minimumBy (\(_, a) (_, b) -> compare a b) <$>
            zip [3..] <$>
            mapM getA (drop 3 (thread_caps state))

exitAll :: Exception e => ThreadState -> e -> IO ()
exitAll state e = throwTo (main_tid state) e

incCap state cap = appA (+1) (thread_caps state !! (cap `mod` length (thread_caps state))) >> return ()
decCap state cap = appA (+(-1)) (thread_caps state !! (cap `mod` length (thread_caps state))) >> return ()

finalHandle :: ThreadState -> Either SomeException a -> (Either SomeException a -> IO ()) -> IO ()
finalHandle state r proc =
    case r of
        Right _ -> proc r
        Left err ->
            if shouldExitOn err then do
                tLnM "killing the main thread(bug)"
                exitAll state err
            else proc r

forkCapFinally :: ThreadState -> ThreadType -> IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkCapFinally state ttype action finally = do
    cap <- assignCap state ttype

    incCap state cap

    forkOn cap $ try action >>= \r -> do
        decCap state cap
        finalHandle state r finally
        
forkCap state ttype action = do
    forkCapFinally state ttype action $ \r ->
        case r of
            Right _ -> return ()
            Left err -> tLnM ("thread killed: " ++ show err)

-- forkMap and ignore any exceptions/return values
forkMap__ :: ThreadState -> ThreadType -> (a -> IO b) -> [a] -> IO ()
forkMap__ state ttype proc lst = do
    mvars <- forM lst $ \x -> do
        mvar <- newEmptyMVar
        forkCapFinally state ttype (proc x) (const (putMVar mvar ()))
        return mvar

    mapM_ takeMVar mvars

-- forkMap and ignore return values(but may fail with exceptions)
forkMap :: ThreadState -> ThreadType -> (a -> IO b) -> [a] -> IO [b]
forkMap state ttype proc lst = do
    mvars <- forM lst $ \x -> do
        mvar <- newEmptyMVar
        forkCapFinally state ttype (proc x) (putMVar mvar)
        return mvar

    -- throw the first exception enconutered
    forM mvars $ \v ->
        takeMVar v >>= \r ->
            case r of
                Right r -> return r
                Left err -> throw err

msDelay :: Integral t => t -> IO ()
msDelay ms = threadDelay (fi ms * 1000)

killAllThreads :: [ThreadId] -> IO ()
killAllThreads ids =
    mapM_ killThread ids

waitUntilIO :: IO Bool -> IO ()
waitUntilIO = untilM_ yieldWait

yieldWait :: IO ()
yieldWait =
    msDelay 100 -- 100ms
