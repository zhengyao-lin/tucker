module Tucker.Lock where

import Control.Concurrent
import qualified Control.Concurrent.Lock as LK

import Tucker.Atom
import Tucker.Util

data Lock =
    Lock {
        current_tid :: Atom (Maybe ThreadId),
        raw_lock    :: LK.Lock
    }

new :: IO Lock
new = do
    current_tid <- newA Nothing
    raw_lock <- LK.new

    return $ Lock {
        current_tid = current_tid,
        raw_lock = raw_lock
    }

-- note this acquire function has different semantics with LK.acquire
-- as it will not block the thread when multiple acquires are performed
acquire :: Lock -> IO ()
acquire lock = do
    my_tid <- myThreadId

    do_lock <- peekA (current_tid lock) (/= Just my_tid)
    -- do lock if it's not locked by the current thread
    
    -- tid <- getA (current_tid lock)
    -- tLnM (do_lock, tid, my_tid)

    if do_lock then do
        LK.acquire (raw_lock lock)
        setA (current_tid lock) (Just my_tid)
    else
        return ()

release :: Lock -> IO ()
release lock = do
    my_tid <- myThreadId
    do_release <- peekA (current_tid lock) (== Just my_tid)

    if do_release then do
        setA (current_tid lock) Nothing
        LK.release (raw_lock lock)
    else
        return () -- not locked/locked by the current thread

with :: Lock -> IO a -> IO a
with lock action = do
    acquire lock
    res <- action
    release lock
    return res
