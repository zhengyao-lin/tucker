module Tucker.Lock where

import Control.Monad
import Control.Concurrent

import Tucker.Atom
import Tucker.Util

-- a wrap for the normal lock with an extra support of recursive locking
data Lock =
    Lock {
        current_tid :: Atom (Maybe ThreadId, Int),
        raw_lock    :: MVar () -- when this mvar has value, the lock is in the locked state
    }

new :: IO Lock
new = do
    current_tid <- newA (Nothing, 0)
    raw_lock <- newEmptyMVar

    return $ Lock {
        current_tid = current_tid,
        raw_lock = raw_lock
    }

acquire :: Lock -> IO ()
acquire lock = do
    my_tid <- myThreadId

    do_lock <- peekA (current_tid lock) ((/= Just my_tid) . fst)
    -- do lock if it's not locked by the current thread

    -- d <- getA (current_tid lock)
    -- tLnM (show (d, do_lock, my_tid))

    when do_lock (putMVar (raw_lock lock) ()) -- held by different thread
     
    appA (\(_, cnt) -> (Just my_tid, cnt + 1)) (current_tid lock)
    return ()

release :: Lock -> IO ()
release lock = do
    my_tid <- myThreadId
    do_release <- peekA (current_tid lock) ((== Just my_tid) . fst)

    -- d <- getA (current_tid lock)
    -- tLnM (show (d, do_release, my_tid))

    when do_release $ do
        (_, cnt) <-
            flip appA (current_tid lock) $ \(t, cnt) ->
                if cnt == 1 then (Nothing, 0)
                else (t, cnt - 1) -- more layers

        when (cnt == 0) $ do
            res <- tryTakeMVar (raw_lock lock)
            when (isNothing res) (error "releasing unlocked lock")

with :: Lock -> IO a -> IO a
with lock action = do
    acquire lock
    res <- action
    release lock
    return res
