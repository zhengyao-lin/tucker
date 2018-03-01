module Tucker.Atom where

-- import Control.Concurrent
-- import Control.Concurrent.STM

import Data.IORef

import qualified System.IO.Strict as SIO

import Tucker.Util

-- type Atom t = TVar t

-- newA :: t -> IO (Atom t)
-- newA = atomically . newTVar

-- getA :: Atom t -> IO t
-- getA = readTVarIO

-- setA :: Atom t -> t -> IO ()
-- setA v = atomically . writeTVar v

-- -- return the new value
-- appA :: (t -> t) -> Atom t -> IO t
-- appA f x = atomically $ do
--     modifyTVar' x f
--     readTVar x

type Atom t = IORef t

newA :: t -> IO (Atom t)
newA = newIORef

getA :: Atom t -> IO t
getA = readIORef

setA :: Atom t -> t -> IO ()
setA x v =
    atomicModifyIORef' x (flip (,) () . const v)

-- return the new value
appA :: (t -> t) -> Atom t -> IO t
appA f x =
    atomicModifyIORef' x (dup . f)

-- return the original value
appA_ :: (t -> t) -> Atom t -> IO t
appA_ f x =
    atomicModifyIORef' x $ \x -> (f x, x)

peekA :: Atom t -> (t -> a) -> IO a
peekA x f =
    atomicModifyIORef' x $ \x -> (x, f x)

lseq :: [a] -> b -> b
lseq []     w = w
lseq (x:xs) w = x `seq` lseq xs w

putStr' = SIO.run . SIO.putStr
putStrLn' = SIO.run . SIO.putStrLn
