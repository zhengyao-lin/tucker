module Tucker.Atom where

import Control.Concurrent
import Control.Concurrent.STM

import qualified System.IO.Strict as SIO

type Atom t = TVar t

newA :: t -> IO (Atom t)
newA = atomically . newTVar

getA :: Atom t -> IO t
getA = atomically . readTVar

setA :: Atom t -> t -> IO ()
setA v = atomically . writeTVar v

appA :: (t -> t) -> Atom t -> IO ()
appA f x = atomically $ modifyTVar' x f

lseq :: [a] -> b -> b
lseq []     w = w
lseq (x:xs) w = x `seq` lseq xs w

putStr' = SIO.run . SIO.putStr
putStrLn' = SIO.run . SIO.putStrLn
