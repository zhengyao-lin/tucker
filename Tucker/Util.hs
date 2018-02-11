module Tucker.Util where

import Data.Time.Clock.POSIX

import System.CPUTime

import Control.Monad
import Control.Exception
import Control.Monad.Loops

import Tucker.Error

class Default a where
    def :: a

unixTimestamp :: Integral a => IO a
unixTimestamp = round `fmap` getPOSIXTime

replace pos new list = take pos list ++ new:drop (pos + 1) list

fst3 (v, _, _) = v
snd3 (_, v, _) = v
trd3 (_, _, v) = v

-- microsec(e-6) to millisec(e-3)
us2ms t = t `div` 1000

-- nanosec(e-9) to millisec(e-3)
ns2ms t = t `div` 1000000

-- picosec(e-12) to millisec(e-3)
ps2ms t = t `div` 1000000000

-- microsec(e-6) to second
us2s t = t `div` 1000000

-- nanosec(e-9) to second
ns2s t = t `div` 1000000000

-- picosec(e-12) to second
ps2s t = t `div` 1000000000000

msCPUTime :: Integral t => IO t
msCPUTime = getCPUTime >>= (pure . fromInteger . ps2ms)

dupFill :: [a] -> Int -> Int -> [a]
dupFill lst times max =
    if length lst >= max then lst
    else take max $ concat [ lst | _ <- [ 1 .. times ] ]

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a (Just b) = Right b
maybeToEither a Nothing = Left a

maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool Nothing = False

fi :: (Integral a, Integral b) => a -> b
fi = fromIntegral

maybeCat lst = [ v | Just v <- lst ]

eitherToIO :: Show a => Either a b -> IO b
eitherToIO = either (fail . show) (pure . id)

ioToEitherIO :: IO a -> IO (Either TCKRError a)
ioToEitherIO =
    (`catch` \e -> return $ Left $ TCKRError $ show (e :: SomeException)) .
    (Right <$>)

divCeiling :: Integral a => a -> a -> a
divCeiling a b = (a + b - 1) `div` b

divFloor :: Integral a => a -> a -> a
divFloor a b = a `div` b

-- split a list to n lists such that the number of elements of each list is <= max
splitList :: Int -> [a] -> [[a]]
splitList max list =
    [
        take max $ drop (i * max) list
        | i <- [ 0 .. n - 1 ]
    ]
    where
        n = length list `divCeiling` max

forUntilM_ :: Monad m => [a] -> (a -> m Bool) -> m ()
forUntilM_ lst mpred =
    anyM mpred lst >> return ()

-- separate a list to two list (l1, l2)
-- where all pred l1
-- and   all (not . pred) l2
sepWhen :: (a -> Bool) -> [a] -> ([a], [a])
sepWhen pred =
    foldl (\(l1, l2) v ->
        if pred v then
            (l1 ++ [v], l2)
        else
            (l1, l2 ++ [v])) ([], [])

sepWhenM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
sepWhenM pred =
    foldM (\(l1, l2) v -> do
        r <- pred v
        if r then
            return (l1 ++ [v], l2)
        else
            return (l1, l2 ++ [v])) ([], [])
