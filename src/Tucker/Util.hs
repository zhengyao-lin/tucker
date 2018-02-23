module Tucker.Util where

import qualified Text.Printf as TP

import Data.Bits
import Data.Time.Clock.POSIX
import qualified Data.Foldable as FD
import qualified Data.Set.Ordered as OSET

import Math.NumberTheory.Moduli

import System.CPUTime

import Control.Monad
import Control.Exception
import Control.Monad.Loops

import Tucker.Error
import Tucker.DeepSeq

class Default a where
    def :: a

data PartialList a = PartialList Int | FullList [a] deriving (Show)

isPartial :: PartialList a -> Bool
isPartial (PartialList _) = True
isPartial _ = False

isFull :: PartialList a -> Bool
isFull = not . isPartial

toPartial :: PartialList a -> PartialList a
toPartial (FullList list) = PartialList (length list)
toPartial a = a

partial_access = throw $ TCKRError "access to partial list"

instance NFData a => NFData (PartialList a) where
    rnf (FullList list) = rnf list
    rnf (PartialList len) = rnf len

instance FD.Foldable PartialList where
    foldMap f (FullList list) = foldMap f list
    foldMap f (PartialList list) = partial_access

    foldr f a (FullList list) = foldr f a list
    foldr f a (PartialList list) = partial_access

    length (FullList list) = length list
    length (PartialList len) = len

    null (PartialList len) = len == 0
    null (FullList list) = null list

    toList (PartialList len) = replicate len partial_access
    toList (FullList list) = list

unixTimestamp :: Integral a => IO a
unixTimestamp = round `fmap` getPOSIXTime

replace pos new list = take pos list ++ new : drop (pos + 1) list

-- replace' pos new list = take pos list ++ new : drop (pos + 1) list

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

fi :: (Integral a, Num b) => a -> b
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

listUnion :: Ord a => [[a]] -> [a]
listUnion lists =
    FD.toList $
    foldl (OSET.|<>) OSET.empty $
    map OSET.fromList lists

unique :: Ord a => [a] -> [a]
unique = FD.toList . OSET.fromList

printf :: TP.PrintfType r => String -> r
printf = TP.printf

dup :: a -> (a, a)
dup a = (a, a)

foldM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM' _ z [] = return z
foldM' f z (x:xs) = do
    z' <- f z x
    z' `seq` foldM' f z' xs

isqrt' :: Integral t => t -> t -> t -> Maybe t
isqrt' num u l =
    -- assert l <= (sqrt num) < u
    let mid = (u - l) `div` 2 + l
        sq = mid ^ 2 in

    if mid == l then
        if sq == num then Just mid
        else Nothing
    else if sq < num then
        isqrt' num u mid
    else if sq > num then
        isqrt' num mid l
    else
        Just mid

isqrt :: Integral t => t -> Maybe t
isqrt num =
    isqrt' num (num + 1) 0

modSqrt :: Integer -> Integer -> Maybe Integer
modSqrt = sqrtModP

-- by trevordixon at https://gist.github.com/trevordixon/6788535
modExp :: (Integral t, Bits t) => t -> t -> t -> t
modExp b 0 m = 1
modExp b e m =
    t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
    where t = if testBit e 0 then b `mod` m else 1

ascending :: Ord a => [a] -> Bool
ascending [] = True
ascending [_] = True
ascending (x:y:xs) = x <= y && ascending (y:xs)
