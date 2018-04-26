{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Tucker.Util where

import qualified Text.Printf as TP

import Data.Bits
import Data.List
import Data.Time.Clock.POSIX
import qualified Data.Foldable as FD
import qualified Data.Set.Ordered as OSET
import qualified Data.ByteString.Char8 as BS

import Math.NumberTheory.Moduli

import System.CPUTime
import System.IO.Unsafe
import qualified System.Console.ANSI as CA

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Concurrent.Thread.Delay
import qualified Control.Monad.Trans.Maybe as MT

import Tucker.Error
import Tucker.DeepSeq

type Id a = a

class Default a where
    def :: a

class Sizeable a where
    sizeOf :: a -> Int

u :: a
u = undefined

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
replaceApp pos f list = take pos list ++ f (list !! pos) : drop (pos + 1) list

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
msCPUTime = fi <$> ps2ms <$> getCPUTime

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

-- ioToEitherIO :: IO a -> IO (Either TCKRError a)
-- ioToEitherIO = (`catchT` (return . Left)) . (Right <$>)

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

-- fold a list to n sublists
foldList :: Int -> [t] -> [[t]]
foldList n list =
    [
        take maxn $ drop (i * maxn) list
        | i <- [ 0 .. t - 1 ]
    ] ++ [
        take minn $ drop (t * maxn + i * minn) list
        | i <- [ 0 .. n - t - 1 ]
    ]
    where
        oldn = length list
        -- foldn = old_n `divCeiling` n -- number of tasks to fold together
        maxn = oldn `divCeiling` n
        minn = oldn `divFloor` n
        t = oldn - minn * n

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

type MaybeIO = MT.MaybeT IO

runMaybeT = MT.runMaybeT
maybeT = MT.MaybeT

index :: FD.Foldable t => t a -> Int -> a
lst `index` i = FD.toList lst !! i

(!!!) :: FD.Foldable t => t a -> Int -> Maybe a
lst' !!! i =
    if i >= 0 && i < length lst then Just (lst !! i)
    else Nothing
    where lst = FD.toList lst'

half :: Integral t => t -> t
half = (`div` 2)

middle :: [a] -> a
middle [] = error "middle of empty list"
middle lst = lst !! half (length lst - 1)

median :: Ord a => [a] -> a
median [] = error "median of empty list"
median lst = middle (sort lst)

average :: Integral a => [a] -> a
average [] = error "average of empty list"
average lst' =
    fi (sum lst `div` fi (length lst))
    where lst = map fi lst' :: [Integer]

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll a lst = filter (/= a) lst

clearLineInit = CA.clearLine >> CA.setCursorColumn 0

-- class TraceShow t where
--     showT :: t -> String

-- instance {-# OVERLAPPING #-} TraceShow String where
--     showT = id

-- instance {-# OVERLAPPABLE #-} Show t => TraceShow t where
--     showT = show

-- tucker trace

tIO :: Bool -> String -> ()
tIO newline msg = unsafePerformIO $
    clearLineInit >>
    (if newline then BS.putStrLn else BS.putStr) (BS.pack msg)

tLn :: String -> a -> a
tLn msg a = tIO True msg `seq` a

t :: String -> a -> a
t msg a = tIO False msg `seq` a

tLnM :: Applicative m => String -> m ()
tLnM msg = tLn msg (pure ())

tM :: Applicative m => String -> m ()
tM msg = t msg (pure ())

data ConsoleColor
    = Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

fromConsoleColor :: ConsoleColor -> CA.Color
fromConsoleColor Black = CA.Black
fromConsoleColor Red = CA.Red
fromConsoleColor Green = CA.Green
fromConsoleColor Yellow = CA.Yellow
fromConsoleColor Blue = CA.Blue
fromConsoleColor Magenta = CA.Magenta
fromConsoleColor Cyan = CA.Cyan
fromConsoleColor White = CA.White

data ConsoleColorScheme
    = Color ConsoleColor Bool -- color bold
    | ColorDark ConsoleColor Bool -- color bold

fromConsoleColorScheme :: ConsoleColorScheme -> [CA.SGR]
fromConsoleColorScheme (Color c bold) =
    [ CA.SetColor CA.Foreground CA.Vivid (fromConsoleColor c),
      CA.SetConsoleIntensity (if bold then CA.BoldIntensity else CA.NormalIntensity) ]

fromConsoleColorScheme (ColorDark c bold) =
    [ CA.SetColor CA.Foreground CA.Dull (fromConsoleColor c),
      CA.SetConsoleIntensity (if bold then CA.BoldIntensity else CA.NormalIntensity) ]

-- wrap styled string
wss :: ConsoleColorScheme -> String -> String
wss sch msg =
    CA.setSGRCode (fromConsoleColorScheme sch) ++ msg ++ CA.setSGRCode [CA.Reset]

-- perform a binary search on the range [lo, hi) to find i in [lo, hi)
-- such that (pred i == EQ)
-- pred = compare expected given
binarySearchIO :: Integral a => (a -> IO Ordering) -> a -> a -> IO (Maybe a)
binarySearchIO pred lo hi =
    if lo >= hi then return Nothing
    else do
        let half = (hi - lo) `div` 2 + lo
        res <- pred half
        case res of
            LT -> binarySearchIO pred lo half
            EQ -> return (Just half)
            GT -> binarySearchIO pred (half + 1) hi

first :: (a -> Bool) -> [a] -> Maybe a
first pred lst =
    case dropWhile (not . pred) lst of
        x:_ -> Just x
        [] -> Nothing

killAllThreads :: [ThreadId] -> IO ()
killAllThreads ids =
    mapM_ killThread ids

timeit :: Integral t => IO a -> IO t
timeit action = do
    begin <- msCPUTime
    action
    end <- msCPUTime
    
    return (fi (end - begin))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

waitUntilIO :: IO Bool -> IO ()
waitUntilIO = (yield `untilM_`)

yieldWait :: IO ()
yieldWait =
    delay 100000 -- 100ms
