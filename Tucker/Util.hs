module Tucker.Util where

import Data.Time.Clock.POSIX

import System.CPUTime

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

fi :: (Integral a, Integral b) => a -> b
fi = fromIntegral
