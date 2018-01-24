module Tucker.Util where

import Data.Time.Clock.POSIX

unixTimestamp :: Integral a => IO a
unixTimestamp = round `fmap` getPOSIXTime

replace pos new list = take pos list ++ new:drop (pos + 1) list
