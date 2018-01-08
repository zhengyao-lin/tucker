module Tucker.Int where

import Debug.Trace

import Data.Bits
import qualified Data.ByteString as BSR

type ByteString = BSR.ByteString

data Endian = LittleEndian | BigEndian deriving (Show, Read, Eq)

bchar :: Integral t => t -> ByteString
bchar = BSR.singleton . fromIntegral

-- VInt > 0
encodeVInt :: Endian -> Integer -> ByteString
encodeVInt _ 0 = BSR.pack []
encodeVInt end num =
    if num < 0 then error "encoding negative variable length ineteger"
    else let
        (rest, m) = num `divMod` 0x100
    in
        if end == LittleEndian then
            BSR.append (bchar m) (encodeVInt end rest)
        else
            BSR.append (encodeVInt end rest) (bchar m)

encodeInt :: Integral t => Int -> Endian -> t -> ByteString
encodeInt 0 _ _ = BSR.pack []
encodeInt nbyte end num =
    let
        (rest, m) = (fromIntegral num :: Integer) `divMod` 0x100
    in
        if end == LittleEndian then
            BSR.append (bchar m) (encodeInt (nbyte - 1) end rest)
        else
            BSR.append (encodeInt (nbyte - 1) end rest) (bchar m)

-- cannot determine the sign
decodeVInt :: Endian -> ByteString -> Integer
decodeVInt LittleEndian = BSR.foldr (\x a -> shift a 8 + (toInteger x)) 0
decodeVInt BigEndian = BSR.foldl (\a x -> shift a 8 + (toInteger x)) 0

decodeInt' :: Int -> ByteString -> Int -> Integer
decodeInt' 0 _ init = toInteger init
decodeInt' nbyte bs init =
    shift (decodeInt' (nbyte - 1) rest init) 8 + toInteger c
    where
        c = BSR.head bs
        rest = BSR.tail bs

decodeInt :: Int -> Endian -> ByteString -> Integer
decodeInt nbyte BigEndian bs = decodeInt nbyte LittleEndian $ BSR.reverse bs
decodeInt nbyte LittleEndian bs =
    if BSR.last bs < 0x80 then -- positive
        decodeInt' nbyte bs 0
    else
        decodeInt' nbyte bs (-1)

-- little-endian
bs2vint'le :: ByteString -> Integer
bs2vint'le = decodeVInt LittleEndian

-- pre-condition: n >= 0
vint2bs'le :: Integer -> ByteString
vint2bs'le = encodeVInt LittleEndian

-- big-endian
bs2vint'be :: ByteString -> Integer
bs2vint'be = decodeVInt BigEndian

-- pre-condition: n >= 0
vint2bs'be :: Integer -> ByteString
vint2bs'be = encodeVInt BigEndian
