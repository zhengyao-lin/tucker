{-# LANGUAGE FlexibleInstances #-}

module Tucker.Enc where

import Debug.Trace

import Data.Int
import Data.Bits
import Data.Word
import Data.Char
import qualified Data.ByteString as BSR

import Tucker.Error

type ByteString = BSR.ByteString

data Endian = LittleEndian | BigEndian deriving (Show, Read, Eq)

class Encodable t where
    encode :: Endian -> t -> ByteString
    
    encodeLE :: t -> ByteString
    encodeLE = encode LittleEndian

    encodeBE :: t -> ByteString
    encodeBE = encode BigEndian

instance Encodable ByteString where
    encode _ = id

instance Encodable Bool where
    encode _ v = if v then bchar 1 else bchar 0

instance Encodable Char where
    encode _ = bchar . ord

instance Encodable Int8 where
    encode _ = bchar

instance Encodable Word8 where
    encode _ = bchar

instance Encodable Int16 where
    encode = encodeInt 2

instance Encodable Int32 where
    encode = encodeInt 4

instance Encodable Int64 where
    encode = encodeInt 8

instance Encodable Word16 where
    encode = encodeInt 2

instance Encodable Word32 where
    encode = encodeInt 4

instance Encodable Word64 where
    encode = encodeInt 8

instance Encodable a => Encodable [a] where
    encode end = BSR.concat . (map (encode end))

bchar :: Integral t => t -> ByteString
bchar = BSR.singleton . fromIntegral

-- VInt > 0
encodeVInt :: Endian -> Integer -> Either TCKRError ByteString
encodeVInt _ 0 = Right $ BSR.pack []
encodeVInt end num = do
    if num < 0 then
        Left $ TCKRError "encoding negative variable length ineteger"
    else do
        let
            (rest, m) = num `divMod` 0x100
    
        rest_enc <- encodeVInt end rest

        if end == LittleEndian then
            Right $ BSR.append (bchar m) rest_enc
        else
            Right $ BSR.append rest_enc (bchar m)

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
bs2vintLE :: ByteString -> Integer
bs2vintLE = decodeVInt LittleEndian

-- pre-condition: n >= 0
vint2bsLE :: Integer -> Either TCKRError ByteString
vint2bsLE = encodeVInt LittleEndian

-- big-endian
bs2vintBE :: ByteString -> Integer
bs2vintBE = decodeVInt BigEndian

-- pre-condition: n >= 0
vint2bsBE :: Integer -> Either TCKRError ByteString
vint2bsBE = encodeVInt BigEndian
