{-# LANGUAGE FlexibleInstances #-}

module Tucker.Enc where

import Debug.Trace

import Data.Int
import Data.Bits
import Data.Word
import Data.Char
import qualified Data.Monoid as MND
import qualified Data.ByteString as BSR

import Control.Monad

import Tucker.Error

type ByteString = BSR.ByteString

(<>) :: Monoid a => a -> a -> a
(<>) = MND.mappend

-- mconcat :: Monoid a => [a] -> a
-- mconcat = MND.mconcat

-- mempty :: Monoid a => a
-- mempty = MND.mempty

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
encodeVInt _ 0 = Right $ BSR.empty
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
encodeInt 0 _ _ = BSR.empty
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

-- decoding
newtype Decoder r = Decoder { doDecode :: Endian -> ByteString -> (Either TCKRError r, ByteString) }

instance Functor Decoder where
    -- fmap f (Parser ps) = Parser $ \p -> [ (f a, b) | (a, b) <- ps p ]
    fmap f (Decoder d) =
        Decoder $ \end bs ->
            case d end bs of
                (Right r, rest) -> (Right $ f r, rest)
                (Left err, rest) -> (Left err, rest)

instance Applicative Decoder where
    pure = return

    (Decoder d1) <*> (Decoder d2) =
        Decoder $ \end bs ->
            case d1 end bs of
                (Right f, rest) ->
                    case d2 end rest of
                        (Right r, rest) -> (Right $ f r, rest)
                        (Left err, rest) -> (Left err, rest)

instance Monad Decoder where
    return res = Decoder $ \end rest -> (Right res, rest)

    fail err = Decoder $ \end bs -> (Left $ TCKRError err, bs)

    (Decoder d) >>= f =
        Decoder $ \end bs ->
            case d end bs of
                (Right r, rest) -> doDecode (f r) end rest
                (Left err, rest) -> (Left err, rest)

class Decodable t where
    decoder :: Decoder t

    decoderLE :: Decoder t
    decoderLE = Decoder $ \_ -> decodeLE

    decoderBE :: Decoder t
    decoderBE = Decoder $ \_ -> decodeBE

    decode :: Endian -> ByteString -> (Either TCKRError t, ByteString)
    decode = doDecode decoder

    decodeLE :: ByteString -> (Either TCKRError t, ByteString)
    decodeLE = decode LittleEndian

    decodeBE :: ByteString -> (Either TCKRError t, ByteString)
    decodeBE = decode BigEndian

    decodeAllLE :: ByteString -> Either TCKRError t
    decodeAllLE = fst . decodeLE

    decodeAllBE :: ByteString -> Either TCKRError t
    decodeAllBE = fst . decodeBE

intD :: Integral t => Int -> Decoder t
intD nbyte = Decoder $ \end bs ->
    if BSR.length bs >= nbyte then
        (Right $ fromInteger $ decodeInt nbyte end bs, BSR.drop nbyte bs)
    else
        (Left $ TCKRError
            ("no enough byte for a " ++
             (show nbyte) ++
             "-byte int"), bs)

byteD :: Decoder Word8
byteD =
    Decoder $ \_ bs ->
        if BSR.length bs >= 1 then
            (Right $ BSR.head bs, BSR.tail bs)
        else
            (Left $ TCKRError "need 1 byte", bs)

bsD :: Int -> Decoder ByteString
bsD len =
    Decoder $ \_ bs ->
        if BSR.length bs >= len then
            (Right $ BSR.take len bs, BSR.drop len bs)
        else
            (Left $ TCKRError ("need " ++ (show len) ++ " byte(s)"), bs)

listD :: Int -> Decoder t -> Decoder [t]
listD len d = forM [ 1 .. len ] (`seq` d)

lenD :: Decoder Int
lenD = Decoder $ \_ bs -> (Right $ BSR.length bs, bs)

checkLenD :: Int -> Decoder Bool
checkLenD len = (len <=) <$> lenD

ifD :: Bool -> t -> Decoder t -> Decoder t
ifD cond t d = if cond then return t else d

-- append a bytestring to the parsing buffer
appendD :: ByteString -> Decoder ()
appendD bs = Decoder $ \_ orig -> (Right (), BSR.append bs orig)

allD :: Decoder ByteString
allD = Decoder $ \_ bs -> (Right bs, BSR.empty)

instance Decodable Bool where
    decoder = do
        c <- byteD
        case c of
            0x00 -> return False
            0x01 -> return True
            _ -> fail "illegal bool"

instance Decodable Char where
    decoder = (chr. fromIntegral) <$> byteD

instance Decodable Int8 where
    decoder = fromIntegral <$> byteD

instance Decodable Word8 where
    decoder = byteD

instance Decodable Int16 where
    decoder = intD 2

instance Decodable Int32 where
    decoder = intD 4

instance Decodable Int64 where
    decoder = intD 8

instance Decodable Word16 where
    decoder = intD 2

instance Decodable Word32 where
    decoder = intD 4

instance Decodable Word64 where
    decoder = intD 8
