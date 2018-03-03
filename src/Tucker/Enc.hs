{-# LANGUAGE FlexibleInstances #-}

module Tucker.Enc where

import Debug.Trace

import Data.Int
import Data.Bits
import Data.Word
import Data.Char
import Data.LargeWord
import qualified Data.Monoid as MND
import qualified Data.Foldable as FD
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Exception
import Control.Monad.Catch
import Control.Applicative

import Tucker.Util
import Tucker.Error

type ByteString = BSR.ByteString

(<>) :: Monoid a => a -> a -> a
(<>) = MND.mappend

-- mconcat :: Monoid a => [a] -> a
-- mconcat = MND.mconcat

-- mempty :: Monoid a => a
-- mempty = MND.mempty

data Placeholder = Placeholder

data Endian = LittleEndian | BigEndian deriving (Show, Read, Eq)

class Encodable t where
    encode :: Endian -> t -> ByteString
    
    encodeLE :: t -> ByteString
    encodeLE = encode LittleEndian

    encodeBE :: t -> ByteString
    encodeBE = encode BigEndian

instance Encodable Placeholder where
    encode _ _ = BSR.empty

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

instance Encodable Word256 where
    encode = encodeInt 32

-- encode an integer with variable length
-- to the shortest form
instance Encodable Integer where
    encode = encodeVInt

instance Encodable a => Encodable [a] where
    encode end = BSR.concat . (map (encode end))

instance Encodable a => Encodable (PartialList a) where
    encode end = encode end . FD.toList

instance (Encodable t1, Encodable t2) => Encodable (t1, t2) where
    encode end (a, b) = encode end a <> encode end b

instance (Encodable t1, Encodable t2, Encodable t3) => Encodable (t1, t2, t3) where
    encode end (a, b, c) = encode end a <> encode end b <> encode end c

bchar :: Integral t => t -> ByteString
bchar = BSR.singleton . fromIntegral

-- encode integer to the shortest possible encoding
-- using 2's complement
encodeVInt' :: Endian -> Integer -> ByteString
encodeVInt' end 0 = BSR.empty
encodeVInt' end (-1) = bchar 0xff
encodeVInt' end num =
    case end of
        LittleEndian -> BSR.reverse res
        BigEndian -> res
    where
        pred (bs, num) = not $
            (num == 0 || num == -1) &&
            not (BSR.null bs) &&
            (BSR.head bs < 0x80) == (num >= 0) -- sign is correct

        res =
            -- BSR.dropWhile (== 0) $
            fst $ head $
            dropWhile pred $
            flip iterate (BSR.empty, num) $ \(bs, num) ->
                let least = num .&. 0xff
                in  (BSR.cons (fi least) bs, num `shiftR` 8)

-- encode int to an indefinite size
-- inverse of decodeVInt
encodeVInt :: (Integral t, Bits t) => Endian -> t -> ByteString
encodeVInt end num = encodeVInt' end (fi num)

-- similar to encodeVInt
-- but it will trim all unnecessary zero bytes
-- from the high end
-- inverse of decodeVWord
encodeVWord :: (Integral t, Bits t) => Endian -> t -> ByteString
encodeVWord end num =
    case end of
        LittleEndian -> BSR.reverse $ BSR.dropWhile (== 0) $ BSR.reverse res
        BigEndian -> BSR.dropWhile (== 0) res
    where res = encodeVInt' end (fi num)

-- encode int to a fixed-size string(will truncate/fill the resultant string)
encodeInt :: (Integral t, Bits t) => Int -> Endian -> t -> ByteString
encodeInt nbyte end num =
    if diff > 0 then -- fill
        if num < 0 then BSR.replicate diff 0xff `fill` res
        else BSR.replicate diff 0x00 `fill` res
    else
        BSR.take nbyte res
    
    where res = encodeVInt end num
          len = BSR.length res
          diff = nbyte - len
          fill pref a =
              case end of
                  LittleEndian -> a <> pref
                  BigEndian -> pref <> a

decodeInt' :: Integer -> Endian -> ByteString -> Integer
decodeInt' init LittleEndian = BSR.foldr' (\x a -> shiftL a 8 + fi x) init
decodeInt' init BigEndian = BSR.foldl' (\a x -> shiftL a 8 + fi x) init

-- decodes a bytestring as an integer and determines the sign
decodeVInt :: Integral t => Endian -> ByteString -> t
decodeVInt end bs =
    if BSR.null bs then 0
    else if sign < 0x80 then fi (decodeInt' 0 end bs)
    else fi (decodeInt' (-1) end bs)
    where
        sign =
            case end of
                LittleEndian -> BSR.last bs
                BigEndian -> BSR.head bs

-- similar to above, but doesn't care about the sign
decodeVWord :: Integral t => Endian -> ByteString -> t
decodeVWord end bs = fi (decodeInt' 0 end bs)

-- little-endian
bs2vwordLE :: ByteString -> Integer
bs2vwordLE = decodeVWord LittleEndian

vword2bsLE :: Integer -> ByteString
vword2bsLE = encodeVWord LittleEndian

-- big-endian
bs2vwordBE :: ByteString -> Integer
bs2vwordBE = decodeVWord BigEndian

vword2bsBE :: Integer -> ByteString
vword2bsBE = encodeVWord BigEndian

-- decoding
newtype Decoder r = Decoder { doDecode :: Endian -> ByteString -> (Either TCKRError r, ByteString) }

instance Functor Decoder where
    -- fmap f (Parser ps) = Parser $ \p -> [ (f a, b) | (a, b) <- ps p ]
    fmap f (Decoder d) =
        Decoder $ \end bs ->
            case d end bs of
                (Right r, rest) -> (Right (f r), rest)
                (Left err, rest) -> (Left err, rest)

instance Applicative Decoder where
    pure res = Decoder $ \end rest -> (Right res, rest)

    Decoder d1 <*> Decoder d2 =
        Decoder $ \end bs ->
            case d1 end bs of
                (Right f, rest) ->
                    case d2 end rest of
                        (Right r, rest) -> (Right (f r), rest)
                        (Left err, rest) -> (Left err, rest)

                (Left err, rest) -> (Left err, rest)

instance Monad Decoder where
    return = pure

    fail err = Decoder $ \end bs -> (Left $ TCKRError err, bs)

    Decoder d >>= f =
        Decoder $ \end bs ->
            case d end bs of
                (Right r, rest) -> doDecode (f r) end rest
                (Left err, rest) -> (Left err, rest)

instance Alternative Decoder where
    empty = fail "empty decoder"
    Decoder d1 <|> Decoder d2 =
        Decoder $ \end bs ->
            case d1 end bs of
                r@(Right _, rest) -> r
                (Left e1, _) ->
                    case d2 end bs of
                        r@(Right _, rest) -> r
                        (Left e2, rest) ->
                            (Left $ wrapError e2 (show e1), rest)

instance MonadThrow Decoder where
    throwM = fail . show

instance MonadCatch Decoder where
    catch d proc =
        Decoder $ \end bs ->
            case doDecode d end bs of
                r@(Right _, _) -> r
                r@(Left exc, _) ->
                    case fromException $ toException exc of
                        Nothing -> r
                        Just e -> doDecode (proc e) end bs

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

    decodeFailLE :: ByteString -> t
    decodeFailLE = (either throw id) . decodeAllLE

    decodeFailBE :: ByteString -> t
    decodeFailBE = (either throw id) . decodeAllBE

intD :: Integral t => Int -> Decoder t
intD nbyte = Decoder $ \end bs ->
    if BSR.length bs >= nbyte then
        (Right $ decodeVInt end (BSR.take nbyte bs), BSR.drop nbyte bs)
    else
        (Left $ TCKRError
            ("no enough byte for a " ++ (show nbyte) ++ "-byte int"), bs)

wordD :: Integral t => Int -> Decoder t
wordD nbyte = Decoder $ \end bs ->
    if BSR.length bs >= nbyte then
        (Right $ decodeVWord end (BSR.take nbyte bs), BSR.drop nbyte bs)
    else
        (Left $ TCKRError
            ("no enough byte for a " ++ (show nbyte) ++ "-byte word"), bs)

byteD :: Decoder Word8
byteD =
    Decoder $ \_ bs ->
        if BSR.length bs >= 1 then
            (Right $ BSR.head bs, BSR.tail bs)
        else
            (Left $ TCKRError "need 1 byte", bs)

beginWithByteD :: Word8 -> Decoder ()
beginWithByteD byte =
    Decoder $ \_ bs ->
        if BSR.length bs >= 1 then
            if BSR.head bs == byte then
                (Right (), BSR.tail bs)
            else
                (Left $ TCKRError "first byte not match", bs)
        else
            (Left $ TCKRError "need 1 byte", bs)

bsD :: Int -> Decoder ByteString
bsD len =
    Decoder $ \_ bs ->
        if BSR.length bs >= len then
            (Right $ BSR.take len bs, BSR.drop len bs)
        else
            (Left $ TCKRError ("need " ++ (show len) ++ " byte(s)"), bs)

peekByteD :: Decoder Word8
peekByteD =
    Decoder $ \_ bs ->
        if BSR.length bs >= 1 then
            (Right $ BSR.head bs, bs)
        else
            (Left $ TCKRError "peek need 1 byte", bs)

listD :: Int -> Decoder t -> Decoder [t]
listD = replicateM
    -- forM [ 1 .. len ] (\l -> traceShowM l >> d)
    -- replicateM

lenD :: Decoder Int
lenD = Decoder $ \_ bs -> (Right $ BSR.length bs, bs)

checkLenD :: Int -> Decoder Bool
checkLenD len = (len <=) <$> lenD

ifD :: Bool -> t -> Decoder t -> Decoder t
ifD cond t d = if cond then return t else d

-- append a bytestring to the parsing buffer
appendD :: ByteString -> Decoder ()
appendD bs = Decoder $ \_ orig -> (Right (), BSR.append bs orig)

forceEndian :: Endian -> Decoder a -> Decoder a
forceEndian end (Decoder d) =
    Decoder $ \_ bs -> d end bs

allD :: Decoder ByteString
allD = Decoder $ \_ bs -> (Right bs, BSR.empty)

allD' :: Decoder ByteString
allD' = Decoder $ \_ bs -> (Right bs, bs)

getD = allD'

putD :: ByteString -> Decoder ()
putD bs = Decoder $ \_ _ -> (Right (), bs)

-- only feed part of the current bs to the given decoder
quota :: Int -> Decoder t -> Decoder t
quota len d =
    Decoder $ \end bs ->
        let part = BSR.take len bs
            (res, rest) = doDecode d end part
        in (res, rest <> BSR.drop len bs)

instance Decodable Placeholder where
    decoder = return Placeholder

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
    decoder = wordD 2

instance Decodable Word32 where
    decoder = wordD 4

instance Decodable Word64 where
    decoder = wordD 8

instance Decodable Word256 where
    decoder = wordD 32

instance (Decodable t1, Decodable t2) => Decodable (t1, t2) where
    decoder = (,) <$> decoder <*> decoder

instance Decodable String where
    decoder = BS.unpack <$> allD

instance Decodable Integer where
    decoder = Decoder $ \end bs ->
        (Right (decodeVInt end bs), BSR.empty)

-- -- decode as many t as possible
-- instance Decodable t => Decodable [t] where
--     decoder = many decoder
