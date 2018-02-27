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

instance Encodable a => Encodable [a] where
    encode end = BSR.concat . (map (encode end))

instance Encodable a => Encodable (PartialList a) where
    encode end = encode end . FD.toList

instance (Encodable t1, Encodable t2) => Encodable (t1, t2) where
    encode end (a, b) = encode end a <> encode end b

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

allD :: Decoder ByteString
allD = Decoder $ \_ bs -> (Right bs, BSR.empty)

allD' :: Decoder ByteString
allD' = Decoder $ \_ bs -> (Right bs, bs)

getD = allD'

putD :: ByteString -> Decoder ()
putD bs = Decoder $ \_ _ -> (Right (), bs)

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
    decoder = intD 2

instance Decodable Word32 where
    decoder = intD 4

instance Decodable Word64 where
    decoder = intD 8

instance Decodable Word256 where
    decoder = intD 32

instance (Decodable t1, Decodable t2) => Decodable (t1, t2) where
    decoder = (,) <$> decoder <*> decoder

instance Decodable String where
    decoder = BS.unpack <$> allD

-- -- decode as many t as possible
-- instance Decodable t => Decodable [t] where
--     decoder = many decoder
