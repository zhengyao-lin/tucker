{-# LANGUAGE ForeignFunctionInterface #-}

module Tucker.Msg.Hash256 where

import Data.Hex
import Data.Int
import Data.Bits
import Data.Word
import Data.Char
import Data.Hashable
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB

import System.IO.Unsafe

import Foreign.Ptr

import Tucker.Enc
import Tucker.Auth
import Tucker.Util
import Tucker.DeepSeq

foreign import ccall "hash256_compare" c_hash256_compare :: Ptr Word8 -> Ptr Word8 -> IO Int8
foreign import ccall "hash256_add" c_hash256_add :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "hash256_mul" c_hash256_mul :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "hash256_neg" c_hash256_neg :: Ptr Word8 -> Ptr Word8 -> IO ()

-- when used as an integer, the string is interpreted as an little-endian number
newtype Hash256 = Hash256 ByteString deriving (Eq)

-- stored in little endian
-- data Hash256 = Hash256 Word256 deriving (Eq)

instance NFData Hash256 where
    rnf (Hash256 raw) = raw `seq` ()

instance Ord Hash256 where
    compare (Hash256 a) (Hash256 b) =
        unsafePerformIO $
        BA.withByteArray a $ \a ->
            BA.withByteArray b $ \b -> do
                res <- c_hash256_compare a b
                return $ case res of
                    -1 -> LT
                    0  -> EQ
                    1  -> GT
                    e  -> error ("unexpected result " ++ show e)

instance Num Hash256 where
    (Hash256 a) + (Hash256 b) =
        bsToHash256 $
        snd $ unsafePerformIO $
        BA.withByteArray a $ \a ->
            BA.withByteArray b $ \b ->
                BA.allocRet 32 $ \result ->
                    c_hash256_add a b result
        
    (Hash256 a) * (Hash256 b) =
        bsToHash256 $
        snd $ unsafePerformIO $
        BA.withByteArray a $ \a ->
            BA.withByteArray b $ \b ->
                BA.allocRet 32 $ \result ->
                    c_hash256_mul a b result
    
    abs = id
    -- negate (Hash256 n) = Hash256 (-n)
    -- negate _ = error "hash256 method not implemented" -- Hash256 (complement n + 1)
    -- TODO: using two's complment here, a bug in the large-word library

    negate (Hash256 a) =
        bsToHash256 $
        snd $ unsafePerformIO $
        BA.withByteArray a $ \a ->
            -- :: IO ((), ByteString)
            BA.allocRet 32 $ \result ->
                c_hash256_neg a result

    signum = const 1

    -- TODO: using the negate of Hash256 because the same problem of large-word
    fromInteger = intToHash256

instance Real Hash256 where
    toRational h = toRational (hash256ToInt h :: Integer)

instance Enum Hash256 where
    toEnum = intToHash256
    fromEnum = hash256ToInt

instance Integral Hash256 where
    quotRem a b =
        let c = hash256ToInt a :: Integer
            d = hash256ToInt b :: Integer
            (q, r) = quotRem c d
        in 
            (intToHash256 q, intToHash256 r)

    toInteger = hash256ToInt

instance Show Hash256 where
    -- display order is the reversed order of the internal format
    show = map toLower . hex . BS.unpack . BS.reverse . hash256ToBS

instance Read Hash256 where
    readsPrec _ str = [(
        bsToHash256 .
        BS.reverse .
        BS.pack .
        (!!0) .
        unhex .
        map toUpper $ str, "")]

instance Encodable Hash256 where
    encode end (Hash256 bs) = bs

instance Decodable Hash256 where
    decoder = -- intD 32
        Hash256 <$> bsD 32

instance Sizeable Hash256 where
    sizeOf _ = 32

instance Hashable Hash256 where
    hashWithSalt salt (Hash256 bs) =
        (fi (decodeFailLE (BSR.drop 28 bs) :: Word32)) `xor` salt

nullHash256 = Hash256 nullHash256BS
nullHash256BS = BSR.replicate 32 0

isNullHash256 = (== nullHash256)

intToHash256 :: (Bits t, Integral t) => t -> Hash256
intToHash256 n =
    bsToHash256 $
    encodeInt 32 LittleEndian n

hash256ToInt :: (Bits t, Integral t) => Hash256 -> t
hash256ToInt (Hash256 bs) =
    decodeVWord LittleEndian bs

hash256ToBS (Hash256 bs) = bs

bsToHash256 :: ByteString -> Hash256
bsToHash256 bs =
    case decodeLE bs of
        (Right v, _) -> v
        (Left e, _) -> error ("failed to decode hash256: " ++ show e)

stdHash256 :: ByteString -> Hash256
stdHash256 = bsToHash256 . doubleSHA256

-- decodeBE $ hex2bs "9b0fc92260312ce44e74ef369f5c66bbb85848f2eddd5a7a1cde251e54ccfdd5" :: (Either TCKRError Hash256, ByteString)

-- -- real bit length of the hash
-- validBits :: Hash256 -> Word
-- validBits (Hash256 bs) =
--     let
--         rvalid = BSR.dropWhile (== 0) $ BSR.reverse bs
--     in
--         if BSR.null rvalid then 0
--         else
--             8 * (BSR.length rvalid - 1) +
--             (8 - countLeadingZeros (BSR.head rvalid))

-- TODO: too messy!!!
packHash256 :: Hash256 -> Word32
packHash256 hash =
    let
        bs = hash256ToBS hash

        valid = BSR.unpack $ BSR.dropWhile (== 0) $ BSR.reverse bs -- remove leading zero bytes
        valid3 = take 3 valid

        has_first_bit = head valid >= 0x80

        -- first three valid bytes
        first3 =
            -- first bit is 1, increase the exponent and shift the mantissa
            (if has_first_bit then (0:) . take 2 else id) $

            if length valid3 == 3 then valid3
            else
                -- fill the lacking low bytes with zeros
                valid3 ++ replicate (3 - length valid3) 0

        nsize = length valid +
            if has_first_bit then 1
            else 0

        -- the bytes were reversed to big endian
        -- TODO: ignoring errors and the ssign bit here
        (Right res, _) = decodeBE $ BSR.pack $ (fi nsize) : first3
    in res

unpackHash256 :: Word32 -> Hash256
unpackHash256 packed =
    let
        enc = BSR.unpack $ encodeBE packed
        size = fi $ enc !! 0
        val = reverse $ drop 1 enc

        fill len v lst =
            if length lst < len then
                lst ++ replicate (len - length lst) v
            else
                -- TODO: no overflow report
                take len lst

        real = fill 32 0 $
            if size <= 3 then
                drop (3 - size) val
            else
                replicate (size - 3) 0 ++ val

    in bsToHash256 $ BSR.pack real
