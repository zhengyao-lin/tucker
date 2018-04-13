module Tucker.Msg.Hash256 where

import Data.Hex
import Data.Bits
import Data.Word
import Data.Char
import Data.LargeWord
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.Enc
import Tucker.Auth
import Tucker.Util
import Tucker.DeepSeq

-- stored in little endian
data Hash256 = Hash256 Word256 deriving (Eq)

instance NFData Hash256 where
    rnf (Hash256 hash) = hash `seq` ()

instance Ord Hash256 where
    compare (Hash256 n1) (Hash256 n2) = compare n1 n2

instance Num Hash256 where
    (Hash256 n1) + (Hash256 n2) = Hash256 (n1 + n2)
    (Hash256 n1) * (Hash256 n2) = Hash256 (n1 * n2)
    
    abs (Hash256 n) = Hash256 (abs n)
    -- negate (Hash256 n) = Hash256 (-n)
    negate (Hash256 n) = Hash256 (complement n + 1)
    -- TODO: using two's complment here, a bug in the large-word library
    signum (Hash256 n) = Hash256 (signum n)

    -- TODO: using the negate of Hash256 because the same problem of large-word
    fromInteger i =
        if i < 0 then
            -Hash256 (fromInteger (abs i))
        else
            Hash256 (fromInteger i)

instance Real Hash256 where
    toRational (Hash256 n) = toRational n

instance Enum Hash256 where
    toEnum i = Hash256 (toEnum i)
    fromEnum (Hash256 n) = fromEnum n

instance Integral Hash256 where
    quotRem (Hash256 n1) (Hash256 n2) =
        let (a, b) = quotRem n1 n2 in (Hash256 a, Hash256 b)

    toInteger (Hash256 n) = toInteger n

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
    encode end (Hash256 n) = encode end n

instance Decodable Hash256 where
    decoder = intD 32

nullHash256 = 0 :: Hash256

hash256ToBS (Hash256 n) = encodeLE n

bsToHash256 :: ByteString -> Hash256
bsToHash256 bs =
    case decodeLE bs of
        (Right v, _) -> v
        _ -> error "failed to decode hash256"

stdHash256 :: ByteString -> Hash256
stdHash256 = bsToHash256 . sha256 . sha256

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
