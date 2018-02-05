module Tucker.Msg.Hash256 where

import Data.Hex
import Data.Word
import Data.Char
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.Enc
import Tucker.Auth
import Tucker.Util

-- stored in little endian
data Hash256 = Hash256 ByteString deriving (Eq)

instance Ord Hash256 where
    -- reverse the little-endian number to compare from the highest byte
    compare (Hash256 h1) (Hash256 h2) =
        compare (BSR.reverse h1) (BSR.reverse h2)

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
    encode _ (Hash256 bs) =
        if BSR.length bs == 32 then bs
        else error "hash 256 length not correct"

instance Decodable Hash256 where
    decoder =
        bsD 32 >>= pure . Hash256

nullHash256 = Hash256 $ BSR.pack [ 0 | _ <- [ 1 .. 32 ] ]

hash256ToBS (Hash256 bs) = bs
bsToHash256 bs = Hash256 bs

stdHash256 :: ByteString -> Hash256
stdHash256 = Hash256 . ba2bs . sha256 . sha256

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
packHash256 (Hash256 bs) =
    let
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

    in Hash256 $ BSR.pack real
