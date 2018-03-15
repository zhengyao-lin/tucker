module Test.Enc where

import qualified Data.ByteString as BSR

import Test.Util
import Test.HUnit
import Test.Common

hash256Test = TestCase $ do
    let h1 = read "00000000000404cb000000000000000000000000000000000000000000000000"
        h2 = read "00000000008004cb000000000000000000000000000000000000000000000000"
        h3 = read "7f00000000000000000000000000000000000000000000000000000000000000"
        h4 = read "0000000000000000000000000000000000000000000000000000000000000000"

        h5 = read "000000000000000000000000000000000000000000000000000000000000aabb"
        h6 = read "00000000000000000000000000000000000000000000000000000000000000aa"

        h7 = read "ff00000000000000000000000000000000000000000000000000000000000000"

    assertBool "wrong compare result 1" $ h2 > h1
    assertBool "wrong compare result 2" $ h3 > h2
    assertBool "wrong compare result 3" $ h7 > h3 -- hash256 is unsigned
    assertBool "wrong compare result 4" $ h5 > h6
    assertBool "wrong compare result 5" $ h6 > h4

    assertEqual "wrong encode result 1" 0x1b0404cb (packHash256 h1)
    assertEqual "wrong encode result 2" 0x1c008004 (packHash256 h2)

    assertEqual "wrong decode result 1" h1 (unpackHash256 0x1b0404cb)
    assertEqual "should overflow 3" h3 (unpackHash256 0x2200007f)
    assertEqual "should overflow 4" h4 (unpackHash256 0x237fffff)

    assertEqual "wrong decode result 5" h5 (unpackHash256 0x0300aabb)
    assertEqual "wrong decode result 6" h6 (unpackHash256 0x0200aabb)

basicEncTest = TestCase $ do
    -- encodeVInt
    -- encodeInt

    forM_ encode_map_be $ \(num, vint_bs, vword_bs) -> do
        assertEqual ("wrong big-endian encoding as vint of " ++ show num)
            vint_bs (encodeVInt BigEndian num)

        assertEqual ("wrong little-endian encoding as vint of " ++ show num)
            (BSR.reverse vint_bs) (encodeVInt LittleEndian num)

        assertEqual ("wrong big-endian encoding as vword of " ++ show num)
            vword_bs (encodeVWord BigEndian num)

        assertEqual ("wrong little-endian encoding as vword of " ++ show num)
            (BSR.reverse vword_bs) (encodeVWord LittleEndian num)

basicDecTest = TestCase $ do
    return ()

encTests = TestList [
        TestLabel "basic encoding tests" basicEncTest,
        TestLabel "basic decoding tests" basicDecTest,

        TestLabel "hash256 basic" hash256Test
    ]
