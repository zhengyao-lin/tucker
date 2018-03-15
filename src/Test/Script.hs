module Test.Script where

import qualified Data.ByteString as BSR

import Test.Util
import Test.HUnit
import Test.Common

scriptTest1 = TestCase $ do
    let sc1 = [ OP_NOP ]
        sc2 = [ OP_IF True 1, OP_ENDIF ]
        sc3 = [
                OP_IF True 6,
                OP_IF False 4,
                OP_IF True 2,
                OP_NOP,
                OP_ENDIF,
                OP_ENDIF,
                OP_ELSE 2,
                OP_NOP,
                OP_ENDIF
            ]

        sc4 = [ OP_PUSHDATA $ BSR.pack [ 0x00 ] ]

        sc5 = [
                OP_CONST 1,
                OP_IF True 2,
                OP_CONST 2,
                OP_ELSE 3,
                OP_CONST 3,
                OP_CONST 3,
                OP_ENDIF
            ]

    assertEqual "wrong script decode result 1" sc1 (decodeFailLE (encodeLE sc1))
    assertEqual "wrong script encode result 2" (BSR.pack [ 0x63, 0x68 ]) (encodeLE sc2)
    assertEqual "wrong script decode result 3" sc3 (decodeFailLE (encodeLE sc3))
    assertEqual "wrong script encode result 4" (BSR.pack [ 0x01, 0x00 ]) (encodeLE sc4)
    assertEqual "wrong script decode result 3" sc5 (decodeFailLE (encodeLE sc5))

    res <- runScript sc5

    assertEqual "wrong script exec result 5" [
            toItem (2 :: Integer)
        ] res

scriptTests = TestList [
        TestLabel "script test 1" scriptTest1
    ]

