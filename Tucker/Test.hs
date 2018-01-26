{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.Test where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import System.Random

import Test.HUnit

import Control.Monad

import Tucker.Enc
import Tucker.Std
import Tucker.Msg
import Tucker.Conf
import Tucker.Atom
import Tucker.Util
import Tucker.Error

import Tucker.P2P.Init
import Tucker.P2P.Util
import Tucker.P2P.Node
import Tucker.P2P.Action

import Tucker.Chain.Object

simpleBPH :: Hash256 -> IO BlockPayloadHashed
simpleBPH prev_block = do
    nonce <- randomIO
    timestamp <- unixTimestamp

    return $ blockPayloadToBPH $ BlockPayload {
        header = BlockHeader {
            vers = 1,
            prev_block = prev_block,
            merkle_root = nullHash256,

            timestamp = timestamp,
            diff_bits = 1,
            nonce = nonce,

            txn_count = VInt 0
        },

        txns = []
    }

assertEitherRight :: Either TCKRError t -> IO t
assertEitherRight (Right v) = return v
assertEitherRight (Left err) = assertFailure $ show err

assertEitherLeft :: Either TCKRError t -> IO ()
assertEitherLeft (Right _) = assertFailure "expecting error"
assertEitherLeft (Left err) = return ()

assertInsertTree tree blocks =
    foldM (\t b -> assertEitherRight $ insertToTree t b) tree blocks

assertFailInsertTree tree blocks =
    forM blocks (assertEitherLeft . insertToTree tree)

blockBasicTest = TestCase $ do
    b1 <- simpleBPH nullHash256
    b2 <- simpleBPH $ bph_hash b1
    b3 <- simpleBPH $ bph_hash b1
    b4 <- simpleBPH $ bph_hash b2
    b5 <- simpleBPH $ bph_hash b3
    b6 <- simpleBPH $ bph_hash b3
    b7 <- simpleBPH $ bph_hash b5

    {-
    
    the tree1 looks like this:
    1
    -- 2
       -- 4
    -- 3
       -- 5
          -- 7
       -- 6

    the tree2 should looks like this:
    1
    -- 3
       -- 5
          -- 7
    -}

    assertEqual "empty tree should have height 0" 0 (treeHeight emptyTree)
    assertBool "should not be in tree" (not $ isBlockInTree emptyTree $ bph_hash b1)

    -- inserting non-genesis block should fail
    assertFailInsertTree emptyTree [ b2, b3, b4, b5, b6, b7 ]

    tree1 <- assertInsertTree emptyTree [ b1, b2, b3, b4, b5, b6, b7 ]
    tree2 <- assertInsertTree emptyTree [ b1, b3, b5, b7 ]

    assertBool "should all be in tree" $ all (isBlockInTree tree1) $
        map (bph_hash) [ b1, b2, b3, b4, b5, b6, b7 ]

    assertBool "should be tree" (isValidTree tree1)
    assertBool "should be tree" (isValidTree tree2)

    assertEqual "wrong tree height" 4 (treeHeight tree1)
    assertEqual "wrong tree height" 4 (treeHeight tree2)

    -- re-inserting should fail
    assertFailInsertTree tree1 [ b1, b2, b3, b4, b5, b6, b7 ]

    let chain = fixTree tree1

    assertEqual "should only be 1 chain" 1 (length chain)
    assertEqual "wrong fixed chain" tree2 (chainToTree $ head chain)

blockTreePartTest = TestCase $ do
    b1 <- simpleBPH nullHash256
    b2 <- simpleBPH $ bph_hash b1
    b3 <- simpleBPH $ bph_hash b1
    b4 <- simpleBPH $ bph_hash b2
    b5 <- simpleBPH $ bph_hash b3
    b6 <- simpleBPH $ bph_hash b3
    b7 <- simpleBPH $ bph_hash b5

    -- see the structure in the test above

    tree1 <- assertInsertTree emptyTree [ b1, b2, b3, b4, b5, b6, b7 ]

    let tp1 = treeToTreePart tree1
        (tp2, tp3) = splitTreePart 2 tp1
        (tp4, tp5) = splitTreePart 100 tp2
        (tp6, tp7) = splitTreePart 1 tp3

    assertEqual "should be equal when converted back" (Just tree1) (treePartToTree tp1)
    assertBool "should be fail converting" $
        all ((== Nothing) . treePartToTree) [ tp3, tp5, tp6, tp7 ]

    assertEqual "split on exceeding length should yield the same result" tp2 tp4
    -- assertEqual "should be empty" mempty tp5

    assertEqual "should be the same tree part" tp1 (tp2 <> tp3)
    assertEqual "should be the same tree part" tp2 (tp4 <> tp5)
    assertEqual "should be the same tree part" tp3 (tp6 <> tp7)
    assertEqual "should be the same tree part" tp1 (mconcat [ tp4, tp5, tp6, tp7 ])

    assertEqual "should obey associativity" ((tp2 <> tp6) <> tp7) (tp2 <> (tp6 <> tp7))

blockTest = TestList [
        TestLabel "block tree/chain basic" blockBasicTest,
        TestLabel "block tree part" blockTreePartTest
    ]

{-

need to check bitseed.xf2.org
testnet-seed.bluematt.me
use this !!! seed.tbtc.petertodd.org

99.242.230.163
76.111.96.126
130.235.100.241
198.251.83.19
138.68.229.19

let net = btc_testnet3
addr <- ipToAddr "88.198.20.152" 18333
sock <- buildSocketTo addr
connect sock (addrAddress addr)
selfaddr <- ip4ToNetAddr "127.0.0.1" (listenPort net) btc_cli_service
msg <- encodeMsg net BTC_CMD_VERSION $ encodeVersionPayload net selfaddr
send sock msg
recv sock 1024
recv sock 1024

-- handshake finished

msg <- encodeMsg net BTC_CMD_GETBLOCKS $ encodeGetblocksPayload [] nullHash256
send sock msg
recv sock 1024

msg <- encodeMsg net BTC_CMD_GETDATA $ encodeGetdataPayload [ InvVector INV_TYPE_BLOCK (read "00000000700e92a916b46b8b91a14d1303d5d91ef0b09eecc3151fb958fd9a2e") ]
send sock msg
recv sock 1024

msg <- encodeMsg btc_testnet3 BTC_CMD_TX $ encodeTxPayload btc_testnet3 "933qtT8Ct7rGh29Eyb5gG69QrWmwGein85F1kuoShaGjJFFBSjk" [ OutPoint (decodeRPCHash "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5") 0 ] [ (10000, "miro9ZNPjcLnqvnJpSm8P6CUf1WPU98jET"), (119990000, "mvU2ysD322amhCeCPMhPc3L7hKDGGWSBz7") ]

send sock msg
recv sock 1024

-}
