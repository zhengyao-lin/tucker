{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.Test where

import qualified Data.ByteString as BSR
    
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString

import System.IO
import System.Random

import Test.HUnit

import Control.Monad
import Control.Monad.Loops

import Control.Concurrent
import Control.Concurrent.Thread.Delay

import Tucker.Enc
import Tucker.Msg
import Tucker.Conf
import Tucker.Atom
import Tucker.Util
import Tucker.Error

import Tucker.P2P.Init
-- import Tucker.P2P.Util
-- import Tucker.P2P.Node
-- import Tucker.P2P.Action

import Tucker.Chain.Object
-- import Tucker.Chain.Cached

simpleBlock :: Hash256 -> IO Block
simpleBlock prev_hash = do
    nonce <- randomIO
    timestamp <- unixTimestamp

    let tmp = Block {
        block_hash = nullHash256,

        vers = 1,
        prev_hash = prev_hash,
        merkle_root = nullHash256,

        timestamp = timestamp,

        hash_target = read "0000000000000000000000000000000000000000000000000000000000000000",
        
        nonce = nonce,

        txns = []
    }

    return $ tmp { block_hash = hashBlock tmp }

assertEitherRight :: Either TCKRError t -> IO t
assertEitherRight (Right v) = return v
assertEitherRight (Left err) = assertFailure $ show err

assertEitherLeft :: Either TCKRError t -> IO ()
assertEitherLeft (Right _) = assertFailure "expecting error"
assertEitherLeft (Left err) = return ()

-- assertInsertTree tree blocks =
--     foldM (\t b -> assertEitherRight $ insertToTree t b) tree blocks

-- assertFailInsertTree tree blocks =
--     forM blocks (assertEitherLeft . insertToTree tree)

-- blockBasicTest = TestCase $ do
--     b1 <- simpleBPH nullHash256
--     b2 <- simpleBPH $ bph_hash b1
--     b3 <- simpleBPH $ bph_hash b1
--     b4 <- simpleBPH $ bph_hash b2
--     b5 <- simpleBPH $ bph_hash b3
--     b6 <- simpleBPH $ bph_hash b3
--     b7 <- simpleBPH $ bph_hash b5

--     {-
    
--     the tree1 looks like this:
--     1
--     -- 2
--        -- 4
--     -- 3
--        -- 5
--           -- 7
--        -- 6

--     the tree2 should looks like this:
--     1
--     -- 3
--        -- 5
--           -- 7
--     -}

--     assertEqual "empty tree should have height 0" 0 (treeHeight emptyTree)
--     assertBool "should not be in tree" (not $ isBlockInTree emptyTree $ bph_hash b1)

--     -- inserting non-genesis block should fail
--     assertFailInsertTree emptyTree [ b2, b3, b4, b5, b6, b7 ]

--     tree1 <- assertInsertTree emptyTree [ b1, b2, b3, b4, b5, b6, b7 ]
--     tree2 <- assertInsertTree emptyTree [ b1, b3, b5, b7 ]

--     assertBool "should all be in tree" $ all (isBlockInTree tree1) $
--         map (bph_hash) [ b1, b2, b3, b4, b5, b6, b7 ]

--     assertBool "should be tree" (isValidTree tree1)
--     assertBool "should be tree" (isValidTree tree2)

--     assertEqual "wrong tree height" 4 (treeHeight tree1)
--     assertEqual "wrong tree height" 4 (treeHeight tree2)

--     -- re-inserting should fail
--     assertFailInsertTree tree1 [ b1, b2, b3, b4, b5, b6, b7 ]

--     let chain = fixTree tree1

--     assertEqual "should only be 1 chain" 1 (length chain)
--     assertEqual "wrong fixed chain" tree2 (chainToTree $ head chain)

-- blockTreePartTest = TestCase $ do
--     b1 <- simpleBPH nullHash256
--     b2 <- simpleBPH $ bph_hash b1
--     b3 <- simpleBPH $ bph_hash b1
--     b4 <- simpleBPH $ bph_hash b2
--     b5 <- simpleBPH $ bph_hash b3
--     b6 <- simpleBPH $ bph_hash b3
--     b7 <- simpleBPH $ bph_hash b5

--     -- see the structure in the test above

--     tree1 <- assertInsertTree emptyTree [ b1, b2, b3, b4, b5, b6, b7 ]

--     let tp1 = treeToTreePart tree1
--         (tp2, tp3) = splitTreePart 2 tp1
--         (tp4, tp5) = splitTreePart 100 tp2
--         (tp6, tp7) = splitTreePart 1 tp3

--     assertEqual "should be equal when converted back" (Just tree1) (treePartToTree tp1)
--     assertBool "should be fail converting" $
--         all ((== Nothing) . treePartToTree) [ tp3, tp5, tp6, tp7 ]

--     assertEqual "split on exceeding length should yield the same result" tp2 tp4
--     -- assertEqual "should be empty" mempty tp5

--     assertEqual "should be the same tree part" tp1 (tp2 <> tp3)
--     assertEqual "should be the same tree part" tp2 (tp4 <> tp5)
--     assertEqual "should be the same tree part" tp3 (tp6 <> tp7)
--     assertEqual "should be the same tree part" tp1 (mconcat [ tp4, tp5, tp6, tp7 ])

--     assertEqual "should obey associativity" ((tp2 <> tp6) <> tp7) (tp2 <> (tp6 <> tp7))

--     -- forM (zip [ 1 .. ] [ tp4, tp5, tp6, tp7 ]) $ \(i, tp) -> do
--     --     withBinaryFile ("/home/rodlin/tucker-btp/btp." ++ show i) WriteMode $ \handle -> do
--     --         BSR.hPut handle (encodeLE tp)

--     return ()

blockTest = TestList [
        -- TestLabel "block tree/chain basic" blockBasicTest,
        -- TestLabel "block tree part" blockTreePartTest
    ]

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

msgTest = TestList [
        TestLabel "hash256 basic" hash256Test
    ]

allTest = TestList [
        blockTest, msgTest
    ]

{-

13:50 4958
13:55 9319
14:01 12545 stop

14:09 12572
14:21 24043
14:32 32821

to collect blocks

:l Tucker.Test
env <- mainLoop btc_testnet3 tucker_default_conf
idle = envDumpIdleBlock env >>= (return . length)
fetched = envDumpReceivedBlock env >>= (return . length)
height = envCurrentTreeHeight env
envSpreadSimpleAction env (NormalAction fetchBlock) 1

sync <- forkIO $ blockSyncLoop env

whileM (pure True) $ do; envSpreadSimpleAction env (NormalAction fetchBlock) 1; delay $ 20 * 1000 * 1000

then, to get status:

number of idle blocks:
    envDumpIdleBlock env >>= (return . length)

tree height:
    envCurrentTreeHeight env

number of total received blocks:
    envDumpReceivedBlock env >>= (return . length)

envHasFetchedBlock env (read "0000000005618907cb6a234fd732fd16cb230cfe726137e281aa467165029ffb")

(getA $ block_tree env) >>= (getTreeChunk . (!!0) . chunks)
(getA $ block_tree env) >>= (flushTreeChunk . (!!0) . chunks)
(getA $ block_tree env) >>= flushTreeCached

(getA $ node_list env) >>= mapM nodeNetDelay

-}

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
