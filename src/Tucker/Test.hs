{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.Test where

import Data.Hex
import qualified Data.ByteString as BSR

import Debug.Trace

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString

import System.IO
import System.Mem
import System.Random
import System.Directory

import Test.HUnit

import Control.Monad
import Control.Monad.Loops
import Control.Monad.Morph
import Control.Monad.Trans.Resource

import Control.Concurrent
import Control.Concurrent.Thread.Delay

import Control.Exception

import Tucker.DB
import Tucker.Enc
import Tucker.Msg
import Tucker.Conf
import Tucker.Atom
import Tucker.Auth
import Tucker.Util
import Tucker.Error

import Tucker.P2P.Init
import Tucker.P2P.Util
import Tucker.P2P.Node
import Tucker.P2P.Action

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

        hash_target = -1,
 
        nonce = nonce,

        txns = FullList [],

        enc_cache = Nothing
    }

    return $ tmp { block_hash = hashBlock tmp }

assertEitherRight :: Either TCKRError t -> IO t
assertEitherRight (Right v) = return v
assertEitherRight (Left err) = assertFailure $ show err

assertEitherLeft :: Either TCKRError t -> IO ()
assertEitherLeft (Right _) = assertFailure "expecting error"
assertEitherLeft (Left err) = return ()

withChain :: TCKRConf -> (Chain -> IO a) -> IO a
withChain conf proc = runResourceT $ do
    chain <- initChain conf
    lift $ proc chain

hex2block :: String -> Block
hex2block = decodeFailLE . hex2bs

hex2tx :: String -> TxPayload
hex2tx = decodeFailLE . hex2bs

blockChainTest = TestCase $ do
    def_conf <- tucker_default_conf_mainnet

    let test_db_path = "tucker-testdb"
        conf = def_conf {
            tckr_db_path = test_db_path
            -- tckr_max_tree_insert_depth = 10
        }

        blocks =
            map hex2block [
                "010000006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000982051fd1e4ba744bbbe680e1fee14677ba1a3c3540bf7b1cdb606e857233e0e61bc6649ffff001d01e362990101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d0104ffffffff0100f2052a0100000043410496b538e853519c726a2c91e61ec11600ae1390813a627c66fb8be7947be63c52da7589379515d4e0a604f8141781e62294721166bf621e73a82cbf2342c858eeac00000000",
                "010000004860eb18bf1b1620e37e9490fc8a427514416fd75159ab86688e9a8300000000d5fdcc541e25de1c7a5addedf24858b8bb665c9f36ef744ee42c316022c90f9bb0bc6649ffff001d08d2bd610101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d010bffffffff0100f2052a010000004341047211a824f55b505228e4c3d5194c1fcfaa15a456abdf37f9b9d97a4040afc073dee6c89064984f03385237d92167c13e236446b417ab79a0fcae412ae3316b77ac00000000",
                "01000000bddd99ccfda39da1b108ce1a5d70038d0a967bacb68b6b63065f626a0000000044f672226090d85db9a9f2fbfe5f0f9609b387af7be5b7fbb7a1767c831c9e995dbe6649ffff001d05e0ed6d0101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d010effffffff0100f2052a0100000043410494b9d3e76c5b1629ecf97fff95d7a4bbdac87cc26099ada28066c6ff1eb9191223cd897194a08d0c2726c5747f1db49e8cf90e75dc3e3550ae9b30086f3cd5aaac00000000",
                "010000004944469562ae1c2c74d9a535e00b6f3e40ffbad4f2fda3895501b582000000007a06ea98cd40ba2e3288262b28638cec5337c1456aaf5eedc8e9e5a20f062bdf8cc16649ffff001d2bfee0a90101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d011affffffff0100f2052a01000000434104184f32b212815c6e522e66686324030ff7e5bf08efb21f8b00614fb7690e19131dd31304c54f37baa40db231c918106bb9fd43373e37ae31a0befc6ecaefb867ac00000000",
                "0100000085144a84488ea88d221c8bd6c059da090e88f8a2c99690ee55dbba4e00000000e11c48fecdd9e72510ca84f023370c9a38bf91ac5cae88019bee94d24528526344c36649ffff001d1d03e4770101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d0120ffffffff0100f2052a0100000043410456579536d150fbce94ee62b47db2ca43af0a730a0467ba55c79e2a7ec9ce4ad297e35cdbb8e42a4643a60eef7c9abee2f5822f86b1da242d9c2301c431facfd8ac00000000",
                "01000000fc33f596f822a0a1951ffdbf2a897b095636ad871707bf5d3162729b00000000379dfb96a5ea8c81700ea4ac6b97ae9a9312b2d4301a29580e924ee6761a2520adc46649ffff001d189c4c970101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d0123ffffffff0100f2052a0100000043410408ce279174b34c077c7b2043e3f3d45a588b85ef4ca466740f848ead7fb498f0a795c982552fdfa41616a7c0333a269d62108588e260fd5a48ac8e4dbf49e2bcac00000000",
                "010000008d778fdc15a2d3fb76b7122a3b5582bea4f21f5a0c693537e7a03130000000003f674005103b42f984169c7d008370967e91920a6a5d64fd51282f75bc73a68af1c66649ffff001d39a59c860101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d012bffffffff0100f2052a01000000434104a59e64c774923d003fae7491b2a7f75d6b7aa3f35606a8ff1cf06cd3317d16a41aa16928b1df1f631f31f28c7da35d4edad3603adb2338c4d4dd268f31530555ac00000000"
            ]
        -- "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d010bffffffff0100f2052a010000004341047211a824f55b505228e4c3d5194c1fcfaa15a456abdf37f9b9d97a4040afc073dee6c89064984f03385237d92167c13e236446b417ab79a0fcae412ae3316b77ac00000000"

    exist <- doesDirectoryExist test_db_path
    if exist then
        removeDirectoryRecursive test_db_path
    else return ()

    -- putStrLn ""
    withChain conf $ \chain -> do
        chain <- addBlocks add_block_common_proc chain blocks

        -- assertEqual "wrong resulting chain"
        --     (zip [1..] blocks)
        --     (tail $ branchToBlockList $ head $ edge_branches chain)

        -- putStrLn $ show $ map branchToBlockList $ edge_branches chain
        -- putStrLn $ show $ branchToBlockList <$> buffer_chain chain

        return ()

add_block_common_proc b r =
    case r of
        Left err ->
            putStrLn $ "failed to add block: " ++ show b ++ show err
        Right _ ->
            putStrLn $ "block added: " ++ show b

blockTest = TestList [
        TestLabel "block chain basic" blockChainTest
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

txCase1 = do
    -- tx0 0437cd7f8525ceed2324359c2d0ba26006d92d856a9c20fa0241106ee5a597c9 at block #9
    -- tx1 f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16 at block #170
    let tx0 = hex2tx "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d0134ffffffff0100f2052a0100000043410411db93e1dcdb8a016b49840f8c53bc1eb68a382e97b1482ecad7b148a6909a5cb2e0eaddfb84ccf9744464f82e160bfa9b8b64f9d4c03f999b8643f656b412a3ac00000000"
        tx1 = hex2tx "0100000001c997a5e56e104102fa209c6a852dd90660a20b2d9c352423edce25857fcd3704000000004847304402204e45e16932b8af514961a1d3a1a25fdf3f4f7732e9d624c6c61548ab5fb8cd410220181522ec8eca07de4860a4acdd12909d831cc56cbbac4622082221a8768d1d0901ffffffff0200ca9a3b00000000434104ae1a62fe09c5f51b13905f07f06b99a2f7159b2225f374cd378d71302fa28414e7aab37397f554a7df5f142c21c1b7303b8a0626f1baded5c72a704f7e6cd84cac00286bee0000000043410411db93e1dcdb8a016b49840f8c53bc1eb68a382e97b1482ecad7b148a6909a5cb2e0eaddfb84ccf9744464f82e160bfa9b8b64f9d4c03f999b8643f656b412a3ac00000000"
        -- decode the script of the first outpoint
        prev_out1 = tx_out tx0 !! 0
        pk_sc1 = decodeFailLE $ pk_script prev_out1
        sig_sc1 = decodeFailLE $ sig_script $ (tx_in tx1 !! 0)

    -- print (sig_sc1 ++ pk_sc1)
    -- print $ execEval (initState tx1 0 prev_out1) (sig_sc1 ++ pk_sc1)

    assertEqual "wrong checksig result 1" (Right True) (runEval (initState tx1 0 prev_out1) (sig_sc1 ++ pk_sc1))

txCase2 = do
    -- tx0 4600bf5f3d49660a546c1b9acd3e6b85479ab227115f741cf12e50506ab24b54 at block #119594
    -- tx1 e3d0425ab346dd5b76f44c222a4bb5d16640a4247050ef82462ab17e229c83b4 at block #123456
    let tx0 = hex2tx "01000000017870c6dbf8522cf9c2b3ee1c8c0e3215b1c613e6ae724405459f96bb8f3bd0ae0000000049483045022043df36465eb8854649a62b36e8d9a402bce012267ac8f4857ef3b5ca2f3396b7022100ae120cbba78942b82304c98efecbabc8745be7b8f6358f089344a2c5efe942dc01ffffffff0200127a00000000001976a914a9730a07ea5626f14f7fa304c1309602b10a055388ac00f2052a010000001976a9144d63a4b3b4ae384c44ff5d112a9b87c4c49ff36f88ac00000000"
        tx1 = hex2tx "0100000001544bb26a50502ef11c745f1127b29a47856b3ecd9a1b6c540a66493d5fbf0046010000008b483045022100c9e35aa55af5ac98cb67c4db7cf3d3f128753c4698f5d25ca0cdc3decd0c46be02204d6dfe89bd3fe88a32d47a44c0ab3ab60d87b27b90106f1b2f9f67c9c60cc80c01410449b8d933f97a8c4fe6ce962ee2abff8a81d8cfc5e0870a50cea76c50d04addf2df09331c4a47cdc3bc27a628e766ef5d01f28ee147ed21723b5ff3a62ed8da3effffffff024094ef03010000001976a9146c8de651f8b92f87ff43fb9732babec784bdb6f588acc05d1626000000001976a9144f006767feebf6438aaf51ef86ae4286a1c571b988ac00000000"
        -- decode the script of the first outpoint
        prev_out1 = tx_out tx0 !! 1
        pk_sc1 = decodeFailLE $ pk_script prev_out1
        sig_sc1 = decodeFailLE $ sig_script $ (tx_in tx1 !! 0)

    assertEqual "wrong checksig result 2" (Right True) (runEval (initState tx1 0 prev_out1) (sig_sc1 ++ pk_sc1))

scriptTest = TestCase $ do
    let sc1 = [ OP_NOP ]
        sc2 = [ OP_IF True [] [] ]
        sc3 = [
                OP_IF True [
                    OP_IF True [
                        OP_IF True [
                            OP_NOP
                        ] sc2
                    ] sc2
                ] sc2
            ]

        sc4 = [ OP_PUSHDATA $ BSR.pack [ 0x00 ] ]

    assertEqual "wrong decode result 1" sc1 (decodeFailLE (encodeLE sc1))
    assertEqual "wrong encode result 2" (BSR.pack [ 0x63, 0x68 ]) (encodeLE sc2)
    assertEqual "wrong decode result 3" sc3 (decodeFailLE (encodeLE sc3))
    assertEqual "wrong encode result 4" (BSR.pack [ 0x01, 0x00 ]) (encodeLE sc4)

    txCase1
    txCase2

msgTest = TestList [
        TestLabel "hash256 basic" hash256Test,
        TestLabel "script test" scriptTest
    ]

allTest = TestList [
        blockTest, msgTest
    ]

tmp :: IO ()
tmp = do
    bs <- BSR.readFile "realbigdata"
    def_conf <- tucker_default_conf_testnet3

    let conf = def_conf

    case doDecode (listD 500 decoder) LittleEndian bs of
        (Left _, _) -> error "no!!!"
        (Right blocks, _) -> do
            traceM "decode finished"

            withChain conf $ \chain -> do
                addBlocks add_block_common_proc chain blocks

            putStrLn "finished!!!"

    return ()

{-

15:38 000000002aa037f4ebfad352c1d7edb421d1ac2148d287a22cb1995ed9337bf6 12339
15:48 000000000451e1119f794c65c8b6610a2eecbfcab288b8cb2ea19d3894564719 30839
15:59 0000000000e56ea246fb0954587330e1e1bd97b1fb91e5fa323812c912d91478 54339
16:08 00000000034b3093e8c649685ed3b1a3ed14c760c4cc995180e4815c037de78e 66839
16:22 000000000885c4005753db23bac7e77a1060915dcea97b764e24872eb1324551 77839
16:33 00000000b659b6074e9743f6c2490c8009dfc8346436bc05e5a628102d93da40 80839

13:50 4958
13:55 9319
14:01 12545 stop

14:09 12572
14:21 24043
14:32 32821

to collect blocks

:l Tucker.Test
env <- tucker_default_conf_testnet3 >>= mainLoop
sync env 1

envSpreadSimpleAction env (NormalAction syncInv) 1



heights of each branch
branchHeights <$> getA (block_chain env)






idle = envDumpIdleBlock env >>= (return . length)
fetched = envDumpReceivedBlock env >>= (return . length)
height = envCurrentTreeHeight env

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