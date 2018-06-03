{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Test.Util where

import qualified Data.ByteString as BSR

import Test.HUnit
import Test.Common

test_db_path = "tucker-testdb"

removeTestDB = do
    exist <- doesDirectoryExist test_db_path
    if exist then
        removeDirectoryRecursive test_db_path
    else
        return ()

-- [(original number, encoded as vint, encoded as vword)]
encode_map_be :: [(Integer, ByteString, ByteString)]
encode_map_be = [
        (0, hex2bs "", hex2bs ""),
        (-1, hex2bs "ff", hex2bs "ff"),
        (-256, hex2bs "ff00", hex2bs "ff00"),
        (65280, hex2bs "00ff00", hex2bs "ff00")
    ]

genCoinbase :: Satoshi -> TxPayload
genCoinbase value =
    updateTx TxPayload {
        txid = 0,
        wtxid = 0,

        version = 0,
        
        tx_in = [ TxInput (OutPoint 0 (-1)) BSR.empty (-1) ],
        tx_out = [], -- no output
        tx_witness = [],

        lock_time = 0
    }

simpleBlock :: Hash256 -> Satoshi -> Block
simpleBlock prev_hash cb_val =
    -- nonce <- randomIO
    -- timestamp <- unixTimestamp

    let coinbase = genCoinbase cb_val
        tmp = Block {
                block_hash = nullHash256,

                vers = 0,
                prev_hash = prev_hash,
                merkle_root = txid coinbase,

                btimestamp = 0,

                hash_target = -1,
        
                nonce = 0, -- nonce,

                txns = FullList [ coinbase ],

                enc_cache = Nothing
            }

    in tmp { block_hash = hashBlock tmp }

genChain' :: Hash256 -> Int -> [Block] -> [Block]
genChain' prev_hash 0 cur_lst = cur_lst
genChain' prev_hash n cur_lst =
    genChain' hash (n - 1) (cur_lst ++ [ nblock ])
    where
        nblock = simpleBlock prev_hash 50
        hash = block_hash nblock

genChain prev_hash n = genChain' prev_hash n []

assertEitherRight :: Either TCKRError t -> IO t
assertEitherRight (Right v) = return v
assertEitherRight (Left err) = assertFailure $ show err

assertEitherLeft :: Either TCKRError t -> IO ()
assertEitherLeft (Right _) = assertFailure "expecting error"
assertEitherLeft (Left err) = return ()

hex2block :: String -> Block
hex2block = decodeFailLE . hex2bs

hex2tx :: String -> TxPayload
hex2tx = decodeFailLE . hex2bs

-- run script on an empty stack with a dummy tx
runScript :: [ScriptOp] -> IO [StackItem]
runScript ops = do
    let tx = hex2tx "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0704ffff001d0134ffffffff0100f2052a0100000043410411db93e1dcdb8a016b49840f8c53bc1eb68a382e97b1482ecad7b148a6909a5cb2e0eaddfb84ccf9744464f82e160bfa9b8b64f9d4c03f999b8643f656b412a3ac00000000"
        state = initState def undefined tx 0

    eval_stack <$> assertEitherRight (execEval state ops)
