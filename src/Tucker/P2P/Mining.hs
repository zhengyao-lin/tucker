{-# LANGUAGE ForeignFunctionInterface #-}

-- mining utils

module Tucker.P2P.Mining where

import Data.Word
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Concurrent

import Foreign.Ptr

import Debug.Trace

import Tucker.Msg
import Tucker.Enc
import Tucker.Util
import Tucker.Atom
import Tucker.Conf
import Tucker.Thread

import Tucker.P2P.Node
import Tucker.P2P.Action

type MinerState = Ptr ()

foreign import ccall "init_miner" c_init_miner :: Ptr Word8 -> Word64 -> Ptr Word8 -> Int -> IO MinerState
foreign import ccall "join_miner" c_join_miner :: MinerState -> IO Word32
foreign import ccall "free_miner" c_free_miner :: MinerState -> IO ()
foreign import ccall "kill_miner" c_kill_miner :: MinerState -> IO ()

initMiner :: Int -> Block -> IO MinerState
initMiner job block = do
    let fixed = blockFixedHeader block
        target = encodeLE (hash_target block)

    BA.withByteArray fixed $ \dat ->
        BA.withByteArray target $ \target ->
            c_init_miner dat (fi (BSR.length fixed)) target job

joinMiner :: Block -> MinerState -> IO Block
joinMiner block state = do
    nonce <- c_join_miner state
    return (updateBlockHashes block { nonce = fi nonce })

freeMiner :: MinerState -> IO ()
freeMiner = c_free_miner

killMiner :: MinerState -> IO ()
killMiner = c_kill_miner

miner :: MainLoopEnv -> IO ()
miner env =
    forever $ do
        tip <- envMainBranchTipHash env
        finished_var <- newA False
        state_var <- newA Nothing

        let msg = BS.pack (envConf env tckr_miner_msg)
            addr = envConf env tckr_miner_p2pkh_addr
            job = envConf env tckr_job_number

        template <- envNextBlock env msg addr

        let mine = do
                state <- initMiner job template

                setA state_var (Just state)
                
                final <- joinMiner template state

                envInfo env ("mined block: " ++ show final)

                -- broadcast!!
                envAddBlock env NullNode final

                inv <- encodeMsg (global_conf env) BTC_CMD_INV $
                       encodeInvPayload [InvVector INV_TYPE_BLOCK (block_hash final)]

                envBroadcastAction env (sendMsgA inv)

        tid <- envForkFinally env THREAD_BASE mine $ \res -> do
            case res of
                Right _ -> setA finished_var True
                Left err ->
                    envInfo env ("miner exiting: " ++ show err)

        envInfo env ("mining started after block " ++ show tip)

        -- branch tip changed
        waitUntilIO $ do
            cur_tip <- envMainBranchTipHash env
            finished <- getA finished_var
            return (cur_tip /= tip || finished)

        Just state <- getA state_var

        envInfo env "killing miner"

        killMiner state
        killThread tid
        freeMiner state
