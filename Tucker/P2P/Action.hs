{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.P2P.Action where

import Data.List
import qualified Data.Set as SET
import qualified Data.ByteString as BSR

import Control.Monad
import qualified Control.Concurrent.Lock as LK

import System.Random

import Tucker.Msg
import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Atom
import Tucker.Error
import Tucker.Transport

import Tucker.P2P.Msg
import Tucker.P2P.Node
import Tucker.P2P.Util

import Tucker.Chain.Object

-- data CoroAction msg = CoroAction { doAction :: MainLoopEnv -> Node -> msg -> IO [RouterAction] }

-- instance Funtor CoroAction where
--     f `fmap` c = 

-- 172.104.120.91

recvM :: MsgPayload t => [RouterAction] -> Command -> (t -> IO [RouterAction]) -> IO [RouterAction]
recvM r_act cmd proc =
    return $ r_act ++ [ UpdateMe $ NormalAction handle ]
    where
        handle env node LackData = return []
        handle env node (MsgHead {
            command = command,
            payload = payload
        }) = do
            if command == cmd then
                decodePayload env node payload (do
                    nodeMsg env node $ "decode failed on command " ++ (show command)
                    return []) proc
            else do
                -- nodeMsg env node $ "command not match, skipping(" ++ (show command) ++ ")"
                return []

-- data BlockFetchTask =
--     BlockFetchTask {
--         fetch_block :: [(Hash256, Maybe BlockPayload)]
--     }

-- instance Monoid BlockFetchTask where
--     mempty = BlockFetchTask []
--     mappend (BlockFetchTask t1) (BlockFetchTask t2)
--         = BlockFetchTask $ t1 ++ t2

-- instance NodeTask BlockFetchTask

-- splitTask :: Int -> [InvVector] -> [BlockFetchTask]
-- splitTask maxt invs =
--     [
--         BlockFetchTask {
--             fetch_block = map (\h -> (h, Nothing)) $ take maxt $ drop (i * maxt) hashes
--         }
--         | i <- [ 0 .. taskn - 1 ]
--     ]
--     where
--         taskn = ceiling $ (fromIntegral $ length invs) / fromIntegral maxt
--         hashes = map invToHash256 invs

-- taskToInv :: BlockFetchTask -> [InvVector]
-- taskToInv (BlockFetchTask { fetch_block = fetch_block })
--     = map (InvVector INV_TYPE_BLOCK . fst) fetch_block

-- -- spread out fetch tasks to different nodes
-- -- return rest tasks
-- spreadFetchTask :: MainLoopEnv -> [BlockFetchTask] -> IO ()
-- spreadFetchTask env tasks = do
--     dup_node     <- getEnvConf env tckr_fetch_dup_node
--     max_dup_task <- getEnvConf env tckr_fetch_dup_max_task
--     len          <- envAllNode env >>= (return . length)

--     envMsg env $ "!!! task: " ++ show (length tasks)

--     final_tasks  <-
--         if length tasks < max_dup_task then do
--             envMsg env $ "!!! duplicate tasks to fetch"
--             return $ dupFill tasks dup_node len
--         else
--             return tasks

--     envSpreadAction env ((:[]) . NormalAction . doFetchBlock) final_tasks

-- -- hashes of the latest blocks
-- latestBlock :: MainLoopEnv -> IO [Hash256]
-- latestBlock env = do
--     tree <- getA $ block_tree env
--     treeCachedLatestHash tree

-- -- find out the inventory
-- fetchBlock :: MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
-- fetchBlock env node msg = do
--     let net = btc_network env
--         trans = conn_trans node

--     nodeMsg env node $ "start fetching blocks"

--     fetched <- getA (fetched_block env)

--     latest <- latestBlock env
--     getblocks <- encodeMsg net BTC_CMD_GETBLOCKS $ encodeGetblocksPayload latest nullHash256
--     timeoutRetryS (timeout_s env) $ tSend trans getblocks

--     recvM [] BTC_CMD_INV $ \(InvPayload {
--         inv_vect = inv_vect
--     }) -> do
--         nodeMsg env node $ "inv received with " ++ (show $ length inv_vect) ++ " item(s)" -- ++ show inv_vect

--         maxt <- getEnvConf env tckr_max_block_task

--         hashes <- envFilterFetchedBlock env (map invToHash256 inv_vect)
--         let new_inv_vect = map (InvVector INV_TYPE_BLOCK) hashes
--             tasks = splitTask maxt new_inv_vect

--         rest <- spreadFetchTask env tasks

--         return [ StopProp, DumpMe ]

-- doFetchBlock :: BlockFetchTask -> MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
-- doFetchBlock task env node msg = do
--     -- forM (fetch_block task) $ \(hash, Nothing) -> do
--     --     getdata <- encodeMsg net BTC_CMD_GETBLOCKS $ encodeGetblocksPayload [] nullHash256

--     let net = btc_network env
--         trans = conn_trans node

--     getdata <- encodeMsg net BTC_CMD_GETDATA $ encodeGetdataPayload (taskToInv task)
--     timeoutRetryS (timeout_s env) $ tSend trans getdata

--     nodeMsg env node "getdata sent"

--     return [ DumpMe ]

pingDelay :: MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
pingDelay env node msg = do
    let conf = global_conf env
        trans = conn_trans node

    nonce <- randomIO
    ping <- encodeMsg conf BTC_CMD_PING $ pure $ encodeLE (PingPongPayload nonce)

    -- setA (ping_delay node) maxBound -- set a maximum in case the node doesn't reply
    start <- msCPUTime
    timeoutRetryS (timeout_s env) $ tSend trans ping

    recvM [] BTC_CMD_PONG $ \(PingPongPayload back_nonce) -> do
        if back_nonce == nonce then do
            end <- msCPUTime
            setA (ping_delay node) (end - start)
            return [ StopProp, DumpMe ]
        else
            return [] -- skip