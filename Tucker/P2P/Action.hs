{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.P2P.Action where

import Data.List
import qualified Data.Set as SET
import qualified Data.ByteString as BSR

import Control.Monad
import qualified Control.Concurrent.Lock as LK

import Tucker.Std
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

-- data CoroAction msg = CoroAction { doAction :: MainLoopEnv -> BTCNode -> msg -> IO [RouterAction] }

-- instance Funtor CoroAction where
--     f `fmap` c = 

recvM :: MsgPayload t => Command -> (t -> IO [RouterAction]) -> IO [RouterAction]
recvM cmd proc =
    return [ StopProp, UpdateMe $ NormalAction handle ]
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
                nodeMsg env node $ "command not match, skipping(" ++ (show command) ++ ")"
                return []

data BlockFetchTask =
    BlockFetchTask {
        fetch_block :: [(Hash256, Maybe BlockPayload)]
    }

splitTask :: Int -> [InvVector] -> [BlockFetchTask]
splitTask maxt invs =
    [
        BlockFetchTask {
            fetch_block = map (\h -> (h, Nothing)) $ take maxt hashes
        }
        | _ <- [ 1 .. taskn ]
    ]
    where
        taskn = ceiling $ (fromIntegral $ length invs) / fromIntegral maxt
        hashes = map invToHash256 invs

taskToInv :: BlockFetchTask -> [InvVector]
taskToInv (BlockFetchTask { fetch_block = fetch_block })
    = map (InvVector INV_TYPE_BLOCK . fst) fetch_block

-- spread out fetch tasks to different nodes
-- return rest tasks
spreadFetchTasks :: MainLoopEnv -> [BlockFetchTask] -> IO [BlockFetchTask]
spreadFetchTasks env tasks = do
    nodes <- getA (node_list env)
    alive_nodes <- filterM (getA . alive) nodes

    let rest = length tasks - length alive_nodes

    -- no enough nodes
    forM (zip alive_nodes tasks) $ \(node, task) -> do
        appA ((NormalAction $ doFetchBlock task):) (new_action node)

    return $ drop (length tasks - rest) tasks

addNewBlock :: MainLoopEnv -> BTCNode -> BlockPayload -> IO [RouterAction]
addNewBlock env node payload@(BlockPayload {
    header = header,
    txns = txns
}) = do
    let hash = hashBlockHeader header
        bph = BlockPayloadHashed hash payload

    -- 1. try to insert to the tree
    -- 2. if cannot, push it to the idle block

    fetched <- getA (fetched_block env)

    case SET.lookupIndex hash fetched of
        Just _ -> do
            nodeMsg env node $ "block received again: " ++ (show hash)
            return () -- already fetched, do nothing

        Nothing -> do
            nodeMsg env node $ "block received: " ++ (show hash) ++ " " ++ (show payload)

            -- add to the set
            appA (SET.insert hash) (fetched_block env)

            -- lock tree
            LK.acquire (tree_lock env)

            tree <- getA (block_tree env)

            case insertToTree tree bph of
                Left err -> do
                    nodeMsg env node $ "failed to insert new block " ++ show hash ++ ": " ++ show err
                    appA (SET.insert bph) (idle_block env)

                Right new_tree -> do
                    nodeMsg env node $ "!!!!!!!!! block " ++ show hash ++ " added to tree, new height: " ++ show (treeHeight new_tree)
                    setA (block_tree env) new_tree

            LK.release (tree_lock env)

    return []


-- find out the inventory
initFetchBlock :: MainLoopEnv -> BTCNode -> MsgHead -> IO [RouterAction]
initFetchBlock env node msg = do
    let net = btc_network env
        trans = conn_trans node

    fetched <- getA (fetched_block env)

    let invs = SET.toList fetched
    getblocks <- encodeMsg net BTC_CMD_GETBLOCKS $ encodeGetblocksPayload invs nullHash256
    timeoutRetryS (timeout_s env) $ tSend trans getblocks

    recvM BTC_CMD_INV $ \(InvPayload {
        inv_vect = inv_vect
    }) -> do
        nodeMsg env node $ "inv received with " ++ (show $ length inv_vect) ++ " item(s)" -- ++ show inv_vect

        -- getdata
        getdata <- encodeMsg net BTC_CMD_GETDATA $ encodeGetdataPayload inv_vect
        timeoutRetryS (timeout_s env) $ tSend trans getdata

        -- request sent, leave the rest to addNewBlock
        return [ DumpMe ]

        -- recvM BTC_CMD_BLOCK $ \payload@(BlockPayload {
        --     header = header,
        --     txns = txns
        -- }) -> addBlock 

            -- r_check_list <- getA check_list

            -- case findIndex (== (hash, Nothing)) r_check_list of
            --     Nothing -> return [] -- pass along
            --     Just i -> do
            --         appA (replace i (hash, Just payload)) check_list
            --         envAddFetchedBlock env hash

            --         -- save block

            --         return [ StopProp ]

        -- maxt <- getEnvConf env tckr_max_block_task
        -- let tasks = splitTask maxt inv_vect
        -- rest <- spreadFetchTasks env tasks
        
        -- if not $ null rest then
        --     nodeMsg env node "extra tasks not handled"
        -- else
        --     nodeMsg env node "all tasks broadcasted"

doFetchBlock :: BlockFetchTask -> MainLoopEnv -> BTCNode -> MsgHead -> IO [RouterAction]
doFetchBlock task env node msg = do
    -- forM (fetch_block task) $ \(hash, Nothing) -> do
    --     getdata <- encodeMsg net BTC_CMD_GETBLOCKS $ encodeGetblocksPayload [] nullHash256

    let net = btc_network env
        trans = conn_trans node

    -- getblocks <- encodeMsg net BTC_CMD_GETBLOCKS $ encodeGetblocksPayload [] nullHash256
    -- timeoutRetryS (timeout_s env) $ tSend trans getblocks

    -- nodeMsg env node "!!! waiting for inv"
    -- recvM BTC_CMD_INV $ \(InvPayload {}) -> do
    -- nodeMsg env node "!!! inv received"

    getdata <- encodeMsg net BTC_CMD_GETDATA $ encodeGetdataPayload (taskToInv task)
    timeoutRetryS (timeout_s env) $ tSend trans getdata

    -- check_list :: Atom [(Hash256, Maybe BlockPayload)]
    check_list <- newA $ fetch_block task

    recvM BTC_CMD_BLOCK $ \payload@(BlockPayload {
        header = header,
        txns = txns
    }) -> do
        nodeMsg env node "!!! block received"

        let hash = hashBlockHeader header
        
        nodeMsg env node $ "block received: " ++ (show hash) ++ " " ++ (show payload)

        r_check_list <- getA check_list

        case findIndex (== (hash, Nothing)) r_check_list of
            Nothing -> return [] -- pass along
            Just i -> do
                appA (replace i (hash, Just payload)) check_list
                envAddFetchedBlock env hash

                -- save block

                return [ StopProp ]

    -- nodeRecvOneMsg env node 
