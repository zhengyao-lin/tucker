{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.P2P.Action where

import Data.List
import qualified Data.Map as MAP
import qualified Data.Set as SET
import qualified Data.ByteString as BSR

import Control.Monad
import Control.Concurrent
import Control.Applicative
import Control.Monad.Loops
import Control.Concurrent.Thread.Delay
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

-- keep receiving a certain type of message until the proc returns DumpMe
keepRecvM :: MsgPayload t => Command -> (t -> IO [RouterAction]) -> IO [RouterAction]
keepRecvM cmd proc = recvM [] cmd $ \msg ->
    (++) <$> proc msg <*> keepRecvM cmd proc

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

data BlockFetchTask =
    BlockFetchTask {
        fetch_block :: [Either Hash256 Block],
        done_proc   :: Maybe (Node -> BlockFetchTask -> [Block] -> IO ())
    }

instance Show BlockFetchTask where
    show t =
        let hashes = fetchTaskToHashes t in
        "BlockFetchTask for " ++ show (length hashes) ++ " hash(es)" ++
            if null hashes then ""
            else " beginning with " ++ show (head hashes)

instance Eq BlockFetchTask where
    t1 == t2 = sort (fetchTaskToHashes t1) == sort (fetchTaskToHashes t2)
 
instance Monoid BlockFetchTask where
    mempty = BlockFetchTask [] Nothing
    mappend (BlockFetchTask t1 d1) (BlockFetchTask t2 d2)
        = BlockFetchTask (t1 ++ t2) (d1 <|> d2)

instance NodeTask BlockFetchTask where
    done env node task =
        case done_proc task of
            Nothing -> return ()
            Just proc ->
                proc node task $ map (either (error "unfetched block") id) (fetch_block task)

-- fetch & sync the block inventory
syncChain :: IO () -> MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
syncChain callback env node msg = do
    let conf = global_conf env
        trans = conn_trans node

    nodeMsg env node $ "!!!!!!!!!!! start fetching blocks"

    latest <- getA (block_chain env) >>= latestBlocks (tckr_known_inv_count conf)

    nodeMsg env node $ "!!!!!!!!!!! latest " ++ show (map block_hash latest)

    getblocks <- encodeMsg conf BTC_CMD_GETBLOCKS $
                 encodeGetblocksPayload conf (map block_hash latest) nullHash256

    timeoutRetryS (timeout_s env) $ tSend trans getblocks

    recvM [] BTC_CMD_INV $ \(InvPayload {
        inv_vect = inv_vect
    }) -> do
        nodeMsg env node $ "inv received with " ++ show (length inv_vect) ++ " item(s)" -- ++ show inv_vect

        chain <- getA (block_chain env)

        -- nodeMsg env node $ "branches " ++ show (map branchToBlockList (edge_branches chain))

        let hashes = map invToHash256 inv_vect

        scheduleFetch env node hashes callback

        return [ StopProp, DumpMe ]

buildFetchTasks :: Int -> [Hash256]
                -> (Node -> BlockFetchTask -> [Block] -> IO ())
                -> [BlockFetchTask]
buildFetchTasks maxt hashes done_proc =
    map (\task -> BlockFetchTask {
        fetch_block = map Left task,
        done_proc = Just done_proc
    }) $ splitList maxt hashes

fetchTaskToHashes :: BlockFetchTask -> [Hash256]
fetchTaskToHashes task =
    map (either id block_hash) (fetch_block task)

scheduleFetch :: MainLoopEnv -> Node -> [Hash256] -> IO () -> IO ()
scheduleFetch env node hashes callback = do
    let conf = global_conf env

    -- task array of (hash, atom maybe block) pair
    tarray <- forM hashes $ \hash -> do
        var <- newA Nothing :: IO (Atom (Maybe Block))
        return (hash, var)

    blacklist <- newA SET.empty :: IO (Atom (SET.Set Node))

    -- assignment list
    assign_var <- newA [] :: IO (Atom [(Node, BlockFetchTask)])

    let doFetch hashes = do
            blacklist <- getA blacklist

            envSpreadActionExcept
                (SET.toList blacklist) env
                ((:[]) . NormalAction . fetchBlock) $
                buildFetchTasks (tckr_max_block_task conf) hashes done_proc
        
            -- nodeMsg env node $ "assignment: " ++ show assign

            -- return assign

        done_proc node task blocks@(first:_) = do
            -- delete current active node from the blacklist
            appA (SET.delete node) blacklist

            -- assign <- getA assign_var
            -- nodeMsg env node $ "current assign " ++ show assign

            -- nodeMsg env node $ "removing: " ++ show (node, task)

            -- remove assignment
            appA (delete (node, task)) assign_var

            -- assign <- getA assign_var
            -- nodeMsg env node $ "after removing " ++ show assign

            -- fill in the block data
            forM blocks $ \block -> do
                let mvar = lookup (block_hash block) tarray

                case mvar of
                    Just var -> setA var (Just block)
                    Nothing -> do
                        nodeMsg env node $ "irrelavent block " ++ show block
                        return ()
            
            return ()

        remain = do
            blocks <- all_blocks
            return $ maybeCat $
                (flip map) (zip [0..] blocks) $ \(i, m) ->
                    if m == Nothing then Just (hashes !! i)
                    else Nothing

        loop_delay_s = tckr_block_fetch_timeout conf
        loop_delay_us = fi $ loop_delay_s * 1000 * 1000
        all_blocks = mapM (getA . snd) tarray

    -- assignment list
    doFetch hashes >>= setA assign_var

    forkIO $ forUntilM_ [1..] $ \time -> do
        -- send fetching request for unfetched blocks
        -- nodeMsg env node $ "received " ++ show mreceived

        start_time <- unixTimestamp
        delay $ loop_delay_us + time `div` 5 * loop_delay_us

        received <- maybeCat <$> all_blocks

        nodeMsg env node $
            "received " ++
            show (length received) ++ "/" ++ show (length hashes) ++
            " block(s) in total"

        if length received == length hashes then do
            nodeMsg env node "all fetching finished"
            blocks <- mapM (getA . snd) tarray
            envAddBlocks env node received

            -- TODO: stop writing js
            forkIO callback

            return True
        else do
            -- refetch
            nodeMsg env node "fetch timeout, start refetching"

            assign <- getA assign_var

            -- last_seens <- mapM (getA . last_seen . fst) assign
            -- nodeMsg env node $ show start_time ++ " last seen: " ++ show last_seens

            -- find non-responsive nodes
            (slow, ok) <- (flip sepWhenM) assign $ \(n, _) -> do
                time <- getA (last_seen n)
                return $ time < start_time

            nodeMsg env node $ "removing slow nodes: " ++ show slow

            -- add them to the blacklist
            appA (`SET.union` SET.fromList (map fst slow)) blacklist

            -- refetch on certain slow nodes
            let retry_hashes = fetchTaskToHashes (mconcat (map snd slow))

            nodeMsg env node $ "refetching on nodes " ++ show retry_hashes

            assign <- doFetch retry_hashes

            if null assign then
                setA blacklist SET.empty
            else
                setA assign_var (assign ++ ok)

            return False

    return ()

-- 00000000a967199a2fad0877433c93df785a8d8ce062e5f9b451cd1397bdbf62

fetchBlock :: BlockFetchTask -> MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
fetchBlock task env node _ = do
    let conf = global_conf env
        trans = conn_trans node

        hashes = fetchTaskToHashes task
        invs = map (InvVector INV_TYPE_BLOCK) hashes

    getdata <- encodeMsg conf BTC_CMD_GETDATA $ encodeGetdataPayload invs
    timeoutRetryS (timeout_s env) $ tSend trans getdata

    -- nodeMsg env node "getdata sent"

    fetched <- newA MAP.empty

    keepRecvM BTC_CMD_BLOCK $ \block@(Block {
        block_hash = hash
    }) -> do
        -- nodeMsg env node $ "received block " ++ show block

        if hash `elem` hashes then do
            appA (MAP.insert hash block) fetched
            fetched_pure <- getA fetched

            if MAP.size fetched_pure == length hashes then do
                -- all fetched
                nodeMsg env node "partial task finished"

                done env node (task {
                    fetch_block = map (Right . snd) $ MAP.toList fetched_pure
                })

                return [ StopProp, DumpMe ]
            else
                return [ StopProp ]
        else
            return []
