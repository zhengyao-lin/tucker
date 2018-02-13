{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.P2P.Action where

import Data.List
import qualified Data.Map as MAP
import qualified Data.Set as SET
import qualified Data.ByteString as BSR

import Control.Monad
import Control.Exception
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

recv' :: MainLoopEnv -> Node
      -> [RouterAction] -> Command -> (ByteString -> IO [RouterAction]) -> IO [RouterAction]
recv' env node r_act cmd proc =
    return $ r_act ++ [ UpdateMe $ NormalAction handle ]
    where
        handle env node LackData = return []
        handle env node (MsgHead {
            command = command,
            payload = payload
        }) = do
            if command == cmd then
                proc payload
            else do
                -- nodeMsg env node $ "command not match, skipping(" ++ (show command) ++ ")"
                return []

recv :: MsgPayload t
     => MainLoopEnv -> Node
     -> [RouterAction] -> Command -> (t -> IO [RouterAction]) -> IO [RouterAction]
recv env node r_act cmd proc =
    recv' env node r_act cmd $ \payload ->
        decodePayload env node payload fail proc

    where
        fail = do
            nodeMsg env node $ "decode failed on command " ++ show cmd
            return []

-- keep receiving a certain type of message until the proc returns DumpMe
keepRecv' :: MainLoopEnv -> Node
          -> Command -> (ByteString -> IO [RouterAction]) -> IO [RouterAction]
keepRecv' env node cmd proc = recv' env node [] cmd $ \msg ->
    (++) <$> proc msg <*> keepRecv' env node cmd proc

keepRecv :: MsgPayload t
         => MainLoopEnv -> Node
         -> Command -> (t -> IO [RouterAction]) -> IO [RouterAction]
keepRecv env node cmd proc = recv env node [] cmd $ \msg ->
    (++) <$> proc msg <*> keepRecv env node cmd proc

pingDelay :: MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
pingDelay env node msg = do
    let conf = global_conf env
        trans = conn_trans node

    nonce <- randomIO
    ping <- encodeMsg conf BTC_CMD_PING $ pure $ encodeLE (PingPongPayload nonce)

    -- setA (ping_delay node) maxBound -- set a maximum in case the node doesn't reply
    start <- msCPUTime
    timeoutRetryS (timeout_s env) $ tSend trans ping

    recv env node [] BTC_CMD_PONG $ \(PingPongPayload back_nonce) -> do
        if back_nonce == nonce then do
            end <- msCPUTime
            setA (ping_delay node) (end - start)
            return [ StopProp, DumpMe ]
        else
            return [] -- skip

type BlockFetchDoneProc = Node -> BlockFetchTask -> [(Hash256, ByteString)] -> IO ()

data BlockFetchTask =
    BlockFetchTask {
        fetch_block :: [(Hash256, ByteString)],
        done_proc   :: Maybe BlockFetchDoneProc
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
                proc node task (fetch_block task)

-- fetch & sync the block inventory
syncChain :: Int -> Atom [[Hash256]] -> IO ()
          -> MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
syncChain n hash_pool callback env node msg = do
    let conf = global_conf env
        trans = conn_trans node

    nodeMsg env node $ "start fetching inventory"

    latest <- getA (block_chain env) >>= latestBlocks (tckr_known_inv_count conf)

    nodeMsg env node $ "latest known blocks" ++ show (map block_hash latest)

    getblocks <- encodeMsg conf BTC_CMD_GETBLOCKS $
                 encodeGetblocksPayload conf (map block_hash latest) nullHash256

    timeoutRetryS (timeout_s env) $ tSend trans getblocks

    recv env node [] BTC_CMD_INV $ \(InvPayload {
        inv_vect = inv_vect
    }) -> do
        nodeMsg env node $ "inv received with " ++ show (length inv_vect) ++ " item(s)" -- ++ show inv_vect

        let hashes = map invToHash256 inv_vect

        -- push to the common hash pool
        cur_hash_pool <- appA (hashes:) hash_pool

        if length cur_hash_pool == n then do
            -- all n inventories fetched

            -- compare inventories
            let final_hashes = listUnion cur_hash_pool

            nodeMsg env node $ "fetching final inventory of " ++ show (length final_hashes) ++ " item(s)"

            scheduleFetch env node final_hashes callback
        else
            return ()

        return [ StopProp, DumpMe ]

buildFetchTasks :: Int -> [Hash256] -> BlockFetchDoneProc -> [BlockFetchTask]
buildFetchTasks maxt hashes done_proc =
    map (\task -> BlockFetchTask {
        fetch_block = map (flip (,) (throw $ TCKRError "unfetched block")) task,
        done_proc = Just done_proc
    }) $ splitList maxt hashes

fetchTaskToHashes :: BlockFetchTask -> [Hash256]
fetchTaskToHashes task =
    map fst (fetch_block task)

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
    assign_lock <- LK.new

    let doFetch hashes = do
            blacklist <- getA blacklist

            envSpreadActionExcept
                (SET.toList blacklist) env
                ((:[]) . NormalAction . fetchBlock) $
                buildFetchTasks (tckr_max_block_task conf) hashes done_proc
        
            -- nodeMsg env node $ "assignment: " ++ show assign

            -- return assign

        done_proc node task results = do
            -- delete current active node from the blacklist
            appA (SET.delete node) blacklist

            -- remove the task
            LK.acquire assign_lock
            orig_assign <- getA assign_var
            new_assign <- appA (filter ((/= task) . snd)) assign_var
            -- nodeMsg env node $ "assign after removal " ++ show assign
            LK.release assign_lock

            -- fill in the block 
            
            if length orig_assign /= length new_assign then do
                forM results $ \(hash, payload) -> do
                    -- decode now
                    let block = decodeFailLE payload
                        mvar = lookup hash tarray

                    -- here if the hash already exists, no decoding will be needed
                    case mvar of
                        Just var -> setA var (Just block)
                        Nothing ->
                            nodeMsg env node $ "irrelavent block " ++ show block

                -- nodeMsg env node $ "task decoding finished"
                return ()
            else do
                -- task already finished
                nodeMsg env node $ "duplicated assignment"
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
        delay loop_delay_us -- $ loop_delay_us + time `div` 5 * loop_delay_us

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

            forM tarray $ \(_, block) -> setA block Nothing

            return True
        else do
            -- refetch
            nodeMsg env node "still fetching"

            LK.acquire assign_lock
            assign <- getA assign_var

            prog <- forM assign $
                getA . cur_progress . fst

            nodeMsg env node $ "node progresses: " ++ show prog

            -- find non-responsive nodes
            (slow, ok) <- flip sepWhenM assign $ \(n, _) -> do
                time <- getA (last_seen n)
                return $ time < start_time

            let
                slow_nodes = unique $ map fst slow
                ok_nodes = unique $ map fst ok

            -- decrease/increase blacklist count
            mapM nodeBlacklistDec ok_nodes
            mapM nodeBlacklistInc slow_nodes

            nodeMsg env node $ "removing slow nodes: " ++ show slow

            if not $ null slow then do
                -- add them to the blacklist
                appA (`SET.union` SET.fromList slow_nodes) blacklist

                -- refetch on certain slow nodes
                let retry_hashes = fetchTaskToHashes (mconcat (map snd slow))

                nodeMsg env node $ "refetching on nodes " ++ show retry_hashes

                assign <- doFetch retry_hashes

                if null assign then do
                    -- blacklist full
                    -- try again with empty blacklist
                    nodeMsg env node "blacklist full"

                    setA blacklist SET.empty
                    assign <- doFetch retry_hashes
                    setA assign_var (assign ++ ok)
                else
                    setA assign_var (assign ++ ok)

                nodeMsg env node $ "new assignment " ++ show (assign ++ ok)
            else
                return () -- no slow node

            LK.release assign_lock

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

    fetched_var <- newA MAP.empty

    keepRecv' env node BTC_CMD_BLOCK $ \payload -> do
        -- nodeMsg env node $ "received block " ++ show block

        -- only decode head to reduce decoding time
        let BlockHeader (Block {
            block_hash = hash
        }) = decodeFailLE payload

        if hash `elem` hashes then do
            fetched <- appA (MAP.insert hash payload) fetched_var

            if MAP.size fetched == length hashes then do
                -- all fetched
                nodeMsg env node "partial task finished"

                done env node (task {
                    fetch_block = MAP.toList fetched
                })

                return [ StopProp, DumpMe ]
            else
                return [ StopProp ]
        else
            return []
