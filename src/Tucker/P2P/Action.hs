{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.P2P.Action where

import Data.List
import qualified Data.Map as MAP
import qualified Data.Set as SET
import qualified Data.Foldable as FD
import qualified Data.ByteString as BSR
import qualified Data.Set.Ordered as OSET

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Applicative
import Control.Monad.Loops
import Control.Concurrent.Thread.Delay

-- import System.IO
import System.Exit
import System.Random

import Tucker.Msg
import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Atom
import Tucker.Error
import Tucker.Transport
import qualified Tucker.Lock as LK

import Tucker.P2P.Msg
import Tucker.P2P.Node
import Tucker.P2P.Util
import Tucker.P2P.Scheduler

import Tucker.Storage.Chain

-- data CoroAction msg = CoroAction { doAction :: MainLoopEnv -> Node -> msg -> IO [RouterAction] }

-- instance Funtor CoroAction where
--     f `fmap` c = 

-- 172.104.120.91

recv' :: MainLoopEnv -> Node
      -> [RouterAction] -> Command -> (ByteString -> IO [RouterAction]) -> IO [RouterAction]
recv' env node r_act cmd proc =
    return $ r_act ++ [ UpdateMe $ NormalAction handle ]
    where
        handle env node (LackData _) = return []
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
    -- timeoutRetryS (timeout_s env) $
    
    tSend trans ping

    recv env node [] BTC_CMD_PONG $ \(PingPongPayload back_nonce) -> do
        if back_nonce == nonce then do
            end <- msCPUTime
            -- setA (ping_delay node) (end - start)

            nodeChangeTransState node
                (\ts -> ts { ping_delay = (end - start) })

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

-- sync with n nodes
sync :: MainLoopEnv -> Int -> IO ()
sync env n = do
    sync_inv_var <- newA OSET.empty
    finished_var <- newA 0 :: IO (Atom Int)

    let callback = do
            forkIO $ sync env n
            return ()

        taskDone sched node hashes = do
            removeNode sched node

            appA (OSET.|<> OSET.fromList hashes) sync_inv_var
            finished <- appA (+1) finished_var

            if finished == n then do
                -- no double execution when finished > n
                cancel sched

                sync_inv <- getA sync_inv_var

                if null sync_inv then
                    -- no sync inv
                    envMsg env "!!! all blocks syncronized"
                else do
                    let final_hashes = FD.toList sync_inv

                    envMsg env $
                        "fetching final inventory of " ++
                        show (length final_hashes) ++ " item(s)"

                    scheduleFetch env final_hashes callback
            else
                envMsg env "excess inv received"
                -- return ()

        action sched = NormalAction (syncChain (taskDone sched))
         
        reassign sched tasks blacklist =
            envSpreadActionExcept blacklist env (const [action sched]) tasks

    newScheduler env
        (tckr_sync_inv_timeout (global_conf env))
        (\sched -> reassign sched (replicate n NullTask) []) -- init assign
        reassign -- reassign
        $ \sched _ -> do -- fail
            envMsg env "failed to find active nodes to sync inventory"
            cancel sched
            return []

    return ()

-- fetch & sync the block inventory
syncChain :: (Node -> [Hash256] -> IO ())
          -> MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
syncChain callback env node msg = do
    let conf = global_conf env
        trans = conn_trans node

    nodeMsg env node $ "start fetching inventory"

    latest <- getA (block_chain env) >>= flip latestBlocks (tckr_known_inv_count conf)

    nodeMsg env node $ "latest known blocks" ++ show (map block_hash latest)

    getblocks <- encodeMsg conf BTC_CMD_GETBLOCKS $
                 encodeGetblocksPayload conf (map block_hash latest) nullHash256

    -- timeoutRetryS (timeout_s env) $
    tSend trans getblocks

    recv env node [] BTC_CMD_INV $ \(InvPayload {
        inv_vect = inv_vect
    }) -> do
        nodeMsg env node $ "inv received with " ++ show (length inv_vect) ++ " item(s)" -- ++ show inv_vect

        let hashes = map invToHash256 $
                     take (tckr_max_block_batch conf) inv_vect

        callback node hashes
        
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

scheduleFetch :: MainLoopEnv -> [Hash256] -> IO () -> IO ()
scheduleFetch env init_hashes callback = do
    let conf = global_conf env

    -- use this lock first before changing the ioref
    var_lock <- LK.new

    -- task array of (hash, atom maybe block) pair
    tarray <- forM init_hashes $ \hash -> do
        var <- newA Nothing :: IO (Atom (Maybe Block))
        return (hash, var)

    -- number of blocks in the front that have already been added
    added_var <- newA 0 :: IO (Atom (Int))
    let total = length tarray

    let reassign sched task blacklist = do
            clearBlacklist sched
            doFetch sched (fetchTaskToHashes (mconcat task)) blacklist

        doFetch sched hashes blacklist =
            envSpreadActionExcept
                blacklist env
                (\task -> [NormalAction (fetchBlock task)]) $
                buildFetchTasks (tckr_max_block_task conf) hashes (taskDone sched)

        taskDone sched node task results = do
            removeFromBlacklist sched node
            valid <- removeTask sched task

            forkIO $ LK.with var_lock $ do
                added <- getA added_var
                
                -- is this still a valid task
                if valid then do
                    -- fill in the block
                    forM_ results $ \(hash, payload) -> do
                        -- decode now
                        let block = decodeFailLE payload
                            midx = findIndex ((== hash) . fst) tarray

                        -- here if the hash already exists, no decoding will be needed
                        case midx of
                            Just idx ->
                                if idx >= added then -- not added
                                    setA (snd (tarray !! idx)) (Just block)
                                else
                                    return ()
                            Nothing ->
                                nodeMsg env node $ "irrelavent block " ++ show block

                    refreshBlock sched node
                    return ()
                    -- nodeMsg env node $ "task decoding finished"
                else do
                    -- task already finished
                    nodeMsg env node $ "duplicated assignment"

            return ()

        refreshFinal node res =
            case res of
                Right _ -> return ()
                Left err -> do
                    -- need to release the lock
                    LK.release var_lock
                    if shouldExitOn err then do
                        nodeMsg env node "killing the main thread(bug)"
                        envExit env err
                    else
                        nodeMsg env node ("refresh failed with: " ++ show err)

        -- refresh block inventory
        refreshBlock sched node = do
            flip forkFinally (refreshFinal node) $ do
                LK.with var_lock $ do
                    old_added <- getA added_var

                    let all_blocks = mapM (getA . snd) tarray
                        new_succ = all_blocks >>= return . takeWhile maybeToBool . drop old_added

                    -- newly received successive blocks
                    new_succ_count <- length <$> new_succ
            
                    new_added <- appA (+ new_succ_count) added_var

                    if new_succ_count /= 0 then do
                        tLnM (show new_succ_count ++ " block(s) to add")

                        -- NOTE: the downloaded part is cleared first
                        -- so that there won't be a double increase in memory usage
                        -- when addind the block(becasue of the reference to these fields)

                        let clear_fields = drop old_added $ take new_added tarray

                        -- clear corresponding fields to free some memory
                        -- and pass alone the blocks to add to the chain
                        -- hopefully the block list is stored in stack so it can be free'd soon
                        let readNClear =
                                forM clear_fields $ \(_, blockv) -> do
                                    (Just block) <- getA blockv
                                    setA blockv Nothing
                                    return block
                            
                        readNClear >>= envAddBlocks env node

                        nodeMsg env node (show new_added ++ " new block(s) added")

                        if new_added == total then do
                            nodeMsg env node "all fetching finished"
                            cancel sched
                            callback
                        else
                            return ()
                    else
                        return ()

    newScheduler env (tckr_block_fetch_timeout conf)
        (\sched -> doFetch sched init_hashes []) -- init assign
        reassign -- reassign
        (\sched tasks -> clearBlacklist sched >> reassign sched tasks []) -- fail

    return ()

-- 00000000a967199a2fad0877433c93df785a8d8ce062e5f9b451cd1397bdbf62

fetchBlock :: BlockFetchTask -> MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
fetchBlock task env node _ = do
    let conf = global_conf env
        trans = conn_trans node

        hashes = fetchTaskToHashes task
        invs = map (InvVector INV_TYPE_BLOCK) hashes

    getdata <- encodeMsg conf BTC_CMD_GETDATA $ encodeGetdataPayload invs
    -- timeoutRetryS (timeout_s env) $ 
    tSend trans getdata

    -- nodeMsg env node "getdata sent"

    fetched_var <- newA MAP.empty

    keepRecv' env node BTC_CMD_BLOCK $ \payload -> do
        -- nodeMsg env node $ "received block " ++ show block

        -- only decode head to reduce decoding time
        let BlockHeader (Block {
            block_hash = hash
        }) = decodeFailLE payload

        -- if the hash is in the task list
        if hash `elem` hashes then do
            fetched <- appA (MAP.insert hash payload) fetched_var

            if MAP.size fetched == length hashes then do
                -- all blocks fetched
                nodeMsg env node "partial task finished"

                done env node (task {
                    fetch_block = MAP.toList fetched
                })

                return [ StopProp, DumpMe ]
            else
                return [ StopProp ]
        else
            return []

seekNode :: MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
seekNode env node msg = do
    getaddr <- encodeMsg (global_conf env) BTC_CMD_GETADDR $ encodeGetaddrPayload
    tSend (conn_trans node) getaddr
    return []
