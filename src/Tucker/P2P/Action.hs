module Tucker.P2P.Action where

import Data.List
import qualified Data.Foldable as FD
import qualified Data.ByteString as BSR

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Applicative
import Control.Monad.Loops

-- import System.IO
import System.Exit
import System.Random

import Tucker.Msg
import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Atom
import Tucker.Error
import Tucker.Thread
import Tucker.Transport
import qualified Tucker.Lock as LK
import qualified Tucker.Container.Map as MAP

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
        decodePayload env node cmd payload fail proc

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
    start <- msMonoTime
    -- timeoutRetryS (timeout_s env) $
    
    tSend trans ping

    recv env node [] BTC_CMD_PONG $ \(PingPongPayload back_nonce) -> do
        if back_nonce == nonce then do
            end <- msMonoTime
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
sync :: MainLoopEnv -> Int -> IO a -> IO ()
sync env n fin_cb = do
    sync_inv_var <- newA []
    finished_var <- newA 0 :: IO (Atom Int)

    -- current height of the main branch
    main_height <- envMainBranchHeight env

    let callback = do
            envFork env THREAD_OTHER (sync env n fin_cb)
            return ()

        all_finished = do
            envInfo env "all blocks synchronized, exiting sync"
            envSyncChain env

            when (envConf env tckr_enable_mempool) $ do
                envInfo env "broadcasting request for mem pool"
                envBroadcastAction env (NormalAction requestMemPool)

            -- finishing callback
            fin_cb

            return ()
            
        taskDone sched node hashes = do
            removeNode sched node

            appA (hashes:) sync_inv_var
            finished <- appA (+1) finished_var

            when (finished == n) $ do
                -- no double execution when finished > n
                cancel sched

                sync_inv <- listUnion <$> getA sync_inv_var
                sync_inv <- envFilterExistingBlock env sync_inv

                let last_batch = length sync_inv < envConf env tckr_max_getblocks_batch

                when last_batch $ do
                    -- not given a full batch -- we are reaching the tip
                    -- set the sync-ready flag
                    envInfo env "last batch received, setting sync ready flag"
                    envSetSyncReady env True

                if null sync_inv then
                    -- no sync inv
                    all_finished
                else do
                    let final_hashes = sync_inv

                    envMsg env $
                        "fetching final inventory of " ++
                        show (length final_hashes) ++ " item(s)"

                    scheduleFetch env final_hashes $ 
                        if last_batch then
                            all_finished
                        else
                            callback

        action sched = NormalAction (syncChain (taskDone sched))
        
        node_pred blacklist node =
            nodeStartHeight node > main_height &&
            node `notElem` blacklist

        reassign sched tasks blacklist =
            envSpreadActionExcept (node_pred blacklist) env (const [action sched]) tasks

    at_tip <- envIsLongestChain env

    -- already synchronized
    -- because nodes don't return empty invs
    -- so it's(maybe) the only way to tell
    -- if you are on the tip at the beginning
    if at_tip then do
        envSetSyncReady env True
        all_finished
    else do
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

    latest <- getA (block_chain env) >>= flip flagBlockHashes (tckr_known_inv_count conf)

    nodeMsg env node $ "latest known blocks" ++ show latest

    getblocks <- encodeMsg conf BTC_CMD_GETBLOCKS $
                 encodeGetblocksPayload conf latest nullHash256

    -- timeoutRetryS (timeout_s env) $
    tSend trans getblocks

    recv env node [] BTC_CMD_INV $ \(InvPayload {
        inv_vect = inv_vect
    }) -> do
        nodeMsg env node $ "inv received with " ++ show (length inv_vect) ++ " item(s)" -- ++ show inv_vect

        let hashes = map invToHash256 $
                     take (tckr_max_getblocks_batch conf) inv_vect

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

    use_segwit <- envForkEnabled env "segwit"

    when use_segwit $
        envMsg env "enabling segwit"

    let node_flag =
            -- only use segwit-supporting nodes
            if use_segwit then [ TCKR_NODE_WITNESS ]
            else []

        reassign sched task blacklist = do
            clearBlacklist sched
            doFetch sched (fetchTaskToHashes (mconcat task)) blacklist

        node_pred blacklist node =
            nodeHasService node (NodeServiceType node_flag) &&
            node `notElem` blacklist

        doFetch sched hashes blacklist =
            envSpreadActionExcept
                (node_pred blacklist) env
                (\task -> [NormalAction (fetchBlock task)]) $
                buildFetchTasks (tckr_max_block_task conf) hashes (taskDone sched)

        taskDone sched node task results = do
            removeFromBlacklist sched node
            valid <- removeTask sched task

            LK.with var_lock $ do
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
                                when (idx >= added) $ -- not added
                                    setA (snd (tarray !! idx)) (Just block)

                            Nothing ->
                                nodeMsg env node $ "irrelavent block " ++ show block

                    refreshBlock sched node
                    -- nodeMsg env node $ "task decoding finished"
                else
                    -- task already finished
                    nodeMsg env node $ "duplicated assignment"

        -- refresh block inventory
        refreshBlock sched node = LK.with var_lock $ do
            old_added <- getA added_var

            let all_blocks = mapM (getA . snd) tarray
                new_succ = all_blocks >>= return . takeWhile maybeToBool . drop old_added

            -- newly received successive blocks
            new_succ_count <- length <$> new_succ
    
            new_added <- appA (+ new_succ_count) added_var

            when (new_succ_count /= 0) $ do
                envMsg env (show new_succ_count ++ " block(s) to add")

                -- NOTE: the downloaded part is cleared first
                -- so that there won't be a double increase in memory usage
                -- when addind the block(becasue of the reference to these fields)

                let clear_fields = drop old_added $ take new_added tarray

                -- clear corresponding fields to free some memory
                -- and add the blocks to the chain
                -- hopefully the block list is stored in the stack so it can be free'd soon
                let readNClear =
                        forM clear_fields $ \(_, blockv) -> do
                            (Just block) <- getA blockv
                            -- EDIT ME
                            -- BSR.appendFile "test_blocks" (encodeLE block)
                            setA blockv Nothing
                            return block
                
                envFork env THREAD_VALIDATION $ do
                    readNClear >>= envAddBlocks env node
                    nodeMsg env node (show new_added ++ " new block(s) added")

                    when (new_added == total) $ do
                        nodeMsg env node "all fetching finished"
                        cancel sched
                        callback

                return ()

    newScheduler env (tckr_block_fetch_timeout conf)
        (\sched -> doFetch sched init_hashes []) -- init assign
        reassign -- reassign
        (\sched tasks -> clearBlacklist sched >> reassign sched tasks []) -- fail

    return ()

-- 00000000a967199a2fad0877433c93df785a8d8ce062e5f9b451cd1397bdbf62

getFullDataMsg :: MainLoopEnv -> InvType -> [Hash256] -> IO ByteString
getFullDataMsg env itype hashes = do
    -- check if segwit is activated
    use_segwit <- envForkEnabled env "segwit"

    let conf = global_conf env
        flag =
            if use_segwit then
                case itype of
                    INV_TYPE_BLOCK -> INV_TYPE_WITNESS_BLOCK
                    INV_TYPE_TX -> INV_TYPE_WITNESS_TX
                    _ -> itype
            else itype

        invs = map (InvVector flag) hashes

    encodeMsg conf BTC_CMD_GETDATA $ encodeGetdataPayload invs

fetchBlock :: BlockFetchTask -> MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
fetchBlock task env node _ = do
    let conf = global_conf env
        trans = conn_trans node
        hashes = fetchTaskToHashes task 

    getFullDataMsg env INV_TYPE_BLOCK hashes >>= tSend trans

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
seekNode env node _ = do
    full <- envNodeFull env

    unless full $ do
        getaddr <- encodeMsg (global_conf env) BTC_CMD_GETADDR encodeGetaddrPayload
        tSend (conn_trans node) getaddr

    return [ DumpMe ]

requestMemPool :: MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
requestMemPool env node _ = do
    mp <- encodeMsg (global_conf env) BTC_CMD_MEMPOOL encodeMempoolPayload
    tSend (conn_trans node) mp
    return [ DumpMe ]

sendMsg :: ByteString -> MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
sendMsg msg env node _ = do
    tSend (conn_trans node) msg
    return [ DumpMe ]

sendMsgA :: ByteString -> NodeAction
sendMsgA = NormalAction . sendMsg
