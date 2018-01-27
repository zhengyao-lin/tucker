module Tucker.P2P.Init where

import Data.Foldable as FD
import qualified Data.Set as SET
import qualified Data.Set.Ordered as OSET

import Control.Monad
import Control.Concurrent
import Control.Monad.Loops
import Control.Concurrent.Thread.Delay
import qualified Control.Concurrent.Lock as LK

import Tucker.Std
import Tucker.Msg
import Tucker.Conf
import Tucker.Util
import Tucker.Atom

import Tucker.P2P.Node
import Tucker.P2P.Util
import Tucker.P2P.Server

import Tucker.Chain.Object
import Tucker.Chain.Cached

bootstrap :: MainLoopEnv -> [String] -> IO ()
bootstrap env hostnames = do
    addrs <- (mapM (seedLookup $ btc_network env) hostnames) >>= (pure . concat)
    probe env addrs

    
{-

some ideas about block chain storage
when the tree has reached a certain height(e.g. 10240)
we want to dump part of it into the disk

1. what do we dump, tree or chain
    tree(so that we can save the branchings and act more quickly at the next boot)

2. how do we store the chain
    we can transform our chain to a encodable and decodable type BlockTreePart
    and write it to a file

3. 

-}

-- NOTE: need to acquire the lock first
trySealTreeCached :: MainLoopEnv -> IO ()
trySealTreeCached env = do
    LK.acquire (tree_lock env)

    tree <- getA (block_tree env)

    v1 <- getEnvConf env tckr_max_block_per_chunk
    v2 <- getEnvConf env tckr_max_tree_insert_depth

    top_height <- treeCachedTopChunkHeight tree

    if top_height > v1 + v2 then do
        new_tree <- sealTreeCached v1 tree
        setA (block_tree env) new_tree
    else
        return ()

    LK.release (tree_lock env)

-- collect idle blocks
blockLoop :: MainLoopEnv -> IO ()
blockLoop env =
    whileM_ (pure True) $ do
        idle <- getA $ idle_block env

        max_depth <- getEnvConf env tckr_max_tree_insert_depth

        res <- forM (FD.toList idle) $ \bph@(BlockPayloadHashed hash payload) -> do
            LK.acquire (tree_lock env)

            tree <- getA (block_tree env)
            res <- insertToTreeCached max_depth tree bph

            -- envMsg env $ "!!!!!!!!!!!!!!!!!!!!!!!!!"

            case res of
                Left err -> do
                    -- envMsg env $ "!!!!! failed to collect idle block " ++ show hash ++ ": " ++ show err
                    -- appA ((hash, payload):) (idle_block env)
                    LK.release (tree_lock env)

                    return (False, bph)

                Right _ -> do
                    envMsg env $ "idle block " ++ show hash ++ " confirmed"
                    LK.release (tree_lock env)

                    -- try to seal the top chunk
                    trySealTreeCached env

                    return (True, bph)

        let collected_idle = map snd $ filter fst res

        -- update idle blocks
        appA (OSET.\\ OSET.fromList collected_idle) (idle_block env)
        
        if null collected_idle then
            -- no new block collected
            -- wait for 10 sec
            delay $ 10 * 1000 * 1000
        else -- try again immediately
            return ()

gcLoop :: MainLoopEnv -> IO ()
gcLoop env = 
    whileM_ (pure True) $ do
        cur_list <- getA $ node_list env

        timestamp <- unixTimestamp

        marked <- forM cur_list $ \node -> do
            alive <- getA $ alive node

            -- check if the node has not been responding for a long time
            last_seen <- nodeLastSeen node
            alive_span <- getEnvConf env tckr_node_alive_span
            let kill = timestamp - last_seen > alive_span

            if kill then do
                nodeMsg env node "timeout and quit"
                killThread $ thread_id node
            else
                return ()

            return (node, alive && not kill)

        let new_list = map fst $ filter snd marked

        setA (node_list env) new_list

        envMsg env $ "gc: " ++
                     (show $ length cur_list - length new_list) ++
                     " dead node(s) collected"
        envMsg env $ "all nodes: " ++ (show new_list)

        delay $ gc_interv env

mainLoop :: BTCNetwork -> TCKRConf -> IO MainLoopEnv
mainLoop net conf = do
    env <- initEnv net conf

    bootstrap env (tckr_bootstrap_host conf)
    -- setA (node_list env) init_nodes

    -- fork to 3 sub-loops
    -- 1. seekerLoop: maintain the number of nodes
    -- 2. serverLoop: accept new incoming nodes
    -- 3. actionLoop: interact with user and send actions to nodes
    -- 4. gcLoop: garbage node collection
    -- 5. blockLoop: collect idling blocks

    forkIO $ gcLoop env
    forkIO $ blockLoop env
    -- forkIO $ ioLoop env

    return env
