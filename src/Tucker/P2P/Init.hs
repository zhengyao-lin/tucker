module Tucker.P2P.Init where

import Data.Foldable as FD
import qualified Data.Set as SET
import qualified Data.Set.Ordered as OSET

import Control.Monad
import Control.Concurrent
import Control.Monad.Loops
import Control.Monad.Morph
import Control.Monad.Trans.Resource
import Control.Concurrent.Thread.Delay

import Tucker.Msg
import Tucker.Conf
import Tucker.Util
import Tucker.Atom
import qualified Tucker.Lock as LK

import Tucker.P2P.Node
import Tucker.P2P.Util
import Tucker.P2P.Action
import Tucker.P2P.Server

import Tucker.Storage.Chain

import System.Mem

bootstrap :: MainLoopEnv -> [String] -> IO ()
bootstrap env hostnames = do
    addrs <- mapM (seedLookup $ global_conf env) hostnames >>= (pure . concat)
    probe env (take (envConf env tckr_max_node) addrs)
    
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

-- measure the availability & responding time
-- of each node(every tckr_reping_time second)
pingLoop :: MainLoopEnv -> IO ()
pingLoop env =
    forever $ do
        let reping_time = envConf env tckr_reping_time
        
        delay $ fi $ reping_time * 1000 * 1000

        cur_list <- getA $ node_list env
        now <- unixTimestamp

        forM_ cur_list $ \node -> do
            alive <- getA $ alive node
            last_seen <- nodeLastSeen node

            if alive && now - last_seen > reping_time then
                nodePrependActions node [ NormalAction pingDelay ]
            else
                return ()

gcLoop :: MainLoopEnv -> IO ()
gcLoop env@(MainLoopEnv {
    global_conf = TCKRConf {
        tckr_node_alive_span = max_alive_span,
        tckr_node_max_blacklist_count = max_bl_count
    }
}) =
    forever $ do
        delay $ gc_interv env

        cur_list <- getA $ node_list env

        timestamp <- unixTimestamp

        marked <- forM cur_list $ \node -> do
            alive <- getA $ alive node

            -- check if the node has not been responding for a long time
            last_seen <- nodeLastSeen node
            bl_count  <- nodeBlacklistTime node

            let kill =
                    timestamp - last_seen > max_alive_span ||
                    bl_count > max_bl_count

            if kill then do
                nodeMsg env node "!!! killed(timeout or too many blacklist count)"
                killThread $ thread_id node
            else
                return ()

            return (node, alive && not kill)

        let new_list = map fst $ filter snd marked

        setA (node_list env) new_list

        -- res <- forM new_list $ \node -> do
        --     action_list <- getA (action_list node)
        --     new_action <- getA (new_action node)
        --     return (length action_list, length new_action)

        -- envMsg env $ "actions: " ++ show res

        envMsg env $ "gc: " ++
                     show (length cur_list - length new_list) ++
                     " dead node(s) collected"
        envMsg env $ "all " ++ show (length new_list) ++ " node(s): " ++ show new_list

-- syncOne env n = do
--     envSpreadSimpleAction env (NormalAction (syncChain (pure ()))) n
--     return ()

mainLoop :: TCKRConf -> IO MainLoopEnv
mainLoop conf = runResourceT $ do
    env <- initEnv conf

    lift $ bootstrap env (tckr_bootstrap_host conf)
    -- setA (node_list env) init_nodes

    -- keep the resource, never exit
    gc_tid <- resourceForkIO $ lift $ gcLoop env

    -- bootstrap finished, start sync with 3 nodes
    lift $ forkIO $ sync env 3

    -- forkIO $ blockCollectLoop env
    -- forkIO $ ioLoop env

    return env
