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
    flip forkMapM__ hostnames $ \hostname -> do
        addrs <- seedLookup (global_conf env) hostname
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
        tckr_node_max_blacklist_count = max_bl_count,
        tckr_bootstrap_host = boot_host,
        tckr_seek_min = seek_min
    }
}) =
    forever $ do
        delay $ gc_interv env

        timestamp <- unixTimestamp

        cur_list <- getA $ node_list env

        -- filter out dead/should-be-killed nodes
        new_list <- flip filterM cur_list $ \node -> do
            alive <- getA $ alive node

            -- check if the node has not been responding for a long time
            last_seen <- nodeLastSeen node
            bl_count  <- nodeBlacklistTime node

            let kill = timestamp - last_seen > max_alive_span ||
                       bl_count > max_bl_count

            if kill then do
                nodeWarn env node "killed because of timeout or too many blacklist count"
                killThread $ thread_id node
            else
                return ()

            return (alive && not kill)

        -- update node list
        setA (node_list env) new_list

        let killed = length cur_list - length new_list

        if killed /= 0 then do
            envMsg env $ "gc: " ++ show killed ++ " dead node(s) collected"
            envMsg env $ "all " ++ show (length new_list) ++ " node(s)"
        else
            return ()

        -- check if there are too few nodes
        if length new_list < seek_min then
            -- seek for more nodes
            if null new_list then do
                envWarn env "lost all connections, try to bootstrap again"
                bootstrap env boot_host
            else do
                envWarn env "too few nodes, start seeking"
                envSpreadSimpleAction env (NormalAction seekNode) (length new_list)
                return ()
        else
            return ()

-- syncOne env n = do
--     envSpreadSimpleAction env (NormalAction (syncChain (pure ()))) n
--     return ()

mainLoop :: TCKRConf -> IO MainLoopEnv
mainLoop conf = runResourceT $ do
    env <- initEnv conf

    lift $ bootstrap env (tckr_bootstrap_host conf)
    -- setA (node_list env) init_nodes

    -- wait until enough nodes are connected
    lift $ yield `untilM_` ((>= envConf env tckr_min_node) <$> length <$> envAliveNodes env)

    lift $ envMsg env "boostrap done"

    -- keep the resource, never exit
    gc_tid <- resourceForkIO $ lift $ gcLoop env

    -- bootstrap finished, start sync with 3 nodes
    lift $ forkIO $ sync env 3

    return env
