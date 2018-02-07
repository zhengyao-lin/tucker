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
import qualified Control.Concurrent.Lock as LK

import Tucker.Msg
import Tucker.Conf
import Tucker.Util
import Tucker.Atom

import Tucker.P2P.Node
import Tucker.P2P.Util
import Tucker.P2P.Action
import Tucker.P2P.Server

import Tucker.Chain.Object

bootstrap :: MainLoopEnv -> [String] -> IO ()
bootstrap env hostnames = do
    addrs <- mapM (seedLookup $ global_conf env) hostnames >>= (pure . concat)
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
-- of each node(every 20 * 1000 * 1000 ms)
pingLoop :: MainLoopEnv -> IO ()
pingLoop env =
    forever $ do
        let reping_time = envConf env tckr_reping_time
        
        delay $ fromIntegral $ reping_time * 1000 * 1000

        cur_list <- getA $ node_list env
        now <- unixTimestamp

        forM cur_list $ \node -> do
            alive <- getA $ alive node
            last_seen <- getA $ last_seen node

            if alive && now - last_seen > reping_time then
                nodePrependAction node [ NormalAction pingDelay ]
            else
                return ()

gcLoop :: MainLoopEnv -> IO ()
gcLoop env = 
    forever $ do
        cur_list <- getA $ node_list env

        timestamp <- unixTimestamp

        marked <- forM cur_list $ \node -> do
            alive <- getA $ alive node

            -- check if the node has not been responding for a long time
            last_seen <- nodeLastSeen node
            let alive_span = envConf env tckr_node_alive_span
                kill = timestamp - last_seen > alive_span

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

mainLoop :: TCKRConf -> IO MainLoopEnv
mainLoop conf = runResourceT $ do
    env <- initEnv conf

    lift $ bootstrap env (tckr_bootstrap_host conf)
    -- setA (node_list env) init_nodes

    -- keep the resource, never exit
    resourceForkIO $ lift $ gcLoop env
    -- forkIO $ blockCollectLoop env
    -- forkIO $ ioLoop env

    return env
