module Tucker.P2P.Init where

import Data.Foldable as FD
import qualified Data.Set as SET
import qualified Data.Set.Ordered as OSET

import Control.Monad
import Control.DeepSeq
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

import System.Mem

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

        forM_ cur_list $ \node -> do
            alive <- getA $ alive node
            last_seen <- getA $ last_seen node

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
                     (show $ length cur_list - length new_list) ++
                     " dead node(s) collected"
        envMsg env $ "all nodes: " ++ (show new_list)

sync :: MainLoopEnv -> Int -> IO ()
sync env n = do
    -- [[Hash256]]
    sync_inv <- newA []

    let callback = do    
            -- delay $ 5000000
            -- cb
            forkIO $ sync env n

            return ()
            
        action = NormalAction (syncChain n sync_inv callback)

    envSpreadSimpleAction env action n

    return ()

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
    lift $ forkIO $ sync env 1 -- $ do
        -- chain <- getA (block_chain env)

        -- let b1 = head $ map branchToBlockList (edge_branches chain)
        --     b2 = branchToBlockList (maybe undefined id $ buffer_chain chain)

        -- envMsg env $ show (head b1) ++ ", " ++ show (last b1)
        -- envMsg env $ show (head b2) ++ ", " ++ show (last b2)

        -- appA (\c -> c {
        --     buffer_chain = force $ buffer_chain c,
        --     edge_branches = force $ edge_branches c
        -- }) (block_chain env)

        -- chain <- getA (block_chain env)

        -- envMsg env $ show $ buffer_chain chain
        -- envMsg env $ show $ edge_branches chain

        -- nodes <- getA (node_list env)
        -- forM_ (init nodes) $ \node -> do
        --     -- appA ((:[]) . last) (action_list node)
        --     alist <- getA (action_list node)

        --     if length alist >= 2 then
        --         killThread (thread_id node)
        --     else
        --         return ()

        --     killThread (thread_id node)
                
        -- delay $ 15 * 1000 * 1000

        -- killThread gc_tid

        -- who cleared the memory?
        -- 1. some thread still holds the handler which contains the closure that contains the downloaded block
        -- 2. levedb is the devil, whose resource handler is in gc_tid, and if gc is killed, memory is freed

        -- tests
        -- 1. full(strict chain, kill node threads, run gc, kill gc)  -- free'd
        -- 2. strict chain, kill gc                                   -- not free'd
        -- 3. strict chain, kill node threads, run gc                 -- free'd
        -- 4. kill node threads, run gc                               -- free'd
        -- 5. kill nodes with 2+ actions, run gc                      -- not free'd
        -- 6. leave only the last node alive                          -- not free'd
        -- 7. kill nodes with 2+ actions, delete chain, run gc        -- free'd
        -- 8. delete chain                                            -- free'd
        -- 9. delete databases only                                   -- not free'd ?
        -- 10. delete several other things                            -- free'd
        -- 11. delete buffer chain and edge branches only             -- free'd

        -- envMsg env "finished"

        -- delay $ 5 * 1000 * 1000

        -- envMsg env "performing gc"
        -- performGC
        -- envMsg env "gc finished"

    -- forkIO $ blockCollectLoop env
    -- forkIO $ ioLoop env

    return env
