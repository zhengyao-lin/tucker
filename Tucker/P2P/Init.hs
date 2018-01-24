module Tucker.P2P.Init where
    
import qualified Data.Set as SET

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

bootstrap :: MainLoopEnv -> [String] -> IO ()
bootstrap env hostnames = do
    addrs <- (mapM (seedLookup $ btc_network env) hostnames) >>= (pure . concat)
    probe env addrs

-- collect idle blocks
blockLoop :: MainLoopEnv -> IO ()
blockLoop env =
    whileM_ (pure True) $ do
        idle <- getA $ idle_block env

        res <- forM (SET.toList idle) $ \bph@(BlockPayloadHashed hash payload) -> do
            LK.acquire (tree_lock env)

            tree <- getA (block_tree env)

            case insertToTree tree bph of
                Left err -> do
                    envMsg env $ "failed to collect idle block " ++ show hash ++ ": " ++ show err
                    -- appA ((hash, payload):) (idle_block env)
                    LK.release (tree_lock env)

                    return (False, bph)

                Right new_tree -> do
                    envMsg env $ "!!!!!!!!! idle block " ++ show hash ++ " collected"
                    setA (block_tree env) new_tree
                    LK.release (tree_lock env)

                    return (True, bph)

        let new_idle = map snd $ filter fst res

        -- update idle blocks
        appA (SET.union $ SET.fromList new_idle) (idle_block env)
        
        delay 100000

gcLoop :: MainLoopEnv -> IO ()
gcLoop env = 
    whileM_ (pure True) $ do
        cur_list <- getA $ node_list env

        timestamp <- unixTimestamp

        marked <- forM cur_list $ \node -> do
            alive <- getA $ alive node

            last_seen <- nodeLastSeen node
            alive_span <- getEnvConf env tckr_node_alive_span
            let kill = timestamp - last_seen > alive_span

            if kill then do
                nodeMsg env node "timeout and quit"
                tid <- getA (thread_id node)
                killThread tid
            else return ()

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
