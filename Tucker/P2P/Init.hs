module Tucker.P2P.Init where

import Control.Monad
import Control.Concurrent
import Control.Monad.Loops
import Control.Concurrent.Thread.Delay

import Tucker.Std
import Tucker.Conf
import Tucker.Atom
import Tucker.P2P.Node
import Tucker.P2P.Util
import Tucker.P2P.Server

bootstrap :: MainLoopEnv -> [String] -> IO ()
bootstrap env hostnames = do
    addrs <- (mapM (seedLookup $ btc_network env) hostnames) >>= (pure . concat)
    probe env addrs

gcLoop :: MainLoopEnv -> IO ()
gcLoop env = 
    whileM_ (pure True) $ do
        cur_list <- getA $ node_list env

        mark <- forM cur_list $ \node -> do
            alive <- getA $ alive node
            return (node, alive)

        let new_list = map fst $ filter snd mark

        setA (node_list env) new_list

        envMsg env $
            "gc: " ++
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

    forkIO $ gcLoop env

    return env
