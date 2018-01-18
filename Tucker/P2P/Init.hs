module Tucker.P2P.Init where

import Tucker.Std
import Tucker.Conf
import Tucker.Atom
import Tucker.P2P.Node
import Tucker.P2P.Util
import Tucker.P2P.Server

bootstrap :: MainLoopEnv -> [String] -> IO [BTCNode]
bootstrap env hostnames = do
    addrs <- (mapM (seedLookup $ btc_network env) hostnames) >>= (pure . concat)
    probe env addrs

mainLoop :: BTCNetwork -> TCKRConf -> IO ()
mainLoop net conf = do
    env <- initEnv net conf

    init_nodes <- bootstrap env (tckr_bootstrap_host conf)
    setA (node_list env) init_nodes

    -- fork to 3 sub-loops
    -- 1. seekerLoop: maintain the number of nodes
    -- 2. serverLoop: accept new incoming nodes
    -- 3. actionLoop: interact with user and send actions to nodes

    return ()
