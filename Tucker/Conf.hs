module Tucker.Conf where

import Network.Socket

data TCKRConf =
    TCKRConf {
        tckr_trans_timeout :: Int, -- in sec
        tckr_bootstrap_host :: [String],

        tckr_seek_min       :: Int,
        tckr_seek_max       :: Int,
        tckr_max_node       :: Int,

        tckr_node_blacklist :: [SockAddr],

        tckr_gc_interval    :: Integer
    } deriving (Show)

tucker_version = "0.0.1"
tucker_default_conf =
    TCKRConf {
        tckr_trans_timeout = 5, -- 5 sec
        tckr_bootstrap_host = [ "seed.tbtc.petertodd.org" ],

        tckr_seek_min = 16, -- if node_count < min_seek then seek for more nodes
        tckr_seek_max = 32, -- if node_count >= max_seek then stop seeking

        tckr_max_node = 64, -- max number of nodes in total
        
        tckr_node_blacklist = [
                ip4 (127, 0, 0, 1),
                ip4 (0, 0, 0, 0)
            ],
    
        tckr_gc_interval = 30 * 1000 * 1000 -- 30 sec
    }

    where
        ip4 = SockAddrInet 0 . tupleToHostAddress
