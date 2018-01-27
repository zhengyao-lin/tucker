module Tucker.Conf where

import Data.Word
import Network.Socket

data TCKRConf =
    TCKRConf {
        tckr_trans_timeout   :: Int, -- in sec
        tckr_bootstrap_host  :: [String],

        tckr_seek_min        :: Int,
        tckr_seek_max        :: Int,
        tckr_max_node        :: Int,

        tckr_node_blacklist  :: [SockAddr],

        tckr_gc_interval     :: Integer,

        tckr_max_block_task  :: Int,

        tckr_node_alive_span :: Word64,

        tckr_known_inv_count :: Int,
        -- max number of hashes to send when trying to sync witht the network

        tckr_block_tree_path :: FilePath,
        tckr_max_block_per_chunk :: Int,
        tckr_max_tree_insert_depth :: Int -- max search depth when inserting a block
    } deriving (Show)

tucker_version = "0.0.1"

tucker_btp_name_regex = "^btp.(0|[1-9][0-9]*)$"
tucker_btp_name_gen num = "btp." ++ show num

-- tucker_cache_tree_chunk = 1

tucker_default_conf =
    TCKRConf {
        tckr_trans_timeout = 5, -- 5 sec
        tckr_bootstrap_host = [ "seed.tbtc.petertodd.org" ],

        tckr_seek_min = 16, -- if node_count < min_seek then seek for more nodes
        tckr_seek_max = 100, -- if node_count >= max_seek then stop seeking

        tckr_max_node = 128, -- max number of nodes in total
        
        tckr_node_blacklist = [
                ip4 (127, 0, 0, 1),
                ip4 (0, 0, 0, 0)
            ],
    
        tckr_gc_interval = 30 * 1000 * 1000, -- 30 sec
        
        tckr_max_block_task = 32,

        -- in sec
        tckr_node_alive_span = 90 * 60, -- 90 min
        
        tckr_known_inv_count = 8,

        tckr_block_tree_path = ".block-tree",
        tckr_max_block_per_chunk = 2048,
        tckr_max_tree_insert_depth = 256

        -- the collector will wait until the top chunk
        -- has (tckr_max_block_per_chunk + tckr_max_tree_insert_depth)
        -- blocks and split & seal the previous (tckr_max_tree_insert_depth)
        -- chunk, while create a new chunk for tckr_max_tree_insert_depth
    }

    where
        ip4 = SockAddrInet 0 . tupleToHostAddress
