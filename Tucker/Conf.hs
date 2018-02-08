module Tucker.Conf where

import Data.Hex
import Data.Word

import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import System.FilePath
import System.Directory

import Network.Socket
import Crypto.PubKey.ECC.Types

tucker_curve = getCurveByName SEC_p256k1

data NodeServiceTypeSingle = TCKR_NODE_NETWORK | TCKR_NODE_GETUTXO | TCKR_NODE_BLOOM deriving (Show, Eq)
data NodeServiceType = NodeServiceType [NodeServiceTypeSingle] deriving (Show, Eq)

data TCKRConf =
    TCKRConf {
        tckr_net_version     :: Integer,
        tckr_node_service    :: NodeServiceType,

        -- keyspaces: block, tx, 
        tckr_db_path         :: FilePath,
        tckr_ks_block        :: String, -- block hash -> block data
        tckr_ks_tx           :: String, -- tx hash -> tx data
        tckr_ks_chain        :: String, -- block height -> block hash

        tckr_user_agent      :: String,

        tckr_wif_pref        :: Word8,
        tckr_pub_pref        :: Word8,
        tckr_magic_no        :: BSR.ByteString,
        tckr_listen_port     :: Word16,

        tckr_genesis_raw     :: BSR.ByteString, -- genesis hash

        tckr_trans_timeout   :: Int, -- in sec
        tckr_bootstrap_host  :: [String],

        tckr_seek_min        :: Int,
        tckr_seek_max        :: Int,
        tckr_max_node        :: Int,

        tckr_node_blacklist  :: [SockAddr],

        tckr_gc_interval     :: Integer,

        tckr_max_block_task  :: Int,

        tckr_node_alive_span :: Word64,
        tckr_reping_time     :: Word64,

        tckr_known_inv_count :: Int,
        -- max number of hashes to send when trying to sync witht the network

        tckr_block_tree_path :: FilePath,
        tckr_max_block_per_chunk :: Int,
        tckr_max_tree_insert_depth :: Int, -- max search depth when inserting a block

        tckr_fetch_dup_node  :: Int,
        -- number of duplicated nodes to fetch the same bunch of block
        tckr_fetch_dup_max_task :: Int,
        -- if the number of block fetch task is less than this number,
        -- use dup_node

        -- max difference of the timestamp of a block with the time received
        tckr_max_block_future_diff :: Word32, -- in sec

        -- the difficulty changes every tckr_diff_change_span blocks
        tckr_diff_change_span :: Word32,
        tckr_expect_diff_change_time :: Word32, -- expected difficulty change time in sec

        tckr_use_special_min_diff :: Bool -- support special-min-difficulty or not(mainly on testnet)
    } deriving (Show)

tucker_version = "0.0.1"

tucker_btp_name_regex = "^btp.(0|[1-9][0-9]*)$"
tucker_btp_name_gen num = "btp." ++ show num

tucker_bdiff_diff1 = 0x00000000ffff0000000000000000000000000000000000000000000000000000
tucker_pdiff_diff1 = 0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff

-- tucker_cache_tree_chunk = 1

tucker_default_conf_mainnet = do
    user_home <- getHomeDirectory

    let tucker_path = user_home </> ".tucker"

    createDirectoryIfMissing False tucker_path

    return $ TCKRConf {
        tckr_net_version = 60002,
        tckr_node_service = NodeServiceType [ TCKR_NODE_NETWORK ],

        tckr_db_path = tucker_path </> "db",
        tckr_ks_block = "block",
        tckr_ks_tx = "tx",
        tckr_ks_chain = "chain",

        tckr_user_agent = "/Tucker:" ++ tucker_version ++ "/",

        tckr_wif_pref = 0x80,
        tckr_pub_pref = 0x00,
        tckr_magic_no = BSR.pack [ 0xf9, 0xbe, 0xb4, 0xd9 ],
        tckr_listen_port = 8333,

        tckr_genesis_raw = BS.pack $ (!! 0) $ unhex "0100000000000000000000000000000000000000000000000000000000000000000000003BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A29AB5F49FFFF001D1DAC2B7C0101000000010000000000000000000000000000000000000000000000000000000000000000FFFFFFFF4D04FFFF001D0104455468652054696D65732030332F4A616E2F32303039204368616E63656C6C6F72206F6E206272696E6B206F66207365636F6E64206261696C6F757420666F722062616E6B73FFFFFFFF0100F2052A01000000434104678AFDB0FE5548271967F1A67130B7105CD6A828E03909A67962E0EA1F61DEB649F6BC3F4CEF38C4F35504E51EC112DE5C384DF7BA0B8D578A4C702B6BF11D5FAC00000000",

        tckr_trans_timeout = 5, -- 5 sec
        tckr_bootstrap_host = [ "seed.tbtc.petertodd.org" ],

        tckr_seek_min = 16, -- if node_count < min_seek then seek for more nodes
        tckr_seek_max = 32, -- if node_count >= max_seek then stop seeking

        tckr_max_node = 128, -- max number of nodes in total
        
        tckr_node_blacklist = [
                ip4 (127, 0, 0, 1),
                ip4 (0, 0, 0, 0)
            ],
    
        tckr_gc_interval = 30 * 1000 * 1000, -- 30 sec
        
        tckr_max_block_task = 32,

        -- in sec
        tckr_node_alive_span = 90 * 60, -- 90 min
        tckr_reping_time = 60, -- 1 min

        tckr_known_inv_count = 8,

        tckr_block_tree_path = "test.block.d",
        tckr_max_block_per_chunk = 2048,
        tckr_max_tree_insert_depth = 256,

        tckr_fetch_dup_node = 8,
        tckr_fetch_dup_max_task = 4,

        tckr_max_block_future_diff = 60 * 2, -- 2 hours

        tckr_diff_change_span = 2016,
        tckr_expect_diff_change_time = 14 * 24 * 60 * 60, -- 2 weeks in sec

        tckr_use_special_min_diff = False

        -- the collector will wait until the top chunk
        -- has (tckr_max_block_per_chunk + tckr_max_tree_insert_depth)
        -- blocks and split & seal the previous (tckr_max_tree_insert_depth)
        -- chunk, while create a new chunk for tckr_max_tree_insert_depth
    }

    where
        ip4 = SockAddrInet 0 . tupleToHostAddress

tucker_default_conf_testnet3 = do
    conf <- tucker_default_conf_mainnet
    return $ conf {
        tckr_wif_pref = 0xef,
        tckr_pub_pref = 0x6f,
        tckr_magic_no = BSR.pack [ 0x0b, 0x11, 0x09, 0x07 ],
        tckr_listen_port = 18333,

        tckr_genesis_raw = BS.pack $ (!! 0) $ unhex "0100000000000000000000000000000000000000000000000000000000000000000000003BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4ADAE5494DFFFF001D1AA4AE180101000000010000000000000000000000000000000000000000000000000000000000000000FFFFFFFF4D04FFFF001D0104455468652054696D65732030332F4A616E2F32303039204368616E63656C6C6F72206F6E206272696E6B206F66207365636F6E64206261696C6F757420666F722062616E6B73FFFFFFFF0100F2052A01000000434104678AFDB0FE5548271967F1A67130B7105CD6A828E03909A67962E0EA1F61DEB649F6BC3F4CEF38C4F35504E51EC112DE5C384DF7BA0B8D578A4C702B6BF11D5FAC00000000",
    
        tckr_use_special_min_diff = True    
    }
