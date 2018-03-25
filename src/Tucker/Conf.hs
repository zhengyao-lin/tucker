module Tucker.Conf where

import Data.Hex
import Data.Int
import Data.Word
import Data.Char

import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import System.FilePath
import System.Directory

import Network.Socket
import Crypto.PubKey.ECC.Types

tucker_curve = getCurveByName SEC_p256k1

data NodeServiceTypeSingle
    = TCKR_NODE_NETWORK
    | TCKR_NODE_GETUTXO
    | TCKR_NODE_BLOOM
    | TCKR_NODE_WITNESS deriving (Show, Eq)

data NodeServiceType = NodeServiceType [NodeServiceTypeSingle] deriving (Show, Eq)

type Timestamp = Word32

type SoftForkId = Int32

soft_fork_id_range = [ 0 .. 28 ] :: [SoftForkId] -- only the lowest 29 bits are used(see BIP 9)

data SoftForkStatus
    = FORK_STATUS_UNDEFINED -- should not occur in the database
    | FORK_STATUS_DEFINED
    | FORK_STATUS_STARTED
    | FORK_STATUS_LOCKED_IN

    -- final status
    | FORK_STATUS_FAILED
    | FORK_STATUS_ACTIVE
    deriving (Eq, Show)

isActiveStatus :: SoftForkStatus -> Bool
isActiveStatus status =
    status == FORK_STATUS_STARTED ||
    status == FORK_STATUS_LOCKED_IN ||
    status == FORK_STATUS_ACTIVE

data SoftFork =
    SoftFork {
        fork_name    :: String,
        fork_bit     :: SoftForkId,
        fork_start   :: Timestamp,
        fork_timeout :: Timestamp,
        fork_status  :: SoftForkStatus
    } deriving (Show)

instance Eq SoftFork where
    f1 == f2 = fork_name f1 == fork_name f2 && fork_bit f1 == fork_bit f2

data TCKRConf =
    TCKRConf {
        tckr_net_version     :: Integer,
        tckr_node_service    :: NodeServiceType,

        -- keyspaces: block, tx, 
        tckr_block_db_path   :: FilePath,
        tckr_tx_db_path      :: FilePath,
        tckr_bucket_block_name :: String,
        tckr_bucket_chain_name :: String,
        tckr_bucket_tx_name    :: String,
        tckr_bucket_utxo_name  :: String,
        tckr_bucket_fork_name  :: String,
        tckr_bucket_stat_name  :: String,

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

        tckr_speed_test_span :: Word, -- time span of a download speed test in seconds

        tckr_gc_interval     :: Integer,

        tckr_max_block_task  :: Int,

        -- do the input checking in parrallel
        -- when nIn is greater or equal to this number
        tckr_min_parallel_input_check :: Int,

        tckr_node_alive_span :: Timestamp,
        tckr_reping_time     :: Timestamp,

        tckr_known_inv_count :: Int,
        -- max number of hashes to send when trying to sync witht the network
        
        tckr_initial_fee :: Integer,
        tckr_fee_half_rate :: Integer,

        tckr_max_tree_insert_depth :: Int, -- max search depth when inserting a block

        tckr_fetch_dup_node  :: Int,
        -- number of duplicated nodes to fetch the same bunch of block
        tckr_fetch_dup_max_task :: Int,
        -- if the number of block fetch task is less than this number,
        -- use dup_node

        -- max difference of the timestamp of a block with the time received
        tckr_max_block_future_diff :: Word32, -- in sec

        -- the difficulty changes every tckr_retarget_span blocks
        tckr_retarget_span :: Word32,
        tckr_expect_retarget_time :: Word32, -- expected difficulty change time in sec
        tckr_soft_fork_lock_threshold :: Word32, -- roughly 95% of retarget span

        tckr_use_special_min_diff :: Bool, -- support special-min-difficulty or not(mainly on testnet)

        tckr_block_fetch_timeout :: Int, -- in sec
        tckr_node_max_blacklist_count :: Int,

        tckr_max_block_batch :: Int,

        tckr_sync_inv_timeout :: Int, -- in sec

        tckr_coinbase_maturity :: Int,

        tckr_p2sh_enable_time :: Timestamp,
        tckr_dup_tx_disable_time :: Timestamp,
        tckr_mtp_number :: Int,

        tckr_soft_forks :: [SoftFork],

        -- different from the assumevalid in bitcoin core
        -- because currently tucker does not support header-first
        -- sync and can not check whether a block is the ancestor
        -- of the assumevalid block
        tckr_block_assumed_valid :: Maybe (Word, String),

        tckr_enable_difficulty_check :: Bool,
        tckr_enable_mtp_check :: Bool
    } deriving (Show)

tucker_version = "0.0.1"

tucker_btp_name_regex = "^btp.(0|[1-9][0-9]*)$"
tucker_btp_name_gen num = "btp." ++ show num

tucker_bdiff_diff1 = 0x00000000ffff0000000000000000000000000000000000000000000000000000
tucker_pdiff_diff1 = 0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff

hex2bs = BS.pack . (!! 0) . unhex . map toUpper

-- tucker_cache_tree_chunk = 1

tucker_default_conf_mainnet mpath = do
    user_home <- getHomeDirectory

    let tucker_path = maybe (user_home </> ".tucker") id mpath

    createDirectoryIfMissing False tucker_path

    return $ TCKRConf {
        tckr_net_version = 60002,
        tckr_node_service = NodeServiceType [ TCKR_NODE_NETWORK ],

        tckr_block_db_path = tucker_path </> "db" </> "chain",
        tckr_tx_db_path = tucker_path </> "db" </> "tx",
        tckr_bucket_block_name = "block",
        tckr_bucket_chain_name = "chain",
        tckr_bucket_tx_name = "tx",
        tckr_bucket_utxo_name = "utxo",
        tckr_bucket_fork_name = "fork",
        tckr_bucket_stat_name = "stat",

        tckr_user_agent = "/Tucker:" ++ tucker_version ++ "/",

        tckr_wif_pref = 0x80,
        tckr_pub_pref = 0x00,
        tckr_magic_no = BSR.pack [ 0xf9, 0xbe, 0xb4, 0xd9 ],
        tckr_listen_port = 8333,

        tckr_genesis_raw = hex2bs "0100000000000000000000000000000000000000000000000000000000000000000000003BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A29AB5F49FFFF001D1DAC2B7C0101000000010000000000000000000000000000000000000000000000000000000000000000FFFFFFFF4D04FFFF001D0104455468652054696D65732030332F4A616E2F32303039204368616E63656C6C6F72206F6E206272696E6B206F66207365636F6E64206261696C6F757420666F722062616E6B73FFFFFFFF0100F2052A01000000434104678AFDB0FE5548271967F1A67130B7105CD6A828E03909A67962E0EA1F61DEB649F6BC3F4CEF38C4F35504E51EC112DE5C384DF7BA0B8D578A4C702B6BF11D5FAC00000000",

        tckr_trans_timeout = 5, -- 5 sec
        tckr_bootstrap_host = [ "seed.btc.petertodd.org" ],

        tckr_seek_min = 16, -- if node_count < min_seek then seek for more nodes
        tckr_seek_max = 32, -- if node_count >= max_seek then stop seeking

        tckr_max_node = 48, -- max number of nodes in total
        
        tckr_node_blacklist = [
                ip4 (127, 0, 0, 1),
                ip4 (0, 0, 0, 0)
            ],
    
        tckr_speed_test_span = 5,

        tckr_gc_interval = 20 * 1000 * 1000, -- 20 sec
        
        tckr_max_block_task = 20,
        tckr_min_parallel_input_check = 256, -- maxBound,

        -- in sec
        tckr_node_alive_span = 90 * 60, -- 90 min
        tckr_reping_time = 30, -- 30sec

        tckr_known_inv_count = 8,

        tckr_max_tree_insert_depth = 64,

        tckr_max_block_batch = 500,
        -- receive 200 blocks a time(if inv is greater than that, trim the tail)

        tckr_fetch_dup_node = 8,
        tckr_fetch_dup_max_task = 4,

        tckr_max_block_future_diff = 60 * 2, -- 2 hours

        tckr_retarget_span = 2016,
        tckr_expect_retarget_time = 14 * 24 * 60 * 60, -- 2 weeks in sec
        tckr_soft_fork_lock_threshold = 1916,

        tckr_use_special_min_diff = False,

        tckr_block_fetch_timeout = 10,

        tckr_node_max_blacklist_count = 5,

        tckr_sync_inv_timeout = 3,

        tckr_coinbase_maturity = 100,

        tckr_p2sh_enable_time = 1333238400,
        tckr_dup_tx_disable_time = 1331769600,

        tckr_mtp_number = 11,

        tckr_initial_fee = 50 * 100000000,
        tckr_fee_half_rate = 210000,

        tckr_soft_forks = [
            SoftFork {
                fork_name = "csv",
                fork_bit = 0,
                fork_start = 1462032000,
                fork_timeout = 1493568000,
                fork_status = FORK_STATUS_DEFINED
            },
            
            SoftFork {
                fork_name = "segwit",
                fork_bit = 1,
                fork_start = 1479139200,
                fork_timeout = 1510675200,
                fork_status = FORK_STATUS_DEFINED
            }
        ],

        tckr_block_assumed_valid = Nothing,

        tckr_enable_difficulty_check = False,
        tckr_enable_mtp_check = False
    }

    where
        ip4 = SockAddrInet 0 . tupleToHostAddress

tucker_default_conf_testnet3 mpath = do
    conf <- tucker_default_conf_mainnet mpath
    return $ conf {
        tckr_wif_pref = 0xef,
        tckr_pub_pref = 0x6f,
        tckr_magic_no = BSR.pack [ 0x0b, 0x11, 0x09, 0x07 ],
        tckr_listen_port = 18333,

        tckr_bootstrap_host = [ "testnet-seed.bitcoin.jonasschnelli.ch", "seed.tbtc.petertodd.org" ],

        tckr_genesis_raw = hex2bs "0100000000000000000000000000000000000000000000000000000000000000000000003BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4ADAE5494DFFFF001D1AA4AE180101000000010000000000000000000000000000000000000000000000000000000000000000FFFFFFFF4D04FFFF001D0104455468652054696D65732030332F4A616E2F32303039204368616E63656C6C6F72206F6E206272696E6B206F66207365636F6E64206261696C6F757420666F722062616E6B73FFFFFFFF0100F2052A01000000434104678AFDB0FE5548271967F1A67130B7105CD6A828E03909A67962E0EA1F61DEB649F6BC3F4CEF38C4F35504E51EC112DE5C384DF7BA0B8D578A4C702B6BF11D5FAC00000000",
    
        tckr_use_special_min_diff = True,

        tckr_p2sh_enable_time = 1329264000,
        tckr_dup_tx_disable_time = 1329696000,

        tckr_soft_forks = [
            SoftFork {
                fork_name = "csv",
                fork_bit = 0,
                fork_start = 1456761600,
                fork_timeout = 1493568000,
                fork_status = FORK_STATUS_DEFINED
            },
            
            SoftFork {
                fork_name = "segwit",
                fork_bit = 1,
                fork_start = 1462032000,
                fork_timeout = 1493568000,
                fork_status = FORK_STATUS_DEFINED
            }
        ],

        tckr_block_assumed_valid = -- Nothing
            -- Just (300000, "000000000000226f7618566e70a2b5e020e29579b46743f05348427239bf41a1")
            -- Just (600000, "000000000000624f06c69d3a9fe8d25e0a9030569128d63ad1b704bbb3059a16")
            Just (700000, "000000000000406178b12a4dea3b27e13b3c4fe4510994fd667d7c1e6a3f4dc1 ")
    }
