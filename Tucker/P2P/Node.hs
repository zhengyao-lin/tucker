module Tucker.P2P.Node where

import Data.Word
import qualified Data.Set as SET
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.Lock as LK

import Network.Socket

import Tucker.Std
import Tucker.Enc
import Tucker.Msg
import Tucker.Conf
import Tucker.Atom
import Tucker.Transport

import Tucker.Chain.Object

-- an environment shared among a main loop
data MainLoopEnv =
    MainLoopEnv {
        btc_network   :: BTCNetwork,
        timeout_s     :: Int, -- timeout in sec
        node_list     :: Atom [BTCNode],

        gc_interv     :: Integer, -- in ms
    
        global_conf   :: TCKRConf,

        fetched_block :: Atom (SET.Set Hash256),

        io_lock       :: LK.Lock,
        io_buf        :: Atom [String],

        tree_lock     :: LK.Lock,
        block_tree    :: Atom BlockTree,
        idle_block    :: Atom (SET.Set BlockPayloadHashed)
    }

data RouterAction
    = StopProp -- stop the propagation of the message
    | DumpMe -- dump the current handler
    | UpdateMe NodeAction -- update to a new action

instance Eq RouterAction where
    StopProp == StopProp = True
    DumpMe   == DumpMe   = True
    _        == _        = False

data NodeAction
    = NormalAction { handler :: ActionHandle }
    | NoAction

type ActionHandle = MainLoopEnv -> BTCNode -> MsgHead -> IO [RouterAction]

data BTCNode =
    BTCNode {
        conn_trans     :: Transport,
        addr           :: AddrInfo,
        incoming       :: Bool,

        vers_payload   :: Atom VersionPayload,

        recv_buf       :: Atom ByteString,
        msg_list       :: Atom [MsgHead], -- prepend
        last_seen      :: Atom Word64,

        thread_id      :: Atom ThreadId,

        action_list    :: Atom [NodeAction],
        new_action     :: Atom [NodeAction],

        alive          :: Atom Bool
    }

instance Show BTCNode where
    show node =
        "node on " ++ (show $ addrAddress $ addr node)

envMsg :: MainLoopEnv -> String -> IO ()
envMsg env msg = do
    -- force eval
    let msg' = BS.pack ("env: " ++ msg)

    LK.acquire (io_lock env)
    BS.putStrLn msg'
    LK.release (io_lock env)

    -- appA (++ [ "env: " ++ msg ]) (io_buf env)
    -- putStrLn' $ "env: " ++ msg

initEnv :: BTCNetwork -> TCKRConf -> IO MainLoopEnv
initEnv net conf = do
    node_list <- newA []
    io_lock <- LK.new
    io_buf <- newA []
    fetched <- newA SET.empty
    tree_lock <- LK.new
    block_tree <- newA emptyTree
    idle_block <- newA SET.empty

    let genesis@(BlockPayload {
            header = header
        }) = case decodeLE (genesisRaw net) of
            (Left err, _) ->
                error $ "fatal: genesis block decoding error: " ++ show err
            (Right gen, _) -> gen

        bph = BlockPayloadHashed (hashBlockHeader header) genesis

    case insertToTree emptyTree bph of
        Left err ->
            error $ "fatal: illegal genesis block: " ++ show err
        
        Right new_tree ->
            setA block_tree new_tree

    return $ MainLoopEnv {
        btc_network = net,
        timeout_s = tckr_trans_timeout conf,
        node_list = node_list,
        gc_interv = tckr_gc_interval conf,

        global_conf = conf,

        io_lock = io_lock,
        io_buf = io_buf,

        fetched_block = fetched,
        tree_lock = tree_lock,
        block_tree = block_tree,
        idle_block = idle_block
    }

getEnvConf :: MainLoopEnv -> (TCKRConf -> t) -> IO t
getEnvConf env field = do
    return $ field $ global_conf env

envAddFetchedBlock :: MainLoopEnv -> Hash256 -> IO ()
envAddFetchedBlock env hash = do
    appA (SET.insert hash) (fetched_block env)

insertAction :: MainLoopEnv -> NodeAction -> IO ()
insertAction env action = do
    nodes <- getA $ node_list env

    forM nodes $ \node -> do
        alive <- getA $ alive node
        if alive then
            appA (action:) (new_action node)
        else
            return ()

    return ()

nodeMsg :: MainLoopEnv -> BTCNode -> String -> IO ()
nodeMsg env node msg = do
    envMsg env $ (show node) ++ ": " ++ msg
    return ()

nodeLastSeen :: BTCNode -> IO Word64
nodeLastSeen = getA . last_seen
