module Tucker.P2P.Node where

import qualified Data.ByteString as BSR

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

-- an environment shared among a main loop
data MainLoopEnv =
    MainLoopEnv {
        btc_network :: BTCNetwork,
        timeout_s   :: Int, -- timeout in sec
        node_list   :: Atom [BTCNode],
        io_lock     :: LK.Lock,
        gc_interv   :: Integer, -- in ms
    
        global_conf :: TCKRConf    
    }

data RouterAction
    = StopProp -- stop the propagation of the message
    | DumpMe -- dump the current handler
    | UpdateMe NodeAction -- update to a new action

data NodeAction
    = NormalAction { handler :: MainLoopEnv -> BTCNode -> MsgHead -> IO [RouterAction] }
    | NoAction

data BTCNode =
    BTCNode {
        conn_trans     :: Transport,
        addr           :: AddrInfo,
        incoming       :: Bool,

        vers_payload   :: Atom VersionPayload,

        recv_buf       :: Atom ByteString,
        msg_list       :: Atom [MsgHead], -- prepend

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
    LK.acquire (io_lock env)
    putStrLn $ "env: " ++ msg
    LK.release (io_lock env)
    return ()

initEnv :: BTCNetwork -> TCKRConf -> IO MainLoopEnv
initEnv net conf = do
    node_list <- newA []
    lock <- LK.new

    return $ MainLoopEnv {
        btc_network = net,
        timeout_s = tckr_trans_timeout conf,
        node_list = node_list,
        io_lock = lock,
        gc_interv = tckr_gc_interval conf,

        global_conf = conf
    }

getEnvConf :: MainLoopEnv -> (TCKRConf -> t) -> IO t
getEnvConf env field = do
    return $ field $ global_conf env

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
