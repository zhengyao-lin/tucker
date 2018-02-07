{-# LANGUAGE DuplicateRecordFields #-}

module Tucker.P2P.Action where

import Data.List
import qualified Data.Set as SET
import qualified Data.ByteString as BSR

import Control.Monad
import qualified Control.Concurrent.Lock as LK

import System.Random

import Tucker.Msg
import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Atom
import Tucker.Error
import Tucker.Transport

import Tucker.P2P.Msg
import Tucker.P2P.Node
import Tucker.P2P.Util

import Tucker.Chain.Object

-- data CoroAction msg = CoroAction { doAction :: MainLoopEnv -> Node -> msg -> IO [RouterAction] }

-- instance Funtor CoroAction where
--     f `fmap` c = 

-- 172.104.120.91

recvM :: MsgPayload t => [RouterAction] -> Command -> (t -> IO [RouterAction]) -> IO [RouterAction]
recvM r_act cmd proc =
    return $ r_act ++ [ UpdateMe $ NormalAction handle ]
    where
        handle env node LackData = return []
        handle env node (MsgHead {
            command = command,
            payload = payload
        }) = do
            if command == cmd then
                decodePayload env node payload (do
                    nodeMsg env node $ "decode failed on command " ++ (show command)
                    return []) proc
            else do
                -- nodeMsg env node $ "command not match, skipping(" ++ (show command) ++ ")"
                return []

pingDelay :: MainLoopEnv -> Node -> MsgHead -> IO [RouterAction]
pingDelay env node msg = do
    let conf = global_conf env
        trans = conn_trans node

    nonce <- randomIO
    ping <- encodeMsg conf BTC_CMD_PING $ pure $ encodeLE (PingPongPayload nonce)

    -- setA (ping_delay node) maxBound -- set a maximum in case the node doesn't reply
    start <- msCPUTime
    timeoutRetryS (timeout_s env) $ tSend trans ping

    recvM [] BTC_CMD_PONG $ \(PingPongPayload back_nonce) -> do
        if back_nonce == nonce then do
            end <- msCPUTime
            setA (ping_delay node) (end - start)
            return [ StopProp, DumpMe ]
        else
            return [] -- skip