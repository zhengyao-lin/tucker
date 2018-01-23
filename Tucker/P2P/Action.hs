module Tucker.P2P.Action where

import qualified Data.ByteString as BSR

import Tucker.Std
import Tucker.Msg
import Tucker.Enc
import Tucker.Atom
import Tucker.Error
import Tucker.Transport

import Tucker.P2P.Msg
import Tucker.P2P.Node
import Tucker.P2P.Util

-- data CoroAction msg = CoroAction { doAction :: MainLoopEnv -> BTCNode -> msg -> IO [RouterAction] }

-- instance Funtor CoroAction where
--     f `fmap` c = 

recvM :: MsgPayload t => Command -> (t -> IO [RouterAction]) -> IO [RouterAction]
recvM cmd proc =
    return [ UpdateMe $ NormalAction handle ]
    where
        handle env node LackData = return []
        handle env node (MsgHead {
            command = command,
            payload = payload
        }) = do
            if command == cmd then
                decodePayload env node payload (pure []) proc
            else do
                nodeMsg env node "command not match, skipping"
                return []

fetchBlock :: MainLoopEnv -> BTCNode -> MsgHead -> IO [RouterAction]
fetchBlock env node msg = do
    let net = btc_network env
        trans = conn_trans node

    getblocks <- encodeMsg net BTC_CMD_GETBLOCKS $ encodeGetblocksPayload [] nullHash256
    timeoutRetryS (timeout_s env) $ tSend trans getblocks

    recvM BTC_CMD_INV $ \(InvPayload {
        inv_vect = inv_vect
    }) -> do
        nodeMsg env node $ "inv received: " ++ show inv_vect
        return [ DumpMe, StopProp ]
