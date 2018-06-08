{-# LANGUAGE OverloadedStrings #-}

module Tucker.RPC.Server where

-- import Control.Monad

-- import Data.Aeson.Types
-- import qualified Data.Text as TXT

-- import Network.JsonRpc

-- import Tucker.Conf

-- rpc_version = V2

-- data RPCRequest
--     = RPCGetInfo

-- instance FromRequest RPCRequest where
--     parseParams "getinfo" = Just (\param -> return RPCGetInfo)
--     parseParams _ = Nothing

-- toServerSettings conf =
--     serverSettings (tckr_rpc_server_port conf) (tckr_rpc_server_addr conf)

-- serverRPC :: TCKRConf -> MainLoopEnv -> IO ()
-- serverRPC conf env =
--     jsonRpcTcpServer rpc_version False (toServerSettings conf) $
--         forever $ do
--             mreq <- receiveRequest

--             case mreq of
--                 Nothing -> return ()
--                 Just req ->
--                     case fromRequest req of
--                         Left err -> tLnM ("rpc error: " ++ show err)
--                         Right req -> procRequest conf env req

-- procRequest :: TCKRConf -> MainLoopEnv -> RPCRequest -> IO ()
-- procRequest RPCGetInfo = error "not implemented"
