{-# LANGUAGE OverloadedStrings, GADTs #-}

module Tucker.RPC.Protocol where

import Data.Aeson
import Data.Aeson.Types

import Tucker.Conf

type RPCId = Value

data RPCErrorCode
    = RPCParseError
    | RPCInvalidRequest
    | RPCMethodNotFound
    | RPCInvalidParams
    | RPCInternalError
    deriving (Show)

errorCode :: RPCErrorCode -> Int
errorCode RPCParseError = -32700
errorCode RPCInvalidRequest = -32600
errorCode RPCMethodNotFound = -32601
errorCode RPCInvalidParams = -32602
errorCode RPCInternalError = -32603

data RPCRequest = RPCRequest RPCId RPCCall deriving (Show)
data RPCResponse r = RPCResponse RPCId (RPCResult r) deriving (Show)

data RPCResult r where
    RPCSuccess :: r -> RPCResult r
    RPCError :: RPCErrorCode -> String -> Maybe Value -> RPCResult Value

instance Show r => Show (RPCResult r) where
    show (RPCSuccess r) = "RPCSuccess " ++ show r
    show (RPCError code msg mobj) =
        "RPCError " ++ show code ++ " " ++ show msg ++ " " ++ show mobj

data RPCCall
    = RPCGetInfo
    | RPCSetTxFee Coin
    | RPCUnknown String
    deriving (Show)

parseRPCCall :: String -> Maybe Value -> Parser RPCCall
parseRPCCall "getinfo" _ = return RPCGetInfo

parseRPCCall "settxfee" (Just val) = do
    [amount] <- parseJSON val
    return (RPCSetTxFee amount)

parseRPCCall "settxfee" _ = fail "settxfee needs a parameter"

parseRPCCall method _ = return (RPCUnknown method)

instance FromJSON RPCRequest where
    parseJSON = withObject "RPCRequest" $ \v -> do
        method <- v .: "method"
        mparams <- v .:? "params"
        id <- v .: "id"

        call <- parseRPCCall method mparams

        return (RPCRequest id call)

instance ToJSON r => ToJSON (RPCResponse r) where
    toJSON (RPCResponse id (RPCSuccess r)) =
        object [ "result" .= r, "error" .= Null, "id" .= id ]
    
    toJSON (RPCResponse id (RPCError code msg mobj)) =
        object [
            "result" .= Null,
            "error" .= object [
                "code" .= errorCode code,
                "message" .= msg,
                "data" .= case mobj of
                    Just obj -> obj
                    Nothing -> Null
            ],
            
            "id" .= id
        ]

requestCall :: RPCRequest -> RPCCall
requestCall (RPCRequest _ call) = call

respondTo :: RPCRequest -> RPCResult r -> RPCResponse r
respondTo (RPCRequest id _) res =
    RPCResponse id res
