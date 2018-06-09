module Tucker.RPC.Server where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBSR
import qualified Data.ByteString.Base64 as B64

import Network.HTTP
import Network.HTTP.Auth

import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.Thread

import Tucker.P2P.Node

import Tucker.RPC.HTTP
import Tucker.RPC.Protocol

encodeStrict :: JSON.ToJSON r => r -> ByteString
encodeStrict = LBSR.toStrict . JSON.encode

procRequest :: TCKRConf -> MainLoopEnv -> ByteString -> IO ByteString
procRequest conf env body =
    case JSON.eitherDecodeStrict body of
        Left err ->
            return $ encodeStrict $
            RPCResponse JSON.Null (RPCError RPCParseError err Nothing)
        
        Right req ->
            execCall conf env req (requestCall req)

mkJSONResponse :: ByteString -> Response ByteString
mkJSONResponse dat =
    mkResponse 200 "OK" [
        mkHeader HdrContentType "application/x-json"
    ] dat

serverRPC :: TCKRConf -> MainLoopEnv -> IO ()
serverRPC conf env = do
    tstate <- initThread conf
    serverHTTP tstate (tckr_rpc_server_addr conf) (fi (tckr_rpc_server_port conf)) handler
    where
        -- tstate = envThreadState env

        unauth =
            mkResponse 401 "Unauthorized"
                       [ mkHeader HdrWWWAuthenticate "Basic realm=\"jsonrpc\"" ] mempty

        badreq = mkResponse 400 "Bad Request" [] mempty

        proc req =
            mkJSONResponse <$>
            procRequest conf env (rqBody req)

        -- a simple, specialized handler for bitcoin rpc purpose
        handler :: Request ByteString -> IO (Response ByteString)
        handler req =
            -- checking required basic access authorization
            case findHeader HdrAuthorization req of
                Nothing -> return unauth
                
                Just ('B':'a':'s':'i':'c':' ':value) ->
                    -- tLnM ("auth: " ++ value)

                    case B64.decode (BS.pack value) of
                        Left err -> return badreq
                        Right bs -> do
                            let str = BS.unpack bs
                                (user, pass) = splitOn ':' str
                            
                            if user == tckr_rpc_user_name conf &&
                               pass == tckr_rpc_user_pass conf then
                                proc req
                            else
                                return unauth

                _ -> return badreq

-- request handlers

suc :: JSON.ToJSON r => RPCRequest -> r -> IO ByteString
suc req r = return $
    encodeStrict (respondTo req (RPCSuccess r))

err :: RPCRequest -> RPCErrorCode -> String -> IO ByteString
err req code msg = return $
    encodeStrict (respondTo req (RPCError code msg Nothing))

execCall :: TCKRConf -> MainLoopEnv -> RPCRequest -> RPCCall -> IO ByteString
execCall conf env req RPCGetInfo = suc req "hello"
execCall conf env req (RPCUnknown method) =
    err req RPCMethodNotFound ("method " ++ show method ++ " does not exists")

{-

simpleHTTP (postRequestWithBody "http://tucker:sonia@127.0.0.1:3150/" "application/x-json" "{\"method\":\"getinfo\",\"id\":10086}")

-}
