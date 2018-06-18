module Tucker.RPC.HTTP where

import qualified Data.ByteString as BSR

import Control.Monad

import Network.HTTP
import Network.Socket

import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.Thread

import Tucker.P2P.Util

mkResponse :: Int -> String -> [Header] -> ByteString -> Response ByteString
mkResponse code reason headers body =
    let tuple = (
                code `div` 100 `mod` 10,
                code `div` 10 `mod` 10,
                code  `mod` 10
            )

    in Response tuple reason (headers ++ [
        mkHeader HdrContentLength (show (BSR.length body))
    ]) body

serverHTTP :: HStream r => ThreadState -> String -> Int -> (Request r -> IO (Response r)) -> IO ()
serverHTTP tstate host port handler = do
    let loop sock = void $ do
            (conn, _) <- accept sock

            forkCap tstate THREAD_RPC $ do
                stream <- socketConnection host port conn
                res <- receiveHTTP stream

                case res of
                    Right req -> handler req >>= respondHTTP stream
                    Left err -> tLnM ("http error: " ++ show err)

            loop sock

    listenOn host port >>= loop
