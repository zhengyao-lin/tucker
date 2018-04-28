module Tucker.Transport where

import qualified Data.ByteString as BSR

import System.IO
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString

import Tucker.Enc

-- tFromSocket
-- tFromHandle
-- tSend
-- tRecv
-- tSendNonBlocking
-- tRecvNonBlocking
-- tClose

newtype Transport = Transport Handle

getHandle (Transport handle) = handle

tFromSocket :: Socket -> IO Transport
tFromSocket sock = socketToHandle sock ReadWriteMode >>= pure . Transport

tFromHandle :: Handle -> IO Transport
tFromHandle = return . Transport

tSend :: Transport -> ByteString -> IO ()
tSend = BSR.hPut . getHandle

-- block until enough byte is received
tRecv :: Transport -> Int -> IO ByteString
tRecv = BSR.hGet . getHandle

tRecvSome :: Transport -> Int -> IO ByteString
tRecvSome = BSR.hGetSome . getHandle

tRecvNonBlocking :: Transport -> Int -> IO ByteString
tRecvNonBlocking = BSR.hGetNonBlocking . getHandle

tIsEnd :: Transport -> IO Bool
tIsEnd = hIsEOF . getHandle

tClose :: Transport -> IO ()
tClose = hClose . getHandle
