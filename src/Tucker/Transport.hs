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

data Transport = Transport Bool Handle

getHandle (Transport _ handle) = handle
isInbound (Transport inbound _) = inbound

tFromSocket :: Socket -> Bool -> IO Transport
tFromSocket sock inbound = Transport inbound <$> socketToHandle sock ReadWriteMode

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
