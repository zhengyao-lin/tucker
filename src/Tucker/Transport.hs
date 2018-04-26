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

newtype Transport =
    Transport {
        handle :: Handle
    }

tFromSocket :: Socket -> IO Transport
tFromSocket sock = socketToHandle sock ReadWriteMode >>= pure . Transport

tFromHandle :: Handle -> IO Transport
tFromHandle = return . Transport

tSend :: Transport -> ByteString -> IO ()
tSend = BSR.hPut . handle

-- block until enough byte is received
tRecv :: Transport -> Int -> IO ByteString
tRecv = BSR.hGet . handle

tRecvSome :: Transport -> Int -> IO ByteString
tRecvSome = BSR.hGetSome . handle

tRecvNonBlocking :: Transport -> Int -> IO ByteString
tRecvNonBlocking = BSR.hGetNonBlocking . handle

tIsEnd :: Transport -> IO Bool
tIsEnd = hIsEOF . handle

tClose :: Transport -> IO ()
tClose = hClose . handle
