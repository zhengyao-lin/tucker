module Tucker.P2P.Msg where

import Control.Monad.Loops
import qualified Data.ByteString as BSR

import System.Timeout

import Tucker.Msg
import Tucker.Enc
import Tucker.Error
import Tucker.Transport

-- receive one full message from buffer
-- timeout in sec
-- return (msg, rest)
recvOneMsg :: Transport -> Int -> ByteString -> IO (Either TCKRError MsgHead, ByteString)
recvOneMsg trans timeout_s buf = do
    let wait = (\(_, stop, _) -> stop)
        timeout_ms = timeout_s * 1000000

    (buf, _, res) <- (flip $ iterateUntilM wait) (buf, False, Right LackData) $
        \(buf, _, _) -> do
            res <- timeout timeout_ms $ tRecvSome trans 1024

            return $ do
                case res of
                    Nothing -> (buf, True, Right LackData)
                    Just res -> do
                        let nbuf = BSR.append buf res

                        case decodeLE nbuf of
                            (msg@(Right (MsgHead {})), rest) ->
                                (rest, True, msg) -- whole message received

                            (Right LackData, _) ->
                                (nbuf, False, Right LackData) -- continue receiving

                            (err@(Left _), _) ->
                                (nbuf, True, err) -- decoding error, return

    return (res, buf)
