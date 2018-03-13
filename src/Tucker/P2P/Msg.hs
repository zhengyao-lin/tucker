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
-- return (msg, rest, received length)
tRecvOneMsg :: Transport -> Int -> ByteString
            -> IO (Either TCKRError MsgHead, ByteString, Int)
tRecvOneMsg trans timeout_s buf = do
    let wait = (\(_, stop, _, _) -> stop)
        timeout_us = timeout_s * 1000000

    (buf, _, res, recv_len) <-
        (flip $ iterateUntilM wait) (buf, False, Right (LackData 0), 0) $
            \(buf, _, _, recv_len) -> do
                res <- timeout timeout_us $ tRecvSome trans $ 1024 * 1024

                return $ case res of
                    Nothing -> (buf, True, Right (LackData 0), recv_len)
                    Just res -> do
                        let nbuf = BSR.append buf res
                            recv_inc = BSR.length res

                        case decodeLE nbuf of
                            (msg@(Right (MsgHead {})), rest) ->
                                -- whole message received
                                (rest, True, msg, recv_len + recv_inc)

                            (Right lack, _) ->
                                -- continue receiving
                                (nbuf, False, Right lack, recv_len + recv_inc)

                            (err@(Left _), _) ->
                                -- decoding error, return
                                (nbuf, True, err, recv_len + recv_inc)

    return (res, buf, recv_len)

tRecvOneMsgNonBlocking :: Transport -> ByteString
                       -> IO (Either TCKRError MsgHead, ByteString, Int)
tRecvOneMsgNonBlocking trans buf = do
    res <- tRecvNonBlocking trans $ 1024 * 1024

    let nbuf = BSR.append buf res
        recv_len = BSR.length res

    return $ case decodeLE nbuf of
        (msg@(Right (MsgHead {})), rest) ->
            (msg, rest, recv_len) -- whole message received

        (Right lack, _) ->
            (Right lack, nbuf, recv_len) -- continue receiving

        (err@(Left _), _) ->
            (err, nbuf, recv_len) -- decoding error, return

-- -- n for node
-- nodeSend :: MainLoopEnv -> Node -> ByteString -> IO Int
-- nodeSend env node = timeoutFailS (timeout_s env) $ tSend (conn_trans node)
