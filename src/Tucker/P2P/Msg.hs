module Tucker.P2P.Msg where

import Control.Monad.Loops
import qualified Data.ByteString as BSR

import Network.Socket
import System.Timeout

import Control.Exception
import Control.Monad.Loops

import Tucker.Msg
import Tucker.Enc
import Tucker.Atom
import Tucker.Conf
import Tucker.Util
import Tucker.Error
import Tucker.Transport

import Tucker.P2P.Node
import Tucker.P2P.Util

filterProbingList :: MainLoopEnv -> [AddrInfo] -> IO [AddrInfo]
filterProbingList env new_list = do
    nodes <- getA $ node_list env

    let bl = envConf env tckr_node_blacklist
        exist_sockaddr = map sock_addr nodes
        blacklist = exist_sockaddr ++ bl

    return $ filter (\a -> all (not . isSameIP (addrAddress a)) blacklist) new_list

decodePayload :: MsgPayload t
              => MainLoopEnv
              -> Node
              -> ByteString
              -> IO a
              -> (t -> IO a)
              -> IO a

decodePayload env node payload fail proc = do
    case decodeAllLE payload of
        Left err -> do
            nodeMsg env node $ "payload decoding error: " ++ (show err)
            fail

        Right v -> proc v


nodeRecvOneMsg :: MainLoopEnv
               -> Node
               -> (Transport -> ByteString -> IO (Either TCKRError MsgHead, ByteString, Int))
               -> IO ()
               -> IO MsgHead
nodeRecvOneMsg env node recv_proc timeout_proc = do
    trans_state <- nodeTransState node

    let orig_buf = recv_buf trans_state
    
    (msg, buf, len) <- recv_proc (conn_trans node) orig_buf

    now <- unixTimestamp

    if len /= 0 then do
        -- at least received some data
        nodeChangeTransState node (\ts -> ts { last_seen = now })
        -- setA (cur_progress node) (Progress (fi $ BSR.length buf) (-1))
    else return ()

    let test_span = fi (envConf env tckr_speed_test_span)
        time_elapsed = now - speed_test_begin trans_state
        downloaded = total_download trans_state + fi len
        speed = fi (downloaded `div` fi time_elapsed)

    -- update max download speed
    if time_elapsed >= test_span then
        nodeChangeTransState node (\ts -> ts {
            total_download = 0,
            max_download_speed = max speed (max_download_speed ts),
            speed_test_begin = now -- reset begin time
        })
    else
        nodeChangeTransState node (\ts -> ts {
            total_download = downloaded -- update download size
        })

    case msg of
        Right msg@(MsgHead {}) ->
            if magicno msg /= envConf env tckr_magic_no then
                throw (TCKRError "network magic number not match")
            else
                updateBuf buf >> return msg

        Right (LackData total) -> do
            updateBuf buf
            timeout_proc

            nodeChangeTransState node $ \ts ->
                ts { cur_progress = Progress (fi (BSR.length buf)) (fi total) }

            return (LackData total)

        Left err -> throw err

    where
        updateBuf buf = nodeChangeTransState node $ \ts ->
            ts { recv_buf = buf }

-- nodeRecvOneMsgTimeout :: MainLoopEnv -> Node -> (MsgHead -> IO ()) -> IO MsgHead
-- nodeRecvOneMsgTimeout env node timeout_proc = do
--     nodeRecvOneMsg env node (`tRecvOneMsg` timeout_s env) timeout_proc

nodeRecvOneMsgNonBlocking :: MainLoopEnv -> Node -> IO MsgHead
nodeRecvOneMsgNonBlocking env node = do
    nodeRecvOneMsg env node tRecvOneMsgNonBlocking (return ())

-- throw an exception when timeout
nodeExpectOneMsg :: MainLoopEnv -> Node -> IO MsgHead
nodeExpectOneMsg env node =
    nodeRecvOneMsg env node (`tRecvOneMsg` timeout_s env)
        (throw (TCKRError "recv timeout"))

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
