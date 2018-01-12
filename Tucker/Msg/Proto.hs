module Tucker.Msg.Proto where

import Data.Int
import Data.Word

import qualified Data.ByteString as BSR

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Network.Socket.Options

import System.Timeout

import Debug.Trace

import Control.Monad
import Control.Monad.Loops

import Tucker.Enc
import Tucker.Std
import Tucker.Error

import Tucker.Msg.Tx
import Tucker.Msg.Common
import Tucker.Msg.Other

ip42addr :: String -> Word16 -> IO AddrInfo
ip42addr ip port =
    (getAddrInfo (Just defaultHints {
        addrFamily = AF_INET,
        addrSocketType = Stream

        -- addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
    }) (Just ip) (Just $ show port)) >>= (pure . head)

-- AF_INET SOCK_STREAM (addrSocketType addr) 

buildSocketTo :: AddrInfo -> IO Socket
buildSocketTo addr =
    socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

-- look up node seeds using a domain
seedLookup :: BTCNetwork -> String -> IO [AddrInfo]
seedLookup net host =
    getAddrInfo (Just defaultHints {
        addrFamily = AF_INET,
        addrSocketType = Stream
    }) (Just host) (Just $ show $ listenPort net)

-- try to trim a message from the received data
-- if a complete message is found, return (Right MsgHead {}, rest of data)
-- if the message is incomplete, return (Right LackData, original data)
-- if the message if incorrect, return (Left err, original data)
-- trimMsg :: ByteString -> (Either TCKRError MsgHead, ByteString)
-- trimMsg = decodeLE

-- receive one full message from buffer
-- timeout in sec
-- return (msg, rest)
recvOneMsg :: Socket -> ByteString -> Int -> IO (Either TCKRError MsgHead, ByteString)
recvOneMsg sock buf timeout_ms = do
    let wait = (\(_, stop, _) -> stop)

    (buf, _, res) <- (flip $ iterateUntilM wait) (buf, False, Right LackData) $
        \(buf, _, _) -> do
            res <- timeout timeout_ms $ recv sock 1024

            return $ case res of
                Nothing -> (buf, True, Left $ TCKRError "socket timeout")
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

data BTCNode =
    BTCNode {
        btc_net   :: BTCNetwork,
        -- conn_sock :: Socket,
        addr      :: AddrInfo
    } deriving (Show, Eq)

-- return alive address
-- timeout in seconds
startProbing :: BTCNetwork -> Int -> [String] -> IO [BTCNode]
startProbing net timeout_s hostnames = do
    addrs <- (mapM (seedLookup net) hostnames) >>= (pure . concat)
    
    let timeout_ms = timeout_s * 1000000

    addrs <- (flip filterM) addrs $ \addr -> do

        putStr ("probing " ++ (show $ addrAddress addr) ++ " -> ")

        sock <- buildSocketTo addr

        -- print (isSupportedSocketOption RecvTimeOut)
        -- setSocketTimeouts sock timeout_ms timeout_ms
        -- setSocketOption sock RecvTimeOut 1

        res <- timeout timeout_ms $ connect sock (addrAddress addr)

        case res of
            Nothing -> do
                putStrLn "connection timeout"
                return False

            Just _ -> do
                msg <- encodeMsg net BTC_CMD_VERSION $ encodeVersionPayload net

                send sock msg

                -- putStrLn (show dat)
                let buf = BSR.pack []
                (vers_msg, buf) <- recvOneMsg sock buf timeout_ms
                (vers_ack, buf) <- recvOneMsg sock buf timeout_ms

                close sock

                case vers_ack of
                    Right (MsgHead {
                        command = BTC_CMD_VERACK
                    }) -> do
                        putStrLn "success"
                        return True

                    Left err -> do
                        putStrLn (show err)
                        return False

                    _ -> error "impossible"

    return $ map (\addr -> BTCNode { btc_net = net, addr = addr }) addrs
    
{-

need to check bitseed.xf2.org
testnet-seed.bluematt.me
use this !!! seed.tbtc.petertodd.org

99.242.230.163
76.111.96.126
130.235.100.241
198.251.83.19
138.68.229.19

addr <- ip42addr "seed.tbtc.petertodd.org" 18333
sock <- buildSocketTo addr
connect sock (addrAddress addr)
msg <- encodeMsg btc_testnet3 BTC_CMD_VERSION $ encodeVersionPayload btc_testnet3
send sock msg
recv sock 1024
recv sock 1024
msg <- encodeMsg btc_testnet3 BTC_CMD_TX $ encodeTxPayload btc_testnet3 "933qtT8Ct7rGh29Eyb5gG69QrWmwGein85F1kuoShaGjJFFBSjk" [ OutPoint (((!! 0) . unhex) "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5") 0 ] [ (10000, "miro9ZNPjcLnqvnJpSm8P6CUf1WPU98jET"), (119990000, "mvU2ysD322amhCeCPMhPc3L7hKDGGWSBz7") ]
send sock msg
recv sock 1024

tx msg
"\v\DC1\t\atx\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\NUL#.`\247\SOH\NUL\NUL\NUL\SOH\181\EOT\CAN\187\234$\SYN\180\201\206\213-\206\133\255\202{Hfh\189i\187\199\195A\STX\225/\130\183\190\NUL\NUL\NUL\NUL\139H0E\STX!\NUL\217s\180\218\128\137t\v\159\149\220Y\133/Y\SUB\147\145\187\235A\f\SUB\132\EOT\141;L*\131\206\227\STX \ENQi\171\CAN\ETB\170\SOx\172\n\SUB\209\237\SYNp<\254qnq.\DC4\253\255\SO\183\229\129c\\\174\t\SOHA\EOT\247\137`^\202\191y\ESCq\155M\n\169\DC1\228\239\128\SOH\t\EOT\170\&2\227|+{\244'\230\188.\212\f\194\NAKh\231\197\174\209\136\229\140\247\207%\179\197@\252\139= \238\196\157\150t\SYN\215U\148G@\255\255\255\255\STX\DLE'\NUL\NUL\NUL\NUL\NUL\NUL\EMv\169\DC4$\169\SI\190~\133/\FS#<\250\186\158G?\128\SUB^y\n\136\172\240\230&\a\NUL\NUL\NUL\NUL\EMv\169\DC4\163\252\141\a\181\155A7\191\238-N\f\249@\163\182V\181\f\136\172\NUL\NUL\NUL\NUL"
"0B11090774780000000000000000000002010000232E60F70100000001B50418BBEA2416B4C9CED52DCE85FFCA7B486668BD69BBC7C34102E12F82B7BE000000008B483045022100D973B4DA8089740B9F95DC59852F591A9391BBEB410C1A84048D3B4C2A83CEE302200569AB1817AA0E78AC0A1AD1ED16703CFE716E712E14FDFF0EB7E581635CAE09014104F789605ECABF791B719B4D0AA911E4EF80010904AA32E37C2B7BF427E6BC2ED40CC21568E7C5AED188E58CF7CF25B3C540FC8B3D20EEC49D967416D755944740FFFFFFFF0210270000000000001976A91424A90FBE7E852F1C233CFABA9E473F801A5E790A88ACF0E62607000000001976A914A3FC8D07B59B4137BFEE2D4E0CF940A3B656B50C88AC00000000"

tx return
"`\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\DEL\NUL\NUL\NUL\NUL\255\255\255\DEL\254\255\255\DEL\SOH\255\255\255\DEL\NUL\NUL\NUL\NUL\255\255\255\DEL\NUL\255\255\255\DEL\NUL/URGENT: Alert key compromised, upgrade required\NULF0D\STX e?\235\214A\SIG\SIk\174\DC1\202\209\156HA;\236\177\172,\ETB\249\b\253\SI\213;\220:\189R\STX m\SO\156\150\254\136\212\160\240\RS\217\222\218\226\182\249\224\r\169L\173\SI\236\170\230n\207h\155\247\ESCP"
"60010000000000000000000000FFFFFF7F00000000FFFFFF7FFEFFFF7F01FFFFFF7F00000000FFFFFF7F00FFFFFF7F002F555247454E543A20416C657274206B657920636F6D70726F6D697365642C2075706772616465207265717569726564004630440220653FEBD6410F470F6BAE11CAD19C48413BECB1AC2C17F908FD0FD53BDC3ABD5202206D0E9C96FE88D4A0F01ED9DEDAE2B6F9E00DA94CAD0FECAAE66ECF689BF71B50"

version return
"0B110907 76657273696F6E0000000000660000004DFE4D227F1101000D04000000000000B3A1555A00000000000000000000000000000000000000000000FFFF7011EFC30B640D0400000000000000000000000000000000000000000000000038C028CF7B817B14102F5361746F7368693A302E31352E312FCB321300010B11090776657261636B000000000000000000005DF6E0E2"
"0B110907 616C65727400000000000000A80000001BF9AAEA60010000000000000000000000FFFFFF7F00000000FFFFFF7FFEFFFF7F01FFFFFF7F00000000FFFFFF7F00FFFFFF7F002F555247454E543A20416C657274206B657920636F6D70726F6D697365642C2075706772616465207265717569726564004630440220653FEBD6410F470F6BAE11CAD19C48413BECB1AC2C17F908FD0FD53BDC3ABD5202206D0E9C96FE88D4A0F01ED9DEDAE2B6F9E00DA94CAD0FECAAE66ECF689BF71B50"


-}

{-

F9BEB4D976657273696F6E000000000066000000A2FFDC737F1101000D00000000000000279A555A00000000000000000000000000000000000000000000FFFF7011EFC387B60D00000000000000000000000000000000000000000000000000546FF1702798DD2F102F5361746F7368693A302E31342E322F9AAE070001F9BEB4D976657261636B000000000000000000005DF6E0E2F9BEB4D9616C65727400000000000000A80000001BF9AAEA60010000000000000000000000FFFFFF7F00000000FFFFFF7FFEFFFF7F01FFFFFF7F00000000FFFFFF7F00FFFFFF7F002F555247454E543A20416C657274206B657920636F6D70726F6D697365642C2075706772616465207265717569726564004630440220653FEBD6410F470F6BAE11CAD19C48413BECB1AC2C17F908FD0FD53BDC3ABD5202206D0E9C96FE88D4A0F01ED9DEDAE2B6F9E00DA94CAD0FECAAE66ECF689BF71B50

"\249\190\180\217version\NUL\NUL\NUL\NUL\NULf\NUL\NUL\NUL\162\255\220s\DEL\DC1\SOH\NUL\r\NUL\NUL\NUL\NUL\NUL\NUL\NUL'\154UZ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255p\DC1\239\195\135\182\r\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULTo\241p'\152\221/\DLE/Satoshi:0.14.2/\154\174\a\NUL\SOH\249\190\180\217verack\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL]\246\224\226\249\190\180\217alert\NUL\NUL\NUL\NUL\NUL\NUL\NUL\168\NUL\NUL\NUL\ESC\249\170\234`\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\DEL\NUL\NUL\NUL\NUL\255\255\255\DEL\254\255\255\DEL\SOH\255\255\255\DEL\NUL\NUL\NUL\NUL\255\255\255\DEL\NUL\255\255\255\DEL\NUL/URGENT: Alert key compromised, upgrade required\NULF0D\STX e?\235\214A\SIG\SIk\174\DC1\202\209\156HA;\236\177\172,\ETB\249\b\253\SI\213;\220:\189R\STX m\SO\156\150\254\136\212\160\240\RS\217\222\218\226\182\249\224\r\169L\173\SI\236\170\230n\207h\155\247\ESCP"

-}
