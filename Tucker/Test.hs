module Tucker.Test where

import Tucker.Std
import Tucker.Msg
import Tucker.Conf
import Tucker.Atom

import Tucker.P2P.Init
import Tucker.P2P.Util
import Tucker.P2P.Node
import Tucker.P2P.Action

import Tucker.Chain.Object

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString

{-

need to check bitseed.xf2.org
testnet-seed.bluematt.me
use this !!! seed.tbtc.petertodd.org

99.242.230.163
76.111.96.126
130.235.100.241
198.251.83.19
138.68.229.19

let net = btc_testnet3
addr <- ipToAddr "88.198.20.152" 18333
sock <- buildSocketTo addr
connect sock (addrAddress addr)
selfaddr <- ip4ToNetAddr "127.0.0.1" (listenPort net) btc_cli_service
msg <- encodeMsg net BTC_CMD_VERSION $ encodeVersionPayload net selfaddr
send sock msg
recv sock 1024
recv sock 1024

-- handshake finished

msg <- encodeMsg net BTC_CMD_GETBLOCKS $ encodeGetblocksPayload [] nullHash256
send sock msg
recv sock 1024

msg <- encodeMsg net BTC_CMD_GETDATA $ encodeGetdataPayload [ InvVector INV_TYPE_BLOCK (read "00000000700e92a916b46b8b91a14d1303d5d91ef0b09eecc3151fb958fd9a2e") ]
send sock msg
recv sock 1024

msg <- encodeMsg btc_testnet3 BTC_CMD_TX $ encodeTxPayload btc_testnet3 "933qtT8Ct7rGh29Eyb5gG69QrWmwGein85F1kuoShaGjJFFBSjk" [ OutPoint (decodeRPCHash "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5") 0 ] [ (10000, "miro9ZNPjcLnqvnJpSm8P6CUf1WPU98jET"), (119990000, "mvU2ysD322amhCeCPMhPc3L7hKDGGWSBz7") ]

send sock msg
recv sock 1024

-}
