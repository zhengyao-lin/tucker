module Tucker.Msg.Common where

import Data.Bits
import Data.Char
import Data.Word
import Data.List
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Network.Socket

import Debug.Trace

import Tucker.Enc
import Tucker.Std
import Tucker.Auth

serv_type = [ BTC_NODE_NETWORK, BTC_NODE_GETUTXO, BTC_NODE_BLOOM ]

cmd_map = [
        (BTC_CMD_VERSION,       "version"),
        (BTC_CMD_VERACK,        "verack"),
        (BTC_CMD_ADDR,          "addr"),
        (BTC_CMD_INV,           "inv"),
        (BTC_CMD_GETDATA,       "getdata"),
        (BTC_CMD_NOTFOUND,      "notfound"),
        (BTC_CMD_GETBLOCKS,     "getblocks"),
        (BTC_CMD_GETHEADERS,    "getheaders"),
        (BTC_CMD_TX,            "tx"),
        (BTC_CMD_BLOCK,         "block"),
        (BTC_CMD_HEADERS,       "headers"),
        (BTC_CMD_GETADDR,       "getaddr"),
        (BTC_CMD_MEMPOOL,       "mempool"),
        (BTC_CMD_REPLY,         "reply"),
        (BTC_CMD_PING,          "ping"),
        (BTC_CMD_PONG,          "pong"),
        (BTC_CMD_REJECT,        "reject"),
        (BTC_CMD_ALERT,         "alert")
    ]

cmd_map_r = map (\(a, b) -> (b, a)) cmd_map

newtype VInt = VInt Integer deriving (Show, Eq)
data VStr = VStr String | VBStr ByteString deriving (Show, Eq)

data NetAddr =
    NetAddr {
        time         :: Word32,
        net_serv     :: BTCServiceType,
        ipv6o4       :: ByteString, -- 16 bytes 
        port         :: Word16
    } deriving (Show, Eq)

data Command
    = BTC_CMD_VERSION
    | BTC_CMD_VERACK
    | BTC_CMD_ADDR
    | BTC_CMD_INV
    | BTC_CMD_GETDATA
    | BTC_CMD_NOTFOUND
    | BTC_CMD_GETBLOCKS
    | BTC_CMD_GETHEADERS
    | BTC_CMD_TX
    | BTC_CMD_BLOCK
    | BTC_CMD_HEADERS
    | BTC_CMD_GETADDR
    | BTC_CMD_MEMPOOL
    | BTC_CMD_REPLY
    | BTC_CMD_PING
    | BTC_CMD_PONG
    | BTC_CMD_REJECT
    | BTC_CMD_ALERT deriving (Show, Eq)

data Hash256 = Hash256FromBS ByteString deriving (Show, Eq)
type RawScript = ByteString

data MsgHead
    = LackData -- lack data mark
    | MsgHead {
        magicno :: ByteString, -- 4 bytes
        command :: Command, -- 12 bytes
        -- payload_length  :: Word32,
        -- checksum :: Word32 -- first 4 bytes of double sha256 of payload
        payload :: ByteString
    } deriving (Show, Eq)

instance Encodable Hash256 where
    encode _ (Hash256FromBS bs) =
        if BSR.length bs == 32 then
            bs
        else error "hash 256 length not correct"

instance Decodable Hash256 where
    decoder =
        bsD 32 >>= pure . Hash256FromBS

instance Encodable VInt where
    encode end (VInt num)
        | num < 0xfd        = bchar num
        | num <= 0xffff     = BSR.append (bchar 0xfd) (encode end (fromInteger num :: Word16))
        | num <= 0xffffffff = BSR.append (bchar 0xfe) (encode end (fromInteger num :: Word32))
        | otherwise         = BSR.append (bchar 0xff) (encode end (fromInteger num :: Word64))

instance Decodable VInt where
    decoder = do
        fst <- byteD
        let
            wrap :: Integral t => t -> Decoder VInt
            wrap = pure . VInt . fromIntegral
        
        case fst of
            0xfd -> (decoder :: Decoder Word16) >>= wrap
            0xfe -> (decoder :: Decoder Word32) >>= wrap
            0xff -> (decoder :: Decoder Word64) >>= wrap
            b    -> return $ VInt $ fromIntegral b

instance Encodable VStr where
    encode end (VStr str) =
        BSR.append (encode end $ VInt $ fromIntegral $ length str) $ BS.pack str

    encode end (VBStr bs) =
        BSR.append (encode end $ VInt $ fromIntegral $ BSR.length bs) bs

instance Decodable VStr where
    decoder = do
        VInt len <- decoder :: Decoder VInt
        bs <- bsD $ fromInteger len
        return $ VStr $ BS.unpack bs

instance Encodable BTCServiceType where
    encode end (BTCServiceType serv) =
        encode end $
        ((foldr (.|.) 0) $
        map (\s -> case findIndex (== s) serv_type of
            Just i -> 1 `shift` i
            _ -> error "impossible") serv :: Word64)

instance Decodable BTCServiceType where
    decoder = do
        serv <- decoder :: Decoder Word64
        return $ BTCServiceType
                $ map snd
                $ filter (\(i, _) -> serv .&. shift 1 i /= 0) (zip [0..] serv_type)

instance Encodable NetAddr where
    encode end (NetAddr {
        time = time,
        net_serv = net_serv,
        ipv6o4 = ipv6o4,
        port = port
    }) =
        BSR.concat [
            e time,
            e net_serv,
            e ipv6o4,
            encodeBE port -- port here is big-endian
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

instance Decodable NetAddr where
    decoder = do
        time <- decoder
        net <- decoder
        ipv6o4 <- bsD 16
        port <- decoderBE 
        
        return $ NetAddr {
            time = time,
            net_serv = net,
            ipv6o4 = ipv6o4,
            port = port
        }

instance Encodable Command where
    encode _ cmd =
        case lookup cmd cmd_map of
            Just cmdstr -> padnull 12 cmdstr
            _ -> error "cmd not in map"

instance Decodable Command where
    decoder = do
        raw <- bsD 12
        let cmdstr = trimnull raw

        case lookup cmdstr cmd_map_r of
            Just cmd -> return cmd
            Nothing -> fail ("no such command " ++ (show cmdstr))

instance Encodable MsgHead where
    encode end (MsgHead {
        magicno = magicno,
        command = command,
        payload = payload
    }) =
        BSR.concat [
            e magicno,
            e command,

            e (fromIntegral $ BSR.length payload :: Word32),
            payloadCheck payload, -- checksum

            e payload
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode end

    encode _ LackData = error "encoding LackData flag"

instance Decodable MsgHead where
    decoder = do
        clen <- checkLenD (4 + 12 + 4 + 4) -- length of header

        if not clen then
            return LackData
        else do
            magicno       <- bsD 4
            command       <- decoder
            payload_len   <- decoder :: Decoder Word32
            checksum      <- bsD 4

            clen          <- checkLenD $ fromIntegral payload_len

            if not clen then
                return LackData
            else do
                payload       <- bsD $ fromIntegral payload_len

                if payloadCheck payload == checksum then
                    return $ MsgHead {
                        magicno = magicno,
                        command = command,
                        payload = payload
                    }
                else
                    fail "payload check failed"

payloadCheck :: ByteString -> ByteString
payloadCheck = BS.take 4 . ba2bs . sha256 . sha256

unixTimestamp :: Integral a => IO a
unixTimestamp = round `fmap` getPOSIXTime

padnull :: Int -> String -> ByteString
padnull full str =
    BSR.append (BS.pack str) $ BSR.pack [ 0x00 | _ <- [ 1 .. (full - len) ] ]
    where
        len = length str

trimnull :: ByteString -> String
trimnull bs =
    if BSR.length bs == 0 || BSR.head bs == 0x00 then
        []
    else
        (chr $ fromIntegral $ BSR.head bs) : (trimnull $ BSR.tail bs)

ip42ip6 :: ByteString -> ByteString
ip42ip6 addrv4 =
    BSR.append (BSR.pack pref) addrv4
    where
        pref = [ 0x00, 0x00, 0x00, 0x00,
                 0x00, 0x00, 0x00, 0x00,
                 0x00, 0x00, 0xff, 0xff ]

ip42netaddr :: String -> Word16 -> BTCServiceType -> IO NetAddr
ip42netaddr addr port serv = do
    time <- unixTimestamp
    enc <- inet_addr addr
    let addrv6 = ip42ip6 $ encodeBE enc
    pure $ NetAddr {
        time = time,
        net_serv = serv,
        ipv6o4 = addrv6,
        port = port
    }

encodeMsg :: BTCNetwork -> Command -> IO ByteString -> IO ByteString
encodeMsg net cmd mpayload = do
    payload <- mpayload

    return $ encodeLE $ MsgHead {
        magicno = magicNo net,
        command = cmd,
        payload = payload
    }
