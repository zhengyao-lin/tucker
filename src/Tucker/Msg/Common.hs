module Tucker.Msg.Common where

import Data.Hex
import Data.Bits
import Data.Char
import Data.Word
import Data.List
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Network.Socket

import Debug.Trace

import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Auth

serv_type = [ TCKR_NODE_NETWORK, TCKR_NODE_GETUTXO, TCKR_NODE_BLOOM ]

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

class (Encodable p, Decodable p) => MsgPayload p where

newtype VInt = VInt Integer deriving (Show, Eq)
data VStr = VStr String | VBStr ByteString deriving (Show, Eq)

vstrToBS :: VStr -> ByteString
vstrToBS (VStr str) = BS.pack str
vstrToBS (VBStr bs) = bs

type RawScript = ByteString

data HashType =
    SIGHASH_ALL | SIGHASH_NONE | SIGHASH_SINGLE | SIGHASH_ANYONECANPAY

hashTypeToInt :: Integral t => HashType -> t
hashTypeToInt SIGHASH_ALL          = 0x01
hashTypeToInt SIGHASH_NONE         = 0x02
hashTypeToInt SIGHASH_SINGLE       = 0x03
hashTypeToInt SIGHASH_ANYONECANPAY = 0x80

intToHashType :: Integral t => t -> HashType
intToHashType i =
    case fi i :: Int of
        0x01 -> SIGHASH_ALL
        0x02 -> SIGHASH_NONE
        0x03 -> SIGHASH_SINGLE
        0x80 -> SIGHASH_ANYONECANPAY

data NetAddr =
    NetAddr {
        time         :: Word32,
        net_serv     :: NodeServiceType,
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

data MsgHead
    = LackData -- lack data mark
    | MsgHead {
        magicno :: ByteString, -- 4 bytes
        command :: Command, -- 12 bytes
        -- payload_length  :: Word32,
        -- checksum :: Word32 -- first 4 bytes of double sha256 of payload
        payload :: ByteString
    } deriving (Show, Eq)

instance Encodable VInt where
    encode end (VInt num)
        | num < 0xfd        = bchar num
        | num <= 0xffff     = bchar 0xfd <> encode end (fromInteger num :: Word16)
        | num <= 0xffffffff = bchar 0xfe <> encode end (fromInteger num :: Word32)
        | otherwise         = bchar 0xff <> encode end (fromInteger num :: Word64)

instance Decodable VInt where
    decoder = do
        fst <- byteD
        let
            wrap :: Integral t => t -> Decoder VInt
            wrap = pure . VInt . fi

        case fst of
            0xfd -> (decoder :: Decoder Word16) >>= wrap
            0xfe -> (decoder :: Decoder Word32) >>= wrap
            0xff -> (decoder :: Decoder Word64) >>= wrap
            b    -> return $ VInt $ fi b

instance Encodable VStr where
    encode end (VStr str) =
        encode end (VInt $ fi $ length str) <> BS.pack str

    encode end (VBStr bs) =
        encode end (VInt $ fi $ BSR.length bs) <> bs

instance Decodable VStr where
    decoder = do
        VInt len <- decoder :: Decoder VInt
        bs <- bsD $ fromInteger len
        return $ VStr $ BS.unpack bs

instance Encodable NodeServiceType where
    encode end (NodeServiceType serv) =
        encode end $
        ((foldl (.|.) 0) $
        map (\s -> case findIndex (== s) serv_type of
            Just i -> 1 `shift` i
            _ -> error "impossible") serv :: Word64)

instance Decodable NodeServiceType where
    decoder = do
        serv <- decoder :: Decoder Word64
        return $ NodeServiceType
               $ map snd
               $ filter (\(i, _) -> serv .&. shift 1 i /= 0) (zip [0..] serv_type)

instance Encodable NetAddr where
    encode end (NetAddr {
        time = time,
        net_serv = net_serv,
        ipv6o4 = ipv6o4,
        port = port
    }) =
        mconcat [
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
        mconcat [
            e magicno,
            e command,

            e (fi $ BSR.length payload :: Word32),
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

            clen          <- checkLenD $ fi payload_len
            -- len           <- lenD

            -- trace ("!!! received: " ++ show command ++ " " ++ show len ++ "/" ++ show payload_len) $

            if not clen then
                return LackData
            else do
                payload       <- bsD $ fi payload_len

                if payloadCheck payload == checksum then
                    return $ MsgHead {
                        magicno = magicno,
                        command = command,
                        payload = payload
                    }
                else
                    fail "payload check failed"

-- vint + list
vlistD :: Decoder t -> Decoder [t]
vlistD elemD = do
    VInt len <- decoder
    -- traceM $ "vlist length " ++ show len
    listD (fi len) elemD

encodeVList :: Encodable t => Endian -> [t] -> ByteString
encodeVList end list =
    encode end (VInt $ fi $ length list) <> encode end list

payloadCheck :: ByteString -> ByteString
payloadCheck = BS.take 4 . sha256 . sha256

padnull :: Int -> String -> ByteString
padnull full str =
    BS.pack str <> BSR.pack [ 0x00 | _ <- [ 1 .. (full - len) ] ]
    where
        len = length str

trimnull :: ByteString -> String
trimnull bs =
    if BSR.length bs == 0 || BSR.head bs == 0x00 then
        []
    else
        (chr $ fi $ BSR.head bs) : (trimnull $ BSR.tail bs)

encodeMsg :: TCKRConf -> Command -> IO ByteString -> IO ByteString
encodeMsg conf cmd mpayload = do
    payload <- mpayload

    return $ encodeLE $ MsgHead {
        magicno = tckr_magic_no conf,
        command = cmd,
        payload = payload
    }
