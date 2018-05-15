{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tucker.Msg.Common where

import Data.Hex
import Data.Int
import Data.Bits
import Data.Char
import Data.Word
import Data.List
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Network.Socket

import Tucker.Enc
import Tucker.Conf
import Tucker.Util
import Tucker.Auth

type Height = Int64

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

commandToString :: Command -> String
commandToString cmd = str
    where Just str = lookup cmd cmd_map

class (Encodable p, Decodable p) => MsgPayload p where

newtype VInt = VInt Word64 deriving (Show, Eq, Ord, Num, Real, Enum, Integral)
newtype VStr = VStr ByteString deriving (Show, Eq)

vstrToBS :: VStr -> ByteString
vstrToBS (VStr bs) = bs

vstrToString :: VStr -> String
vstrToString (VStr bs) = BS.unpack bs

vstr = VStr . BS.pack
vbstr = VStr

type RawScript = ByteString

data HashTypeSingle =
    SIGHASH_ALL | SIGHASH_NONE | SIGHASH_SINGLE | SIGHASH_ANYONECANPAY
    deriving (Eq, Show)

newtype HashType = HashType [HashTypeSingle] deriving (Show)

hashTypeToInt :: (Integral t, Bits t) => HashType -> t
hashTypeToInt (HashType []) = 0
hashTypeToInt (HashType (SIGHASH_ALL:rst)) =
    (hashTypeToInt (HashType rst) .&. complement 0xf) .|. 0x1

hashTypeToInt (HashType (SIGHASH_NONE:rst)) =
    (hashTypeToInt (HashType rst) .&. complement 0xf) .|. 0x2

hashTypeToInt (HashType (SIGHASH_SINGLE:rst)) =
    (hashTypeToInt (HashType rst) .&. complement 0xf) .|. 0x3

hashTypeToInt (HashType (SIGHASH_ANYONECANPAY:rst)) =
    hashTypeToInt (HashType rst) .|. 0x80

hasHashType :: HashType -> HashTypeSingle -> Bool
hasHashType (HashType lst) t = t `elem` lst

intToHashType :: (Integral t, Bits t) => t -> HashType
intToHashType i =
    HashType $
    base : (if i .&. 0x80 /= 0 then [ SIGHASH_ANYONECANPAY ] else [])
    where
        base = case i .&. 0xf of
            0x01 -> SIGHASH_ALL
            0x02 -> SIGHASH_NONE
            0x03 -> SIGHASH_SINGLE
            _ -> error "unrecognized hash type"

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
    = LackData Int -- lack data mark(taking the total length as the argument)
    | MsgHead {
        magicno :: ByteString, -- 4 bytes
        command :: Command, -- 12 bytes
        -- payload_length  :: Word32,
        -- checksum :: Word32 -- first 4 bytes of double sha256 of payload
        payload :: ByteString
    } deriving (Show, Eq)

instance Encodable VInt where
    encodeB end num
        | num < 0xfd        = bcharB num
        | num <= 0xffff     = bcharB 0xfd <> encodeB end (fi num :: Word16)
        | num <= 0xffffffff = bcharB 0xfe <> encodeB end (fi num :: Word32)
        | otherwise         = bcharB 0xff <> encodeB end (fi num :: Word64)

instance Decodable VInt where
    decoder = do
        fst <- byteD
        let
            wrap :: Integral t => t -> Decoder VInt
            wrap = pure . fi

        case fst of
            0xfd -> (decoder :: Decoder Word16) >>= wrap
            0xfe -> (decoder :: Decoder Word32) >>= wrap
            0xff -> (decoder :: Decoder Word64) >>= wrap
            b    -> return (fi b)

instance Sizeable VInt where
    sizeOf (VInt num)
        | num < 0xfd        = 1
        | num <= 0xffff     = 3
        | num <= 0xffffffff = 5
        | otherwise         = 9

instance Encodable VStr where
    encodeB end (VStr bs) =
        encodeB end (fi (BSR.length bs) :: VInt) <> encodeLEB bs

instance Decodable VStr where
    decoder = do
        len <- decoder :: Decoder VInt
        bs <- bsD (fi len)
        return $ vbstr bs

instance Sizeable VStr where
    sizeOf (VStr str) =
        sizeOf (fi len :: VInt) + len
        where len = BSR.length str

serv_type_map :: [(NodeServiceTypeSingle, Word64)]
serv_type_map = [
        (TCKR_NODE_NETWORK, 1),
        (TCKR_NODE_GETUTXO, 2),
        (TCKR_NODE_BLOOM,   4),
        (TCKR_NODE_WITNESS, 8)
    ]

serviceInclude :: NodeServiceType -> NodeServiceType -> Bool
serviceInclude (NodeServiceType a) (NodeServiceType b) = all (`elem` a) b

instance Encodable NodeServiceType where
    encodeB end (NodeServiceType serv) =
        encodeB end $
        foldl (.|.) 0 $
        flip map serv $ \s ->
            case lookup s serv_type_map of
                Just i -> i
                Nothing -> error "service type not supported"

instance Decodable NodeServiceType where
    decoder = do
        serv <- decoder :: Decoder Word64
        return $ NodeServiceType $
                 map fst $
                 filter (\(s, i) -> serv .&. i /= 0) serv_type_map

instance Encodable NetAddr where
    encodeB end (NetAddr {
        time = time,
        net_serv = net_serv,
        ipv6o4 = ipv6o4,
        port = port
    }) =
        e time <> e net_serv <>
        e ipv6o4 <> encodeBEB port -- port here is big-endian
        where
            e :: Encodable t => t -> Builder
            e = encodeB end

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
    encodeB _ cmd =
        case lookup cmd cmd_map of
            Just cmdstr -> encodeLEB (padnull 12 cmdstr)
            _ -> error "cmd not in map"

instance Decodable Command where
    decoder = do
        raw <- bsD 12
        let cmdstr = trimnull raw

        case lookup cmdstr cmd_map_r of
            Just cmd -> return cmd
            Nothing -> fail ("no such command " ++ (show cmdstr))

instance Sizeable MsgHead where
    sizeOf _ = 4 + 12 + 4

instance Encodable MsgHead where
    encodeB end (MsgHead {
        magicno = magicno,
        command = command,
        payload = payload
    }) =
        mconcat [
            e magicno,
            e command,

            e (fi (BSR.length payload) :: Word32),
            e (payloadCheck payload), -- checksum

            e payload
        ]
        where
            e :: Encodable t => t -> Builder
            e = encodeB end

    encodeB _ (LackData _) = error "encoding LackData flag"

instance Decodable MsgHead where
    decoder = do
        let head_size = sizeOf (u :: MsgHead)

        clen <- checkLenD head_size -- length of header

        if not clen then
            return (LackData head_size)
        else do
            magicno     <- bsD 4
            command     <- decoder
            payload_len <- decoder :: Decoder Word32
            checksum    <- bsD 4

            clen        <- checkLenD (fi payload_len)

            -- tLn ("!!! received: " ++ show command ++ " " ++ show len ++ "/" ++ show payload_len) $

            if not clen then -- not eough data
                return (LackData (head_size + fi payload_len))
            else do
                payload <- bsD (fi payload_len)

                if payloadCheck payload == checksum then
                    return MsgHead {
                        magicno = magicno,
                        command = command,
                        payload = payload
                    }
                else
                    fail "payload check failed"

-- vint + list
vlistD :: Decoder t -> Decoder [t]
vlistD elemD = do
    len <- decoder :: Decoder VInt
    listD (fi len) elemD

encodeVList :: Encodable t => Endian -> [t] -> ByteString
encodeVList end list =
    encode end (fi (length list) :: VInt) <> encode end list

encodeVListB :: Encodable t => Endian -> [t] -> Builder
encodeVListB end list =
    encodeB end (fi (length list) :: VInt) <> encodeB end list

payloadCheck :: ByteString -> ByteString
payloadCheck = BS.take 4 . doubleSHA256

padnull :: Int -> String -> ByteString
padnull full str =
    BS.pack str <> BSR.replicate (full - len) 0
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
