module Tucker.Msg.Common where

import Data.Char
import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

import Tucker.Enc
import Tucker.Auth
import Tucker.Std

newtype VInt = VInt Integer deriving (Show, Eq)

instance Encodable VInt where
    encode end (VInt num)
        | num < 0xfd        = bchar num
        | num <= 0xffff     = BSR.append (bchar 0xfd) (encode end (fromInteger num :: Word16))
        | num <= 0xffffffff = BSR.append (bchar 0xfe) (encode end (fromInteger num :: Word32))
        | otherwise         = BSR.append (bchar 0xff) (encode end (fromInteger num :: Word64))

data VStr = VStr String | VBStr ByteString deriving (Show, Eq)

instance Encodable VStr where
    encode end (VStr str) =
        BSR.append (encode end $ VInt $ fromIntegral $ length str) $ BS.pack str

    encode end (VBStr bs) =
        BSR.append (encode end $ VInt $ fromIntegral $ BSR.length bs) bs

data Command = BTC_CMD_VERSION | BTC_CMD_VERACK | BTC_CMD_TX deriving (Show, Eq)

cmd_map = [
        (BTC_CMD_VERSION, "version"),
        (BTC_CMD_VERACK,  "verack"),
        (BTC_CMD_TX,      "tx")
    ]

cmd_map_r = map (\(a, b) -> (b, a)) cmd_map

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

data MsgHead
    = LackData -- lack data mark
    | MsgHead {
        magicno :: ByteString, -- 4 bytes
        command :: Command, -- 12 bytes
        -- payload_length  :: Word32,
        -- checksum :: Word32 -- first 4 bytes of double sha256 of payload
        payload :: ByteString
    } deriving (Show, Eq)

payloadCheck :: ByteString -> ByteString
payloadCheck = BS.take 4 . ba2bs . sha256 . sha256

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

encodeMsg :: BTCNetwork -> Command -> IO ByteString -> IO ByteString
encodeMsg net cmd mpayload = do
    payload <- mpayload

    return $ encodeLE $ MsgHead {
        magicno = magicNo net,
        command = cmd,
        payload = payload
    }
