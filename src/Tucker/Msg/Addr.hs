-- addr and getaddr message

module Tucker.Msg.Addr where

import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.Enc
import Tucker.Util
import Tucker.Msg.Common

newtype AddrPayload =
    AddrPayload {
        addrs :: [NetAddr]
    } deriving (Show)

instance MsgPayload AddrPayload

instance Encodable AddrPayload where
    encodeB end (AddrPayload addrs) =
        encodeB end (fi (length addrs) :: VInt) <>
        encodeB end addrs

instance Decodable AddrPayload where
    decoder = do
        (VInt len) <- decoder
        addrs <- listD (fromIntegral len) decoder

        return $ AddrPayload addrs

encodeAddrPayload :: [NetAddr] -> IO ByteString
encodeAddrPayload = return . encodeLE . AddrPayload

encodeGetaddrPayload :: IO ByteString
encodeGetaddrPayload = return $ BSR.empty
