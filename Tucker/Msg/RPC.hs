module Tucker.Msg.RPC where

import Data.Hex
import Data.Char
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

import Tucker.Enc

type RPCHash = String

-- generate a RPC-byte-order hash from the hash of the transaction
encodeRPCHash :: ByteString -> RPCHash
encodeRPCHash = (map toLower) . hex . BS.unpack . BS.reverse -- . ba2bs . sha256 . sha256

-- reverse process to encodeRPCHash
-- input e.g. "beb7822fe10241c3c7bb69bd6866487bcaff85ce2dd5cec9b41624eabb1804b5"
decodeRPCHash :: RPCHash -> ByteString
decodeRPCHash = BS.reverse . BS.pack . (!!0) . unhex . (map toUpper)
