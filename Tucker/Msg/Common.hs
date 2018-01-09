module Tucker.Msg.Common where

import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS
import Tucker.Enc

newtype VInt = VInt Integer

instance Encodable VInt where
    encode end (VInt num)
        | num < 0xfd        = bchar num
        | num <= 0xffff     = BSR.append (bchar 0xfd) (encode end (fromInteger num :: Word16))
        | num <= 0xffffffff = BSR.append (bchar 0xfe) (encode end (fromInteger num :: Word32))
        | otherwise         = BSR.append (bchar 0xff) (encode end (fromInteger num :: Word64))

newtype VStr = VStr String | VBStr ByteString

instance Encodable VStr where
    encode end (VStr str) =
        BSR.append (encode end $ VInt $ length str) $ BS.pack str

    encode end (VBStr bs) =
        BSR.append (encode end $ VInt $ BSR.length bs) bs
