-- mining utils

module Tucker.P2P.Mining where

import Data.Word

import Tucker.Msg
import Tucker.Enc
import Tucker.Auth

-- a naive mining function
-- append a 32-bit nonce to dat until its hash is lower than the target
-- and return the nonce
doMine :: ByteString -> Hash256 -> IO Word32
doMine dat target =
    return (fst $ head $ dropWhile (not . snd) $ map check [ 0 .. maxBound ])
    where
        check nonce =
            (nonce, bsToHash256 (sha256 (dat <> encodeLE nonce)) <= target)
