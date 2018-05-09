module Tucker.DeepSeq where

import Data.Int
import Data.Word
import qualified Data.ByteString as BSR

class NFData a where
    rnf :: a -> ()
    rnf v = v `seq` () -- default

instance NFData ()
    
instance NFData Int
instance NFData Int8
instance NFData Int16
instance NFData Int32
instance NFData Int64

instance NFData Word
instance NFData Word8
instance NFData Word16
instance NFData Word32
instance NFData Word64

instance NFData Float
instance NFData Double

instance NFData Char
instance NFData BSR.ByteString

instance NFData a => NFData [a] where
    rnf [] = ()
    rnf (x:xs) = rnf x `seq` rnf xs

instance (NFData a, NFData b) => NFData (Either a b) where
    rnf (Left a) = rnf a
    rnf (Right b) = rnf b

instance NFData a => NFData (Maybe a) where
    rnf Nothing = ()
    rnf (Just a) = rnf a

instance (NFData a1, NFData a2) => NFData (a1, a2) where
    rnf (a1, a2) = rnf a1 `seq` rnf a2

instance (NFData a1, NFData a2, NFData a3) => NFData (a1, a2, a3) where
    rnf (a1, a2, a3) = rnf a1 `seq` rnf a2 `seq` rnf a3

deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b

force :: NFData a => a -> a
force a = rnf a `seq` a
