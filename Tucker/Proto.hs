{-# LANGUAGE FlexibleInstances #-}

module Tucker.Proto where

import Tucker.Enc
import Tucker.Int

import Data.Char
import Data.Hex
import Data.Int
import Data.Word
import qualified Data.ByteString as BSR
import qualified Data.ByteString.Char8 as BS

type Hash = String
type RawScript = ByteString

data OutPoint = OutPoint Hash Word32 deriving (Eq, Show, Read)

newtype VInt = VInt Integer

data TransactionInput =
    TxInput {
        prev_out        :: OutPoint,
        sig_script      :: RawScript,
        seqn            :: Int32 -- sequence, currently not used
    } deriving (Eq, Show, Read)

data TransactionOutput =
    TxOutput {
        value           :: Int64, -- in Satoshis, 10^-8 BTC
        pk_script       :: RawScript
    } deriving (Eq, Show, Read)

data TransactionWitness = TxWitness deriving (Eq, Show, Read)
    
data Transaction =
    TxBody {
        version     :: Int32,
        flag        :: Int16, -- currently only 1 or 0
        
        tx_in       :: [TransactionInput],
        tx_out      :: [TransactionOutput],
        tx_witness  :: [TransactionWitness],

        lock_time   :: Int32 -- time when the tx is locked in a block
    } deriving (Eq, Show, Read)

data Wallet =
    Wallet {
        keypair :: ECCKeyPair
    }

-- newtype Encoder rt = Encoder { encoderOf :: ByteString -> Maybe (rt, ByteString) }

-- instance Functor Encoder where
--     -- maps parsed results and returns a new parser
--     fmap f (Encoder enc) = Encoder $ \dat -> enc dat
--     fmap f (Encoder enc) = Encoder $  Parser $ \p -> [ (f a, b) | (a, b) <- ps p ]

class Encodable t where
    encode :: Endian -> t -> ByteString
    
    encode'le :: t -> ByteString
    encode'le = encode LittleEndian

    encode'be :: t -> ByteString
    encode'be = encode BigEndian

instance Encodable ByteString where
    encode _ = id

instance Encodable Char where
    encode _ = bchar . ord

instance Encodable Int8 where
    encode _ = bchar

instance Encodable Word8 where
    encode _ = bchar

instance Encodable Int16 where
    encode = encodeInt 2

instance Encodable Int32 where
    encode = encodeInt 4

instance Encodable Int64 where
    encode = encodeInt 8

instance Encodable Word16 where
    encode = encodeInt 2

instance Encodable Word32 where
    encode = encodeInt 4

instance Encodable Word64 where
    encode = encodeInt 8

instance Encodable a => Encodable [a] where
    encode end = BSR.concat . (map (encode end))

-- data OutPoint = OutPoint Hash Word32

instance Encodable OutPoint where
    encode end (OutPoint hash index) =
        BSR.append (BSR.reverse $ encode end hash) (encode end index)

instance Encodable VInt where
    encode end (VInt num)
        | num < 0xfd        = bchar num
        | num <= 0xffff     = BSR.append (bchar 0xfd) (encode end (fromInteger num :: Word16))
        | num <= 0xffffffff = BSR.append (bchar 0xfe) (encode end (fromInteger num :: Word32))
        | otherwise         = BSR.append (bchar 0xff) (encode end (fromInteger num :: Word64))

instance Encodable TransactionInput where
    encode _ (TxInput {
        prev_out = prev_out,
        sig_script = sig_script,
        seqn = seqn
    }) =
        BSR.concat [
            e prev_out,
            e (VInt $ fromIntegral $ BSR.length sig_script),
            e sig_script,
            e seqn
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode'le

instance Encodable TransactionOutput where
    encode _ (TxOutput {
        value = value,
        pk_script = pk_script
    }) =
        BSR.concat [
            e value,
            e (VInt $ fromIntegral $ BSR.length pk_script),
            e pk_script
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode'le

instance Encodable TransactionWitness where
    encode _ _ = BSR.pack []

instance Encodable Transaction where
    encode _ (TxBody {
        version = version,
        flag = flag, -- currently only 1 or 0
        
        tx_in = tx_in,
        tx_out = tx_out,
        tx_witness = tx_witness,

        lock_time = lock_time
    }) =
        BSR.concat [
            e version,
            
            if flag == 0 then BSR.pack [] else e flag,

            e (VInt $ fromIntegral $ length tx_in),
            e tx_in,
            
            e (VInt $ fromIntegral $ length tx_out),
            e tx_out,
            
            e tx_witness,
            
            e lock_time
        ]
        where
            e :: Encodable t => t -> ByteString
            e = encode'le

-- dehex v = case unhex v :: Maybe String of
--     Just str -> str
--     Nothing -> error "illegal hex"

-- test = TxBody {
--     version = 1,
--     flag = 0,

--     tx_in = [
--         TxInput {
--             prev_out =
--                 OutPoint
--                     (dehex "81b4c832d70cb56ff957589752eb4125a4cab78a25a8fc52d6a09e5bd4404d48")
--                     0,

--             sig_script = BSR.pack [],
--             seqn = -1
--         }
--     ],

--     tx_out = [
--         TxOutput {
--             value = 123,
--             pk_script = BSR.pack []
--         }
--     ],

--     tx_witness = [],

--     lock_time = 0
-- }
