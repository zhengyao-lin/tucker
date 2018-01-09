module Tucker.Std where

import Data.Int
import Data.Word
import qualified Data.ByteString as BSR
import Crypto.PubKey.ECC.Types

import Tucker.Enc

data BTCNetwork = BTCNetwork {
        wifPref :: Word8,
        pubPref :: Word8,
        magicNo :: ByteString
    } deriving (Show, Read, Eq)

btc_curve = getCurveByName SEC_p256k1

btc_mainnet =
    BTCNetwork {
        wifPref = 0x80,
        pubPref = 0x00,
        magicNo = BSR.pack [ 0xf9, 0xbe, 0xb4, 0xd9 ]
    }

btc_testnet3 =
    BTCNetwork {
        wifPref = 0xef,
        pubPref = 0x6f,
        magicNo = BSR.pack [ 0x0b, 0x11, 0x09, 0x07 ]
    }
