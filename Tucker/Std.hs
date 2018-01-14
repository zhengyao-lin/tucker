module Tucker.Std where

import Data.Int
import Data.Word
import qualified Data.ByteString as BSR
import Crypto.PubKey.ECC.Types

import Tucker.Enc
import Tucker.Conf

data BTCNetwork = BTCNetwork {
        wifPref :: Word8,
        pubPref :: Word8,
        magicNo :: ByteString,
        listenPort :: Word16
    } deriving (Show, Read, Eq)

btc_curve = getCurveByName SEC_p256k1

btc_mainnet =
    BTCNetwork {
        wifPref = 0x80,
        pubPref = 0x00,
        magicNo = BSR.pack [ 0xf9, 0xbe, 0xb4, 0xd9 ],
        listenPort = 8333
    }

btc_testnet3 =
    BTCNetwork {
        wifPref = 0xef,
        pubPref = 0x6f,
        magicNo = BSR.pack [ 0x0b, 0x11, 0x09, 0x07 ],
        listenPort = 18333
    }

-- btc_cli_port = 8333 :: Word16

data BTCServiceTypeSingle = BTC_NODE_NETWORK | BTC_NODE_GETUTXO | BTC_NODE_BLOOM deriving (Show, Eq)
data BTCServiceType = BTCServiceType [BTCServiceTypeSingle] deriving (Show, Eq)

btc_cli_service = BTCServiceType [ BTC_NODE_NETWORK ]

btc_version :: Integral t => t
btc_version = 60002

btc_user_agent = "/Tucker:" ++ tucker_version ++ "/"
