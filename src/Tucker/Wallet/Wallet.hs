module Tucker.Wallet.Wallet where

import Tucker.ECC

data Wallet =
    Wallet {
        wal_priv_key :: ECCPrivateKey
    } deriving (Show)
