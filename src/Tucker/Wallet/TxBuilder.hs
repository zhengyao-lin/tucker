{-# LANGUAGE TypeOperators #-}

module Tucker.Wallet.TxBuilder where

import Tucker.Msg
import Tucker.Enc
import Tucker.Conf
import Tucker.Util

import Tucker.Wallet.Address

class TxBuilder b where
    toTx :: TCKRConf -> b -> IO TxPayload

class TxInputBuilder b where
    -- generate tmp inputs
    toDraftInputs :: TCKRConf -> b -> IO ([TxInput], [TxWitness])
    toDraftInputs conf b = do
        (input, wit) <- toDraftInput conf b
        return ([input], [wit])

    -- sign and generate final inputs
    toFinalInputs :: TCKRConf -> TxPayload -> Int -> b -> IO ([TxInput], [TxWitness])
    toFinalInputs conf tx in_idx b = do
        (input, wit) <- toFinalInput conf tx in_idx b
        return ([input], [wit])

    toDraftInput :: TCKRConf -> b -> IO (TxInput, TxWitness)
    toDraftInput = error "using internal interface"

    toFinalInput :: TCKRConf -> TxPayload -> Int -> b -> IO (TxInput, TxWitness)
    toFinalInput = error "using internal interface"

class TxOutputBuilder b where
    toOutputs :: TCKRConf -> b -> IO [TxOutput]

data a :+ b = a :+ b deriving (Show)
data RegularTx i o = RegularTx i o
data PayToAddr = PayToAddr Satoshi Address deriving (Show)

instance TxOutputBuilder PayToAddr where
    toOutputs _ (PayToAddr value addr) =
        return [TxOutput {
            value = value,
            pk_script = encodeLE (genPubKeyScript addr)
        }]

instance (TxInputBuilder a, TxInputBuilder b) => TxInputBuilder (a :+ b) where
    toDraftInputs conf (a :+ b) =
        (\(i1, w1) (i2, w2) -> (i1 ++ i2, w1 ++ w2)) <$>
        toDraftInputs conf a <*> toDraftInputs conf b

    toFinalInputs conf tx in_idx (a :+ b) = do
        (i1, w1) <- toFinalInputs conf tx in_idx a
        (i2, w2) <- toFinalInputs conf tx (in_idx + length i1) b
        return (i1 ++ i2, w1 ++ w2)

instance (TxOutputBuilder a, TxOutputBuilder b) => TxOutputBuilder (a :+ b) where
    toOutputs conf (a :+ b) = (++) <$> toOutputs conf a <*> toOutputs conf b

instance (TxInputBuilder i, TxOutputBuilder o) => TxBuilder (RegularTx i o) where
    toTx conf (RegularTx i o) = do
        (inputs, wits) <- toDraftInputs conf i
        outputs <- toOutputs conf o
        
        let tmp = updateTx def {
                version = 0,
                tx_in = inputs,
                tx_out = outputs,
                tx_witness = wits,
                lock_time = 0
            }

        (inputs, wits') <- toFinalInputs conf tmp 0 i

        let wits = if any (/= nullWitness) wits' then wits' else []

        return $ updateTx tmp {
            tx_in = inputs,
            tx_witness = wits
        }
