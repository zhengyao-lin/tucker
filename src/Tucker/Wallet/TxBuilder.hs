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

    -- sign and generate final inputs
    toFinalInputs :: TCKRConf -> TxPayload -> b -> IO ([TxInput], [TxWitness])

class TxOutputBuilder b where
    toOutputs :: TCKRConf -> b -> IO [TxOutput]

data a :+ b = a :+ b
data RegularTx i o = RegularTx i o
data PayToAddr = PayToAddr Satoshi Address

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

    toFinalInputs conf tx (a :+ b) =
        (\(i1, w1) (i2, w2) -> (i1 ++ i2, w1 ++ w2)) <$>
        toFinalInputs conf tx a <*> toFinalInputs conf tx b

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

        (inputs, wits') <- toFinalInputs conf tmp i

        let wits = if any (/= nullWitness) wits' then wits' else []

        return $ updateTx tmp {
            tx_in = inputs,
            tx_witness = wits
        }
