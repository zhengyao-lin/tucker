{-# LANGUAGE OverloadedStrings, GADTs, TemplateHaskell #-}

module Tucker.RPC.Parse where

import Language.Haskell.TH

import Data.Aeson.Types
import qualified Data.Text as TXT

import Control.Monad

import qualified Text.Read as READ

import Tucker.Msg

data RPCParam a where
    RPCInt :: RPCParam Int
    RPCBool :: RPCParam Bool
    RPCNumber :: RPCParam Double
    RPCString :: RPCParam String
    RPCHash256 :: RPCParam Hash256
    (:~) :: RPCParam a -> a -> RPCParam a

parseParam :: RPCParam a -> [Value] -> Maybe a
parseParam RPCInt (Number n:_) = return (truncate n)
parseParam RPCInt (Bool True:_) = return 1
parseParam RPCInt (Bool False:_) = return 0

parseParam RPCBool (Bool n:_) = return n
parseParam RPCBool (Number n:_) = return (n /= 0)

parseParam RPCNumber (Number n:_) = return (realToFrac n)
parseParam RPCNumber (Bool True:_) = return 1
parseParam RPCNumber (Bool False:_) = return 0

parseParam RPCString (String n:_) = return (TXT.unpack n)
parseParam RPCHash256 (String h:_) = READ.readMaybe (TXT.unpack h)

parseParam (t :~ d) params = maybe (return d) return (parseParam t params)

parseParam _ _ = fail []

parseNParams :: Int -> Q Exp
parseNParams 0 = do
    proc <- newName "f"
    return (LamE [ VarP proc, WildP ] (AppE (VarE (mkName "return")) (VarE proc)))

parseNParams n = do
    dec <- replicateM n (newName "x")
    proc <- newName "f"
    params <- newName "p"

    let arg1 = VarP proc
        arg2 = TupP (map VarP dec)
        arg3 = VarP params
        one = LitE (IntegerL 1)
        parseParamE = VarE (mkName "parseParam")
        returnE = VarE (mkName "return")
        dropE = VarE (mkName "drop")
    
    let sections = flip concatMap dec $ \name ->
            let s1 = BindS (VarP name) (AppE (AppE parseParamE (VarE name)) (VarE params))
                s2 = BindS (VarP params) (AppE returnE (AppE (AppE dropE one) (VarE params)))
            in [ s1, s2 ]

        last =
            NoBindS $
            AppE returnE $
            foldl (\t n -> AppE t (VarE n)) (VarE proc) dec

        body = DoE (sections ++ [last])

    return (LamE ([ arg1, arg2, arg3 ]) body)

-- parseParams :: (RPCParam a, RPCParam b) -> (a -> b -> RPCCall) -> [Value] -> Maybe RPCCall
-- parseParams (a, b) proc params = do
--     a <- parseParam a params
--     params <- return (drop 1 params)

--     b <- parseParam b params
--     params <- return (drop 1 params)

--     return (proc a b)

genParseParams :: Int -> Q [Dec]
genParseParams n =
    forM [ 1 .. n ] $ \n -> do
        body <- parseNParams n
        return (ValD (VarP (mkName ("parseParams" ++ show n))) (NormalB body) [])
