{-# LANGUAGE ConstraintKinds #-}

-- a wrapper for map

module Tucker.Container.Map (
    module Data.HashMap.Strict,
    Constraint, TMap
) where

import Data.Hashable
import Data.HashMap.Strict

type Constraint k = (Hashable k, Eq k)
type TMap = HashMap
