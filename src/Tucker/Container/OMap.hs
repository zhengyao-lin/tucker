{-# LANGUAGE ConstraintKinds #-}

-- a wrapper for ordered map

module Tucker.Container.OMap (
    module Data.HashMap.Strict.InsOrd,
    Constraint, OMap
) where

import Data.Hashable
import Data.HashMap.Strict.InsOrd

type Constraint k = (Hashable k, Eq k)
type OMap = InsOrdHashMap
