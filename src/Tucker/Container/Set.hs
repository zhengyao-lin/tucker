{-# LANGUAGE ConstraintKinds #-}

-- a wrapper for set

module Tucker.Container.Set (
    module Data.HashSet,
    Constraint, TSet
) where

import Data.HashSet
import Data.Hashable

type Constraint k = (Hashable k, Eq k)
type TSet = HashSet
