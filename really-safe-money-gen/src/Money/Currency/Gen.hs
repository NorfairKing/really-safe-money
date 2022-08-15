{-# OPTIONS_GHC -Wno-orphans #-}

module Money.Currency.Gen where

import Data.GenValidity
import Money.Currency

instance GenValid Currency where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
