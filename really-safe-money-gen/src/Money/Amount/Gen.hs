{-# OPTIONS_GHC -Wno-orphans #-}

module Money.Amount.Gen where

import Data.GenValidity
import Money.Amount

instance GenValid Amount where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AmountDistribution where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
