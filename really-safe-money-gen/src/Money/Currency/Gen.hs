{-# OPTIONS_GHC -Wno-orphans #-}

module Money.Currency.Gen where

import Data.GenValidity
import Money.Currency
import Money.QuantisationFactor.Gen ()

instance GenValid Currency where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
