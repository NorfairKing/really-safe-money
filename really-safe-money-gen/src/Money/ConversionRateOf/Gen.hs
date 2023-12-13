{-# OPTIONS_GHC -Wno-orphans #-}

module Money.ConversionRateOf.Gen where

import Data.GenValidity
import Data.Ratio
import Money.ConversionRate.Gen ()
import Money.ConversionRateOf

instance GenValid (ConversionRateOf from to) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
