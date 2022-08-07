{-# OPTIONS_GHC -Wno-orphans #-}

module Money.Account.Gen where

import Data.GenValidity
import Money.Account
import Money.Amount.Gen ()

instance GenValid Account where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AccountDistribution where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
