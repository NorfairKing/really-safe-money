{-# OPTIONS_GHC -Wno-orphans #-}

module Money.AccountOf.Gen where

import Data.GenValidity
import Money.Account.Gen ()
import Money.AccountOf

instance GenValid (AccountOf currency) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid (AccountDistributionOf currency) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
