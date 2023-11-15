{-# OPTIONS_GHC -Wno-orphans #-}

module Money.MultiAmount.Gen where

import Data.GenValidity
import Data.GenValidity.Map ()
import Money.Amount.Gen ()
import Money.Currency.Gen ()
import Money.MultiAmount

instance (Show currency, Ord currency, GenValid currency) => GenValid (MultiAmount currency) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
