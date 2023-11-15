{-# OPTIONS_GHC -Wno-orphans #-}

module Money.MultiAccount.Gen where

import Data.GenValidity
import Data.GenValidity.Map ()
import Money.Account.Gen ()
import Money.Currency.Gen ()
import Money.MultiAccount

instance (Show currency, Ord currency, GenValid currency) => GenValid (MultiAccount currency) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
