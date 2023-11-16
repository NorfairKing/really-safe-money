{-# OPTIONS_GHC -Wno-orphans #-}

module Money.MultiAccount.Gen where

import Data.GenValidity
import Data.GenValidity.Map
import qualified Money.Account as Account
import Money.Account.Gen ()
import Money.Currency.Gen ()
import Money.MultiAccount
import Test.QuickCheck

instance (Show currency, Ord currency, GenValid currency) => GenValid (MultiAccount currency) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = MultiAccount <$> genMapOf ((,) <$> genValid <*> (genValid `suchThat` (/= Account.zero)))
