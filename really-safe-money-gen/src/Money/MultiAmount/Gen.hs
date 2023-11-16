{-# OPTIONS_GHC -Wno-orphans #-}

module Money.MultiAmount.Gen where

import Data.GenValidity
import Data.GenValidity.Map
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Money.Currency.Gen ()
import Money.MultiAmount
import Test.QuickCheck

instance (Show currency, Ord currency, GenValid currency) => GenValid (MultiAmount currency) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = MultiAmount <$> genMapOf ((,) <$> genValid <*> (genValid `suchThat` (/= Amount.zero)))
