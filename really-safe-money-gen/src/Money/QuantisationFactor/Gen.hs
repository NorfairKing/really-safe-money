{-# OPTIONS_GHC -Wno-orphans #-}

module Money.QuantisationFactor.Gen where

import Data.GenValidity
import Money.QuantisationFactor
import Test.QuickCheck

instance GenValid QuantisationFactor where
  genValid = QuantisationFactor <$> genValid `suchThat` (/= 0)
  shrinkValid = fmap QuantisationFactor . filter (/= 0) . shrinkValid . unQuantisationFactor
