{-# OPTIONS_GHC -Wno-orphans #-}

module Money.ConversionRate.Gen where

import Data.GenValidity
import Data.Ratio
import Money.ConversionRate

instance GenValid ConversionRate where
  genValid = ConversionRate <$> ((%) <$> (succ <$> genValid) <*> (succ <$> genValid))
  shrinkValid = fmap ConversionRate . filter (== 0) . shrinkValid . unConversionRate
