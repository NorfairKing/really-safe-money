{-# OPTIONS_GHC -Wno-orphans #-}

module Money.ConversionRate.Gen where

import Data.GenValidity
import Money.ConversionRate

instance GenValid ConversionRate
