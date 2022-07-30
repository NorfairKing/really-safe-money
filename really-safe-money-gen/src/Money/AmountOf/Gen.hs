{-# OPTIONS_GHC -Wno-orphans #-}

module Money.AmountOf.Gen where

import Data.GenValidity
import Money.AmountOf

instance GenValid (Amount currency)
