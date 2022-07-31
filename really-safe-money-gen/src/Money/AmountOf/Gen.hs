{-# OPTIONS_GHC -Wno-orphans #-}

module Money.AmountOf.Gen where

import Data.GenValidity
import Money.Amount.Gen ()
import Money.AmountOf

instance GenValid (AmountOf currency)
