{-# OPTIONS_GHC -Wno-orphans #-}

module Numeric.DecimalLiteral.Gen where

import Data.GenValidity
import Data.GenValidity.Scientific ()
import Data.Int
import Data.Scientific
import Numeric.DecimalLiteral
import Test.QuickCheck

instance GenValid DecimalLiteral where
  -- We only generate small (in absolute value) Scientific values
  genValid =
    DecimalLiteral
      <$> genValid
      <*> genValid
      <*> (scientific <$> genValid <*> ((fromIntegral :: Int8 -> Int) <$> (genValid :: Gen Int8)))
