{-# OPTIONS_GHC -Wno-orphans #-}

module Numeric.DecimalLiteral.Gen where

import Data.GenValidity
import Numeric.DecimalLiteral

instance GenValid DecimalLiteral
