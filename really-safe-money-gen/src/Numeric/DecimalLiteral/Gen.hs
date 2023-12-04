{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Numeric.DecimalLiteral.Gen where

import Data.GenValidity
import Numeric.DecimalLiteral

instance GenValid DecimalLiteral where
  genValid = DecimalLiteral <$> genValid <*> genValid <*> genValid
  shrinkValid = \case
    DecimalLiteral ms m e ->
      [DecimalLiteral ms' m' e' | (ms', m', e') <- shrinkTriple shrinkSign shrinkValid shrinkValid (ms, m, e)]

shrinkSign :: Maybe Bool -> [Maybe Bool]
shrinkSign = \case
  Nothing -> []
  Just True -> [Nothing]
  Just False -> [Nothing]
