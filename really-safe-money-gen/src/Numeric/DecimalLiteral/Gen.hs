{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Numeric.DecimalLiteral.Gen where

import Data.GenValidity
import Numeric.DecimalLiteral
import Test.QuickCheck

instance GenValid DecimalLiteral where
  genValid =
    oneof
      [ DecimalLiteralInteger <$> genValid <*> genValid,
        DecimalLiteralFractional <$> genValid <*> genValid <*> genValid
      ]
  shrinkValid = \case
    DecimalLiteralInteger ms b ->
      [DecimalLiteralInteger ms' b' | (ms', b') <- shrinkTuple shrinkSign shrinkValid (ms, b)]
    DecimalLiteralFractional ms m e ->
      [DecimalLiteralFractional ms' m' e' | (ms', m', e') <- shrinkTriple shrinkSign shrinkValid shrinkValid (ms, m, e)]

shrinkSign :: Maybe Bool -> [Maybe Bool]
shrinkSign = \case
  Nothing -> []
  Just True -> [Nothing]
  Just False -> [Nothing]
