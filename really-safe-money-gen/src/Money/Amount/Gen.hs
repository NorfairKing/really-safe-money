{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Money.Amount.Gen where

import Data.GenValidity
import Money.Amount
import Test.QuickCheck

instance GenValid Amount where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid amount, Ord amount) => GenValid (Distribution amount) where
  genValid =
    oneof
      [ elements [DistributedIntoZeroChunks, DistributedZero],
        DistributedIntoEqualChunks <$> genValid <*> genValid,
        do
          a1 <- genValid
          a2 <- genValid
          let bigger = max a1 a2
          let smaller = min a1 a2
          DistributedIntoUnequalChunks <$> genValid <*> pure bigger <*> genValid <*> pure smaller
      ]
  shrinkValid = \case
    DistributedIntoZeroChunks -> []
    DistributedZero -> []
    DistributedIntoEqualChunks w a -> [DistributedIntoEqualChunks w' a' | (w', a') <- shrinkValid (w, a)]
    DistributedIntoUnequalChunks wl al ws as ->
      DistributedIntoEqualChunks wl al
        : DistributedIntoEqualChunks ws as
        : [DistributedIntoUnequalChunks wl' al' ws' as' | (wl', al', ws', as') <- shrinkValid (wl, al, ws, as), wl' >= ws']

instance GenValid Rounding where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
