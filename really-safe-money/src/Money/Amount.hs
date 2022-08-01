{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Money.Amount
  ( Amount (..),
    zero,
    toMinimalQuantisations,
    fromMinimalQuantisations,
    fromDouble,
    toDouble,
    fromRational,
    toRational,
    add,
    AdditionFailure (..),
    subtract,
    SubtractionFailure (..),
    multiply,
    MultiplicationFailure (..),
    divide,
    DivisionFailure (..),
    fraction,
    FractionFailure (..),
  )
where

import Data.Int
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits
import Prelude hiding (fromRational, subtract, toRational)

-- | An amount of money of an unspecified currency. May be negative.
--
-- If the currency is statically known, you are better off using 'AmountOf' in 'Money.AmountOf'.
--
-- The underlying representation is 'Int64'.
-- This supports 2^64 (about 1E18) minimal quantisations.
-- For example:
--
-- * 10 quadrillion USD ((much) more than the M1 money supply as of 2022)
-- * 50 quadrillion CHF ((much) more than the M1 money supply as of 2022)
-- * 10 billion BTC (more than the 21 million that can exist)
newtype Amount = Amount
  { unAmount :: Int64
  }
  deriving (Show, Eq, Generic)

instance Validity Amount

instance
  TypeError
    ( 'Text "This would require that Amounts of money are an instance of Num"
        ':$$: 'Text "Amounts of money must not be an instance of Num. Don't do this."
    ) =>
  Num Amount
  where
  (+) = error "unreachable"
  (*) = error "unreachable"
  abs = error "unreachable"
  signum = error "unreachable"
  fromInteger = error "unreachable"
  negate = error "unreachable"
  (-) = error "unreachable"

zero :: Amount
zero = Amount 0

toMinimalQuantisations :: Amount -> Int64
toMinimalQuantisations = unAmount

fromMinimalQuantisations :: Int64 -> Amount
fromMinimalQuantisations = Amount

fromDouble :: Word32 -> Double -> Maybe Amount
fromDouble quantisationFactor d = Just $ Amount $ round d * fromIntegral quantisationFactor

toDouble :: Word32 -> Amount -> Double
toDouble quantisationFactor a = fromIntegral (toMinimalQuantisations a) / fromIntegral quantisationFactor

fromRational :: Word32 -> Rational -> Maybe Amount
fromRational quantisationFactor r = Just $ Amount $ round r * fromIntegral quantisationFactor

toRational :: Word32 -> Amount -> Rational
toRational quantisationFactor a = fromIntegral (toMinimalQuantisations a) / fromIntegral quantisationFactor

data AdditionFailure
  = -- | Overflow over the maxBound, with the real result
    OverflowMaxbound !Integer
  | -- | Overflow under the minBound, with the real result
    OverflowMinbound !Integer
  deriving (Show, Eq, Generic)

instance Validity AdditionFailure

-- | Add two amounts of money.
--
-- This operation may fail with an 'AdditionFailure' for the following reasons:
--
-- TODO
--
-- WARNING: This function can be used to accidentally add up two amounts of different currencies.
add :: Amount -> Amount -> Either AdditionFailure Amount
add (Amount a1) (Amount a2) =
  let i1 = fromIntegral a1 :: Integer
      i2 = fromIntegral a2 :: Integer
      maxBoundI = fromIntegral (maxBound :: Int64) :: Integer
      minBoundI = fromIntegral (minBound :: Int64) :: Integer
      i = i1 + i2
   in if
          | i > maxBoundI -> Left $ OverflowMaxbound i
          | i < minBoundI -> Left $ OverflowMinbound i
          | otherwise -> Right (Amount (fromInteger i))

data SubtractionFailure = SubtractionFailure
  deriving (Show, Eq, Generic)

instance Validity SubtractionFailure

--
-- WARNING: This function can be used to accidentally subtract two amounts of different currencies.
subtract :: Amount -> Amount -> Either SubtractionFailure Amount
subtract = undefined

data MultiplicationFailure = MultiplicationFailure
  deriving (Show, Eq, Generic)

instance Validity MultiplicationFailure

-- API Note: The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
multiply ::
  Int32 ->
  Amount ->
  Either MultiplicationFailure Amount
multiply = undefined

data DivisionFailure = DivisionFailure
  deriving (Show, Eq, Generic)

instance Validity DivisionFailure

-- API Note: The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
divide ::
  Amount ->
  Int32 ->
  Either DivisionFailure Amount
divide = undefined

data FractionFailure = FractionFailure
  deriving (Show, Eq, Generic)

instance Validity FractionFailure

fraction ::
  Amount ->
  Rational ->
  Either FractionFailure (Amount, Rational)
fraction = undefined
