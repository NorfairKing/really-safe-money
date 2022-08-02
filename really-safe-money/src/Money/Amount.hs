{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports -Wno-dodgy-exports #-}

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
import qualified Prelude

type Repr = Int64

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
  { unAmount :: Repr
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

toMinimalQuantisations :: Amount -> Repr
toMinimalQuantisations = unAmount

fromMinimalQuantisations :: Repr -> Amount
fromMinimalQuantisations = Amount

fromDouble :: Word32 -> Double -> Maybe Amount
fromDouble quantisationFactor d
  | isNaN d = Nothing
  | isInfinite d = Nothing
  | otherwise =
    let resultDouble :: Double
        resultDouble = d * fromIntegral quantisationFactor
        ceiled = ceiling resultDouble
        floored = floor resultDouble
     in if ceiled == floored
          then Just $ Amount ceiled
          else Nothing

-- Note that the result will be 'NaN' if the quantisation factor is 0
toDouble :: Word32 -> Amount -> Double
toDouble quantisationFactor a = fromIntegral (toMinimalQuantisations a) / fromIntegral quantisationFactor

fromRational :: Word32 -> Rational -> Maybe Amount
fromRational quantisationFactor r =
  Just $ Amount $ round $ r * fromIntegral quantisationFactor

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
      maxBoundI = fromIntegral (maxBound :: Repr) :: Integer
      minBoundI = fromIntegral (minBound :: Repr) :: Integer
      r = i1 + i2
   in if
          | r > maxBoundI -> Left $ OverflowMaxbound r
          | r < minBoundI -> Left $ OverflowMinbound r
          | otherwise -> Right (Amount (fromInteger r))

type SubtractionFailure = AdditionFailure

--
-- WARNING: This function can be used to accidentally subtract two amounts of different currencies.
subtract :: Amount -> Amount -> Either SubtractionFailure Amount
subtract (Amount a1) (Amount a2) =
  let i1 = fromIntegral a1 :: Integer
      i2 = fromIntegral a2 :: Integer
      maxBoundI = fromIntegral (maxBound :: Repr) :: Integer
      minBoundI = fromIntegral (minBound :: Repr) :: Integer
      r = i1 - i2
   in if
          | r > maxBoundI -> Left $ OverflowMaxbound r
          | r < minBoundI -> Left $ OverflowMinbound r
          | otherwise -> Right (Amount (fromInteger r))

type MultiplicationFailure = AdditionFailure

-- API Note: The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
multiply ::
  Int32 ->
  Amount ->
  Either MultiplicationFailure Amount
multiply f (Amount a) =
  let i = fromIntegral a :: Integer
      maxBoundI = fromIntegral (maxBound :: Repr) :: Integer
      minBoundI = fromIntegral (minBound :: Repr) :: Integer
      r = fromIntegral f * fromInteger i
   in if
          | r > maxBoundI -> Left $ OverflowMaxbound r
          | r < minBoundI -> Left $ OverflowMinbound r
          | otherwise -> Right (Amount (fromInteger r))

data DivisionFailure
  = DivideByZero
  deriving (Show, Eq, Generic)

instance Validity DivisionFailure

-- API Note: The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
divide ::
  Amount ->
  Word32 ->
  Either DivisionFailure Amount
divide _ 0 = Left DivideByZero
divide (Amount a) d =
  -- We always round down here, because it is the least surprising.
  let r = a `div` fromIntegral d
   in Right (Amount r)

data FractionFailure = FractionFailure
  deriving (Show, Eq, Generic)

instance Validity FractionFailure

-- Fractional multiplication
fraction ::
  Amount ->
  Rational ->
  (Amount, Rational)
fraction (Amount 0) f = (zero, f)
fraction _ 0 = (zero, 0)
fraction (Amount a) f =
  let theoreticalResult :: Rational
      theoreticalResult = Prelude.toRational a * f
      roundedResult :: Repr
      roundedResult = round theoreticalResult
      actualRate :: Rational
      actualRate = Prelude.toRational roundedResult / Prelude.toRational a
   in (Amount roundedResult, actualRate)
