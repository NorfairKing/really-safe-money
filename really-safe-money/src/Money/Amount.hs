{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports -Wno-dodgy-exports #-}

module Money.Amount
  ( Repr,
    Amount (..),
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

import Control.DeepSeq
import Data.Int
import Data.Typeable
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import GHC.Real (Ratio ((:%)))
import GHC.TypeLits
import Prelude hiding (fromRational, subtract, toRational)
import qualified Prelude

-- | A type-synonym so we only have to change this in one place.
type Repr = Int64

-- | An amount of money of an unspecified currency. May be negative.
--
-- If the currency is statically known, you are better off using 'Money.AmountOf.AmountOf'.
--
-- === Representation
--
-- The underlying representation is 'Int64'.
-- This supports 2^64 (about 1E18) minimal quantisations.
-- For example:
--
-- * 10 quadrillion USD ((much) more than the M1 money supply as of 2022)
-- * 50 quadrillion CHF ((much) more than the M1 money supply as of 2022)
-- * 10 billion BTC (more than the 21 million that can exist)
--
-- === Instances
--
-- The instances that exist are unsurprising, lawful, tested, and implemented
-- using total functions.
--
-- Many instances have been poisoned for good reason.
--
--     * 'Semigroup'
--         * '(<>)' cannot be implemented because 'add' must be able to fail.
--
--     * 'Enum'
--
--         * 'succ' and 'pred' would be partial.
--         * 'fromEnum' would be partial on 32-bit systems.
--
--     * 'Num'
--
--         * '(*)' cannot be implemented because the units don't match.
--         * 'fromInteger' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount.
--         * 'abs' would be wrong for 'minBound'.
--         * 'negate' would be wrong for 'minBound'.
--
--
-- The following instances have not been poisoned because their superclasses
-- instances have already been poisoned.
-- In the very rare case that the type class hierarchy is changed and such an
-- instance could be implemented, we should still not do it because:
--
--
--     * 'Real'
--
--         * 'toRational' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount.
--
--     * 'Integral'
--
--         * 'quot' would be partial (0 divisor)
--         * 'rem' would be partial (0 divisor)
--         * 'div' would be partial (0 divisor)
--         * 'mod' would be partial (0 divisor)
--         * 'quotRem' would be partial (0 divisor)
--         * 'divMod' would be partial (0 divisor)
--         * 'toInteger' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount.
--
--     * 'Fractional'
--
--         * '(/)' cannot be implemented because the units don't match
--         * 'fromRational' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount.
--
--     * 'Floating'
--
--         * 'pi' cannot be represented accurately (and also doesn't mean anything as an amount of money)
--         * 'exp' cannot be implemented because the units don't match
--         * 'log' cannot be implemented because the units don't match
--         * 'sqrt' cannot be implemented because the units don't match
--         * '(**)' cannot be implemented because the units don't match
--         * 'logBase' cannot be implemented because the units don't match
--
--     * 'RealFrac'
--
--         * 'properFraction' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount.
--         * 'truncate' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount.
--         * 'round' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount.
--         * 'ceiling' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount.
--         * 'floor' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount.
--
--     * 'RealFloat', because an amount of money is not represented using a floating-point number.
--     * 'Monoid' could work if there was a 'Semigroup Amount', but there isn't and there shouldn't be.
newtype Amount = Amount
  { unAmount :: Repr
  }
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

instance Validity Amount

instance NFData Amount

instance
  TypeError
    ( 'Text "This would require that Amounts of money are an instance of Enum"
        ':$$: 'Text "Amounts of money must not be an instance of Enum. Don't do this."
        ':$$: 'Text "In particular:"
        ':$$: 'Text "* succ and pred would be partial."
        ':$$: 'Text "* the fromEnum :: Amount -> Int function would be partial on 32-bit systems."
    ) =>
  Enum Amount
  where
  toEnum = undefined
  fromEnum = undefined

instance
  TypeError
    ( 'Text "This would require that Amounts of money are an instance of Bounded"
        ':$$: 'Text "Amounts of money must not be an instance of Bounded. Don't do this."
        ':$$: 'Text "The reasoning is more philosophical than practical:"
        ':$$: 'Text "It is not clear which bound to choose."
        ':$$: 'Text "Setting the bounds equal to the bounds of the representation is surprising if there is a clear bound on the amount of a currency, like in the case of BTC."
        ':$$: 'Text "Setting the bounds equal to the bounds of currency is only possible if there is a clear bound, like in the case of BTC, and that the instance exists at all would be surprising in the case of USD."
    ) =>
  Bounded Amount
  where
  minBound = undefined
  maxBound = undefined

instance
  TypeError
    ( 'Text "This would require that Amounts of money are an instance of Num"
        ':$$: 'Text "Amounts of money must not be an instance of Num. Don't do this."
        ':$$: 'Text "In particular:"
        ':$$: 'Text "* (*) cannot be implemented because the units don't match."
        ':$$: 'Text "* fromInteger cannot be implemented because we don't know the minimal quantisation of an arbitrary amount."
        ':$$: 'Text "* abs would be wrong for minBound."
        ':$$: 'Text "* negate would be wrong for minBound."
    ) =>
  Num Amount
  where
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined
  (-) = undefined

instance
  TypeError
    ( 'Text "This would require that Amounts of money are an instance of Semigroup"
        ':$$: 'Text "Amounts of money must not be an instance of Semigroup. Don't do this."
        ':$$: 'Text "In particular, (<>) cannot be implemented because add must be able to fail."
    ) =>
  Semigroup Amount
  where
  (<>) = undefined

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
fromRational quantisationFactor r
  | isInvalid r = Nothing
  | otherwise = Just $ Amount $ round $ r * fromIntegral quantisationFactor

-- Note that the result will be 'Amount / 0' if the quantisation factor is 0
toRational :: Word32 -> Amount -> Rational
toRational 0 a = fromIntegral (toMinimalQuantisations a) :% 0
toRational quantisationFactor a = fromIntegral (toMinimalQuantisations a) / fromIntegral quantisationFactor

data AdditionFailure
  = -- | Overflow over the maxBound, with the real result
    OverflowMaxbound !Integer
  | -- | Overflow under the minBound, with the real result
    OverflowMinbound !Integer
  deriving (Show, Eq, Generic)

instance Validity AdditionFailure

instance NFData AdditionFailure

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

instance NFData DivisionFailure

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

instance NFData FractionFailure

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
