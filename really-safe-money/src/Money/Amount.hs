{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports -Wno-dodgy-exports -Wno-unused-imports #-}

-- | Amount
--
-- === Importing this module
--
-- This module is designed to be imported as follows:
--
-- @
-- import Money.Amount (Amount)
-- import qualified Money.Amount as Amount
-- @
module Money.Amount
  ( Amount (..),

    -- * Construction
    zero,
    fromMinimalQuantisations,
    fromRatio,
    fromDouble,
    fromRational,
    fromDecimalLiteral,

    -- * Destruction
    toMinimalQuantisations,
    toRatio,
    toDouble,
    toRational,
    toDecimalLiteral,

    -- * Operations

    -- ** Addition
    add,
    sum,

    -- ** Subtraction
    subtract,

    -- ** Integral multiplication
    multiply,

    -- ** Integral distribution
    distribute,
    AmountDistribution,
    Distribution (..),

    -- ** Fractional multiplication
    Rounding (..),
    fraction,

    -- ** Currency conversions
    rate,
    convert,

    -- * Formatting
    format,
    quantisationFactorFormatString,

    -- * Validation functions
    validateStrictlyPositive,
  )
where

import Control.DeepSeq
import Data.Data
import Data.Foldable hiding (sum)
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import GHC.Real (Ratio ((:%)), (%))
import GHC.TypeLits
import Money.ConversionRate (ConversionRate (..))
import qualified Money.ConversionRate as ConversionRate
import Money.QuantisationFactor (QuantisationFactor (..))
import qualified Money.QuantisationFactor as QuantisationFactor
import Numeric.DecimalLiteral (DecimalLiteral (..))
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Numeric.Natural
import Text.Printf
import Prelude hiding (fromRational, subtract, sum, toRational)
import qualified Prelude

-- | An amount of money of an unspecified currency. May not be negative.
--
-- If the currency is statically known, you are better off using 'Money.AmountOf.AmountOf'.
--
-- === Representation
--
-- Negative amounts of money are considered amounts of a different unit, so only positive amounts are allowed.
-- (If you need to represent negative amounts of money, see 'Account'.)
--
-- The underlying representation is 'Word64'.
-- This supports 2^64 (about 1E18) minimal quantisations.
-- For example:
--
-- * 10 quadrillion USD ((much) more than the M1 money supply as of 2023: Less than 20 trillion USD)
-- * 50 quadrillion CHF ((much) more than the M1 money supply as of 2023: Less than 1 trillion CHF)
-- * 10 billion BTC (more than the 21 million that can exist)
--
-- === Instances
--
-- The instances that exist are unsurprising, lawful, tested, and implemented
-- using total functions.
--
-- Many instances have been poisoned for good reason.
--
--     * 'Bounded' because the implementation would be surprising either way.
--     * 'Semigroup'
--         * '(<>)' cannot be implemented because 'add' must be able to fail.
--
--     * 'Enum'
--
--         * 'succ' and 'pred' would be partial.
--         * 'toEnum' would be partial or wrong for negative numbers.
--         * 'fromEnum' would be partial on 32-bit systems.
--
--     * 'Num'
--
--         * '(*)' cannot be implemented because the units don't match.
--         * 'fromInteger' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount and integers can be negative.
--         * 'negate' would be partial or wrong, and makes no sense semantically
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
--         * 'fromRational' cannot be implemented because we don't know the minimal quantisation of an arbitrary amount and Rationals can be negative.
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
newtype Amount = Amount Word64
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Validity Amount

instance NFData Amount

instance
  TypeError
    ( 'Text "This would require that Amounts of money are an instance of Enum"
        ':$$: 'Text "Amounts of money must not be an instance of Enum. Don't do this."
        ':$$: 'Text "In particular:"
        ':$$: 'Text "* succ and pred would be partial."
        ':$$: 'Text "* the fromEnum :: Amount -> Int function would be partial on 32-bit systems."
        ':$$: 'Text "* the toEnum :: Int -> Amount function would fail for negative Ints."
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
        ':$$: 'Text "* fromInteger cannot be implemented because we don't know the minimal quantisation of an arbitrary amount and integers can be negative."
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

-- | No money
--
-- >>> zero
-- Amount 0
zero :: Amount
zero = Amount 0

-- | Turn an amount into a number of minimal quantisations.
--
-- >>> toMinimalQuantisations (Amount 1)
-- 1
toMinimalQuantisations :: Amount -> Word64
toMinimalQuantisations (Amount mq) = mq

-- | Turn a number of minimal quantisations into an amount.
--
-- >>> fromMinimalQuantisations 2
-- Amount 2
fromMinimalQuantisations :: Word64 -> Amount
fromMinimalQuantisations = Amount

-- | Turn a 'Ratio' into an amount of money.
--
-- This function will fail if the 'Ratio':
--
-- * Is NaN (0 :% 0)
-- * Is infinite (1 :% 0) or (-1 :% 0)
-- * Is non-normalised (5 :% 5)
-- * Does not represent an integer number of minimal quantisations.
--
-- >>> fromRatio (QuantisationFactor 100) (1 % 100)
-- Just (Amount 1)
--
-- >>> fromRatio (QuantisationFactor 100) (1 % 1000)
-- Nothing
fromRatio :: QuantisationFactor -> Ratio Natural -> Maybe Amount
fromRatio quantisationFactor r = fromRational quantisationFactor (Prelude.toRational r)

-- | Turn an amount of money into a 'Ratio'.
--
-- WARNING: the result will be @Amount :% 0@ if the quantisation factor is @0@.
--
-- >>> toRatio (QuantisationFactor 100) (Amount 1)
-- 1 % 100
toRatio :: QuantisationFactor -> Amount -> Ratio Natural
toRatio (QuantisationFactor 0) a = fromIntegral (toMinimalQuantisations a) :% 0
toRatio (QuantisationFactor quantisationFactor) a =
  (fromIntegral :: Word64 -> Natural) (toMinimalQuantisations a)
    % (fromIntegral :: Word32 -> Natural) quantisationFactor

-- | Turn a 'Double' into an amount of money.
--
-- This function will fail if the 'Double':
--
-- * is @NaN@
-- * is infinite
-- * Is negative
-- * Is really large
-- * does not represent an integral amount of minimal quantisations
--
--
-- >>> fromDouble (QuantisationFactor 100) 0.01
-- Just (Amount 1)
--
-- >>> fromDouble (QuantisationFactor 100) 0.001
-- Nothing
--
-- >>> fromDouble (QuantisationFactor 100) 20E62
-- Nothing
fromDouble ::
  -- | The quantisation factor: How many minimal quantisations per unit?
  QuantisationFactor ->
  Double ->
  Maybe Amount
fromDouble (QuantisationFactor qf) d
  | d < 0 = Nothing
  | otherwise =
      let resultDouble :: Double
          resultDouble = d * (fromIntegral :: Word32 -> Double) qf
       in go resultDouble
  where
    go resultDouble
      | isNaN d = Nothing
      | isInfinite d = Nothing
      | otherwise =
          -- Shortcut for numbers that are way too big anyway
          -- so that we don't have to compute the according 'Natural' values.
          if exponent resultDouble > 65
            then Nothing
            else
              let ceiled :: Natural
                  ceiled = (ceiling :: Double -> Natural) resultDouble
                  floored :: Natural
                  floored = (floor :: Double -> Natural) resultDouble
               in if ceiled == floored
                    then
                      if ceiled > (fromIntegral :: Word64 -> Natural) (maxBound :: Word64)
                        then Nothing
                        else Just $ Amount (fromIntegral ceiled)
                    else Nothing

-- | Turn an amount of money into a 'Double'.
--
-- WARNING: the result will be infinite or NaN if the quantisation factor is @0@
--
-- >>> toDouble (QuantisationFactor 100) (Amount 1)
-- 1.0e-2
--
-- >>> toDouble (QuantisationFactor 100) (Amount 100)
-- 1.0
toDouble ::
  -- | The quantisation factor: How many minimal quantisations per unit?
  QuantisationFactor ->
  Amount ->
  Double
toDouble (QuantisationFactor qf) a =
  (fromIntegral :: Word64 -> Double) (toMinimalQuantisations a)
    / (fromIntegral :: Word32 -> Double) qf

-- | Turn a 'Rational' into an amount of money.
--
-- This function will fail if the 'Rational':
--
-- * Is NaN (0 :% 0)
-- * Is infinite (1 :% 0) or (-1 :% 0)
-- * Is non-normalised (5 :% 5)
-- * Is negative
-- * Does represent an integer number of minimal quantisations.
-- * Is too big
--
-- >>> fromRational (QuantisationFactor 100) (1 % 100)
-- Just (Amount 1)
--
-- >>> fromRational (QuantisationFactor 100) (1 % 1000)
-- Nothing
--
-- >>> fromRational (QuantisationFactor 100) (-1 % 100)
-- Nothing
--
-- >>> fromRational (QuantisationFactor 100) (200000000000000000 % 1)
-- Nothing
fromRational :: QuantisationFactor -> Rational -> Maybe Amount
fromRational (QuantisationFactor qf) r
  | isInvalid r = Nothing
  | r < 0 = Nothing
  | otherwise =
      let resultRational :: Rational
          resultRational = r * (fromIntegral :: Word32 -> Rational) qf
          ceiled :: Natural
          ceiled = (ceiling :: Rational -> Natural) resultRational
          floored :: Natural
          floored = (floor :: Rational -> Natural) resultRational
       in if ceiled == floored
            then
              if ceiled > (fromIntegral :: Word64 -> Natural) (maxBound :: Word64)
                then Nothing
                else Just $ Amount (fromIntegral ceiled)
            else Nothing

-- | Turn an amount of money into a 'Rational'.
--
-- WARNING: the result will be @Amount :% 0@ if the quantisation factor is @0@.
--
-- >>> toRational (QuantisationFactor 100) (Amount 1)
-- 1 % 100
toRational :: QuantisationFactor -> Amount -> Rational
toRational quantisationFactor amount = (Prelude.toRational :: Ratio Natural -> Rational) $ toRatio quantisationFactor amount

-- | Parse a 'DecimalLiteral' from an 'Amount' of a currency with a given quantisation factor.
--
-- This fails when the 'QuantisationFactor' would prevent the account to be
-- represented as a finite decimal literal.
--
-- Note that:
--
-- * The resulting literals always have a (positive) sign.
-- * The resulting literals always have digits corresponding to the precision
--   that the quantisation factor prescribes.
--
-- >>> toDecimalLiteral (QuantisationFactor 100) (Amount 1)
-- Just (DecimalLiteral (Just True) 1 2)
-- >>> toDecimalLiteral (QuantisationFactor 100) (Amount 100)
-- Just (DecimalLiteral (Just True) 100 2)
-- >>> toDecimalLiteral (QuantisationFactor 20) (Amount 100)
-- Just (DecimalLiteral (Just True) 500 2)
-- >>> toDecimalLiteral (QuantisationFactor 1) (Amount 100)
-- Just (DecimalLiteral (Just True) 100 0)
-- >>> toDecimalLiteral (QuantisationFactor 17) (Amount 100)
-- Nothing
toDecimalLiteral :: QuantisationFactor -> Amount -> Maybe DecimalLiteral
toDecimalLiteral qf acc =
  let r = toRational qf acc
   in DecimalLiteral.setSignRequired . DecimalLiteral.setMinimumDigits (QuantisationFactor.digits qf) <$> DecimalLiteral.fromRational r

-- | Convert a 'DecimalLiteral' to an 'Amount' of a currency with a given quantisation factor.
--
-- This fails when:
--
-- * the result would be too big to fit into an 'Amount'.
-- * the decimal literal is too precise.
--
-- >>> fromDecimalLiteral (QuantisationFactor 100) (DecimalLiteral Nothing 100 0)
-- Just (Amount 10000)
-- >>> fromDecimalLiteral (QuantisationFactor 100) (DecimalLiteral Nothing 1 3)
-- Nothing
-- >>> fromDecimalLiteral (QuantisationFactor 1000000000) (DecimalLiteral Nothing 1000000000000 0)
-- Nothing
fromDecimalLiteral :: QuantisationFactor -> DecimalLiteral -> Maybe Amount
fromDecimalLiteral qf = fromRational qf . DecimalLiteral.toRational

-- | Add two amounts of money.
--
-- This operation may fail when overflow over the maxBound occurs.
--
-- WARNING: This function can be used to accidentally add up two amounts of different currencies.
--
-- >>> add (Amount 2) (Amount 3)
-- Just (Amount 5)
--
-- >>> add (Amount (2 ^ 64 - 1)) (Amount 1)
-- Nothing
add :: Amount -> Amount -> Maybe Amount
add (Amount a1) (Amount a2) =
  let i1 :: Integer
      i1 = (fromIntegral :: Word64 -> Integer) a1
      i2 :: Integer
      i2 = (fromIntegral :: Word64 -> Integer) a2
      maxBoundI :: Integer
      maxBoundI = fromIntegral (maxBound :: Word64)
      r :: Integer
      r = i1 + i2
   in if r > maxBoundI
        then Nothing
        else Just (Amount ((fromInteger :: Integer -> Word64) r))

-- | Add a number of amounts of money together.
--
-- See 'add'
--
-- >>> sum [Amount 4, Amount 5, Amount 6]
-- Just (Amount 15)
--
-- >>> sum [Amount (2 ^ 64 - 3), Amount 1, Amount 2]
-- Nothing
sum :: forall f. Foldable f => f Amount -> Maybe Amount
sum l =
  let maxBoundI :: Integer
      maxBoundI = fromIntegral (maxBound :: Word64)
      r :: Integer
      r = foldl' (\acc a -> (toInteger :: Word64 -> Integer) (toMinimalQuantisations a) + acc) 0 l
   in if r > maxBoundI
        then Nothing
        else Just (Amount ((fromInteger :: Integer -> Word64) r))

-- | Add two amounts of money.
--
-- This operation may fail when the amount becomes negative.
--
-- WARNING: This function can be used to accidentally subtract two amounts of different currencies.
--
-- >>> subtract (Amount 7) (Amount 6)
-- Just (Amount 1)
--
-- >>> subtract (Amount 8) (Amount 9)
-- Nothing
subtract :: Amount -> Amount -> Maybe Amount
subtract (Amount a1) (Amount a2) =
  let i1 :: Integer
      i1 = (fromIntegral :: Word64 -> Integer) a1
      i2 :: Integer
      i2 = (fromIntegral :: Word64 -> Integer) a2
      r :: Integer
      r = i1 - i2
   in if r < 0
        then Nothing
        else Just (Amount ((fromInteger :: Integer -> Word64) r))

-- | Multiply an amount of money by an integer scalar
--
-- This operation may fail when overflow over the maxBound occurs.
--
-- >>> multiply 3 (Amount 1)
-- Just (Amount 3)
--
-- >>> multiply 2 (Amount (2 ^ 63))
-- Nothing
--
-- ==== API Note
--
-- The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
multiply ::
  Word32 ->
  Amount ->
  Maybe Amount
multiply f (Amount a) =
  let maxBoundI :: Integer
      maxBoundI = (fromIntegral :: Word64 -> Integer) (maxBound :: Word64)
      r :: Integer
      r = (fromIntegral :: Word32 -> Integer) f * (fromIntegral :: Word64 -> Integer) a
   in if r > maxBoundI
        then Nothing
        else Just (Amount ((fromInteger :: Integer -> Word64) r))

-- | Distribute an amount of money into chunks that are as evenly distributed as possible.
--
-- >>> distribute (Amount 0) 1
-- DistributedZero
--
-- >>> distribute (Amount 2) 0
-- DistributedIntoZeroChunks
--
-- >>> distribute (Amount 2) 2
-- DistributedIntoEqualChunks 2 (Amount 1)
--
-- >>> distribute (Amount 11) 3
-- DistributedIntoUnequalChunks 2 (Amount 4) 1 (Amount 3)
distribute :: Amount -> Word32 -> AmountDistribution
distribute (Amount 0) _ = DistributedZero
distribute _ 0 = DistributedIntoZeroChunks
distribute (Amount a) f =
  let smallerChunkSize, rest :: Word64
      (smallerChunkSize, rest) = divMod a ((fromIntegral :: Word32 -> Word64) f)
      smallerChunk :: Amount
      smallerChunk = Amount smallerChunkSize
   in if rest == 0
        then DistributedIntoEqualChunks f smallerChunk
        else
          let -- This 'fromIntegral' is theoretically not safe, but it's
              -- necessarily smaller than f so it will fit.
              numberOfLargerChunks :: Word32
              numberOfLargerChunks = (fromIntegral :: Word64 -> Word32) rest
              numberOfSmallerChunks :: Word32
              numberOfSmallerChunks = f - numberOfLargerChunks
              largerChunk :: Amount
              largerChunk = Amount $ succ smallerChunkSize
           in DistributedIntoUnequalChunks
                numberOfLargerChunks
                largerChunk
                numberOfSmallerChunks
                smallerChunk

-- | The result of 'distribute'
type AmountDistribution = Distribution Amount

data Distribution amount
  = -- | The second argument was zero.
    DistributedIntoZeroChunks
  | -- | The first argument was a zero amount.
    DistributedZero
  | -- | Distributed into this many equal chunks of this amount
    DistributedIntoEqualChunks !Word32 !amount
  | -- | Distributed into unequal chunks, this many of the first (larger) amount, and this many of the second (slightly smaller) amount.
    DistributedIntoUnequalChunks !Word32 !amount !Word32 !amount
  deriving (Show, Read, Eq, Generic)

instance (Validity amount, Ord amount) => Validity (Distribution amount) where
  validate ad =
    mconcat
      [ genericValidate ad,
        case ad of
          DistributedIntoUnequalChunks _ a1 _ a2 ->
            declare "The larger chunks are larger" $
              a1 > a2
          _ -> valid
      ]

instance NFData amount => NFData (Distribution amount)

-- | Fractional multiplication
--
-- Multiply an amount by a dimensionless fraction.
-- If the given rate cannot be used to produce an integer number of minimal
-- quantisations, the closest possible rate that can is used and returned
-- instead.
-- The rounding you pass to this function determines in which direction the
-- given rate will be rounded to produce the actual rate.
--
-- This function will fail to produce an 'Amount' if the result would be too big.
--
--
-- In this example the actual fraction equals the given fraction:
--
-- >>> fraction RoundNearest (Amount 100) (1 % 2)
-- (Just (Amount 50),1 % 2)
--
-- In this example the given fraction cannot be used to produce an integer number of minimal quantisations, so the actual fraction is rounded (down) to 0.15 instead of 0.1666...
--
-- >>> fraction RoundNearest (Amount 20) (1 % 6)
-- (Just (Amount 3),3 % 20)
--
-- If instead we ask to round the rate up, we get this result:
--
-- >>> fraction RoundUp (Amount 20) (1 % 6)
-- (Just (Amount 4),1 % 5)
--
-- In this example the same problem occurs, but we can choose to round down instead.
--
-- >>> fraction RoundNearest (Amount 21) (1 % 6)
-- (Just (Amount 4),4 % 21)
-- >>> fraction RoundDown (Amount 21) (1 % 6)
-- (Just (Amount 3),1 % 7)
--
-- In this example the result would be too big:
--
-- >>> fraction RoundNearest (Amount (2^63)) 3
-- (Nothing,3 % 1)
fraction ::
  -- | Where to round the real ratio to
  Rounding ->
  -- | Amount to multiply
  Amount ->
  -- | Fraction to multiply by
  Ratio Natural ->
  -- | The amount and the real rate that was used, considering the 'Rounding'
  (Maybe Amount, Ratio Natural)
fraction _ (Amount 0) f = (Just zero, f)
fraction _ _ 0 = (Just zero, 0)
fraction r (Amount a) f =
  let amountAsRatio :: Ratio Natural
      amountAsRatio = (fromIntegral :: Word64 -> Ratio Natural) a
      theoreticalResult :: Ratio Natural
      theoreticalResult = amountAsRatio * f
      rounder :: Ratio Natural -> Natural
      rounder = case r of
        RoundUp -> ceiling
        RoundDown -> floor
        RoundNearest -> round
      roundedResult :: Natural
      roundedResult = rounder theoreticalResult
      actualRate :: Ratio Natural
      actualRate =
        roundedResult
          % (fromIntegral :: Word64 -> Natural) a
      maxBoundN :: Natural
      maxBoundN = fromIntegral (maxBound :: Word64)
      result = Amount ((fromIntegral :: Natural -> Word64) roundedResult)
   in ( if roundedResult > maxBoundN
          then Nothing
          else Just result,
        actualRate
      )

data Rounding
  = -- | Round up, with 'ceiling'
    RoundUp
  | -- | Round down, with 'floor'
    RoundDown
  | -- | Round to the nearest value, with 'round'
    RoundNearest
  deriving (Show, Eq, Generic)

instance Validity Rounding

instance NFData Rounding

-- | Compute the currency conversion rate between two amounts of money of
-- different currencies.
--
-- The result will be the conversion rate to use to go from the first currency
-- to the second.
-- In other words it will be the number of seconds you get for a first.
--
-- This will fail if the rate is zero or infinite (if either amount is zero).
--
--
--
-- For example, here we compute the rate to convert 1 USD into 1.10 CHF.
--
-- >>> rate (QuantisationFactor 100) (Amount 100) (QuantisationFactor 20) (Amount 22)
-- Just (ConversionRate {unConversionRate = 11 % 10})
rate :: QuantisationFactor -> Amount -> QuantisationFactor -> Amount -> Maybe ConversionRate
rate _ (Amount 0) _ _ = Nothing
rate _ _ _ (Amount 0) = Nothing
rate (QuantisationFactor qf1) (Amount a1) (QuantisationFactor qf2) (Amount a2) =
  ConversionRate.fromRatio $
    (fromIntegral a2 * fromIntegral qf1)
      % (fromIntegral a1 * fromIntegral qf2)

-- | Convert an amount of money of one currency into an amount of money of
-- another currency using a conversion rate.
--
-- Note that this will use 'fraction' under the hood but you must not use the
-- 'fraction' function with 'ConversionRate's directly.
-- Indeed, the fraction contained in the 'ConversionRate' has a different
-- _unit_ than a unitless fraction.
--
-- This will fail to produce an amount if the amount would be too big.
-- This will fail to produce a conversion rate if it would be zero.
--
--
-- For example, here we convert 1 USD into 1.10 CHF with a conversion rate of
-- 1.1 (with no rounding of the conversion rate necessary):
--
-- >>> convert RoundNearest (QuantisationFactor 100) (Amount 100) (ConversionRate (11 % 10)) (QuantisationFactor 20)
-- (Just (Amount 22),Just (ConversionRate {unConversionRate = 11 % 10}))
convert ::
  Rounding ->
  -- | Where to round the real ratio to
  QuantisationFactor ->
  -- | Amount to multiply
  Amount ->
  -- | Conversion rate to use: Number of units of the following currency per number of units of the previous currency.
  ConversionRate ->
  QuantisationFactor ->
  -- | The amount and the real rate that was used, considering the 'Rounding'
  (Maybe Amount, Maybe ConversionRate)
convert
  rounding
  (QuantisationFactor qf1)
  a
  (ConversionRate r)
  (QuantisationFactor qf2) =
    let qf1r = Prelude.fromIntegral qf1
        qf2r = Prelude.fromIntegral qf2
        (ma, ar) = fraction rounding a (r * qf2r / qf1r)
     in (ma, ConversionRate.fromRatio (ar * qf1r / qf2r))

-- | Format an amount of money without a symbol.
--
-- >>> format (QuantisationFactor 100) (Amount 1)
-- "0.01"
--
-- >>> format (QuantisationFactor 20) (Amount 10)
-- "0.50"
--
-- >>> format (QuantisationFactor 1) (Amount 100)
-- "100"
--
-- >>> format (QuantisationFactor 100000000) (Amount 500)
-- "0.00000500"
--
-- >>> format (QuantisationFactor 0) (Amount 1)
-- "Infinity"
format :: QuantisationFactor -> Amount -> String
format qf a =
  printf (quantisationFactorFormatString qf) (toDouble qf a)

-- | Produce a printf-style format string for a currency with a given quantisation factor.
--
-- >>> quantisationFactorFormatString (QuantisationFactor 100000000)
-- "%0.8f"
--
-- >>> quantisationFactorFormatString (QuantisationFactor 100)
-- "%0.2f"
--
-- >>> quantisationFactorFormatString (QuantisationFactor 20)
-- "%0.2f"
--
-- >>> quantisationFactorFormatString (QuantisationFactor 1)
-- "%0.0f"
quantisationFactorFormatString :: QuantisationFactor -> String
quantisationFactorFormatString (QuantisationFactor 0) = "%f"
quantisationFactorFormatString (QuantisationFactor qf) =
  let decimals :: Int
      decimals = ceiling $ logBase 10 (fromIntegral qf :: Float)
   in printf "%%0.%df" decimals

-- | Validate that an 'Amount' is strictly positive. I.e. not 'zero'.
validateStrictlyPositive :: Amount -> Validation
validateStrictlyPositive amount = declare "The Amount is strictly positive" $ amount > zero
