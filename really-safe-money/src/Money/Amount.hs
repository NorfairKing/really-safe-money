{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports -Wno-dodgy-exports -Wno-unused-imports #-}

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

    -- * Destruction
    toMinimalQuantisations,
    toRatio,
    toDouble,
    toRational,

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

    -- ** Validation functions
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
import Numeric.Natural
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
-- >>> fromRatio 100 (1 % 100)
-- Just (Amount 1)
--
-- >>> fromRatio 100 (1 % 1000)
-- Nothing
fromRatio :: Word32 -> Ratio Natural -> Maybe Amount
fromRatio quantisationFactor r = fromRational quantisationFactor (Prelude.toRational r)

-- | Turn an amount of money into a 'Ratio'.
--
-- WARNING: that the result will be @Amount :% 0@ if the quantisation factor is @0@.
--
-- >>> toRatio 100 (Amount 1)
-- 1 % 100
toRatio :: Word32 -> Amount -> Ratio Natural
toRatio 0 a = fromIntegral (toMinimalQuantisations a) :% 0
toRatio quantisationFactor a =
  (fromIntegral :: Word64 -> Natural) (toMinimalQuantisations a)
    % (fromIntegral :: Word32 -> Natural) quantisationFactor

-- | Turn a 'Double' into an amount of money.
--
-- This function will fail if the 'Double':
--
-- * is @NaN@
-- * is infinite
-- * Is negative
-- * does not represent an integral amount of minimal quantisations
--
--
-- >>> fromDouble 100 0.01
-- Just (Amount 1)
--
-- >>> fromDouble 100 0.001
-- Nothing
fromDouble ::
  -- | The quantisation factor: How many minimal quantisations per unit?
  Word32 ->
  Double ->
  Maybe Amount
fromDouble quantisationFactor d
  | isNaN d = Nothing
  | isInfinite d = Nothing
  | d < 0 = Nothing
  | otherwise =
      let resultDouble :: Double
          resultDouble = d * (fromIntegral :: Word32 -> Double) quantisationFactor
          ceiled :: Word64
          ceiled = (ceiling :: Double -> Word64) resultDouble
          floored :: Word64
          floored = (floor :: Double -> Word64) resultDouble
       in if ceiled == floored
            then Just $ Amount ceiled
            else Nothing

-- | Turn an amount of money into a 'Double'.
--
-- WARNING: the result will be infinite or NaN if the quantisation factor is @0@
--
-- >>> toDouble 100 (Amount 1)
-- 1.0e-2
--
-- >>> toDouble 100 (Amount 100)
-- 1.0
toDouble ::
  -- | The quantisation factor: How many minimal quantisations per unit?
  Word32 ->
  Amount ->
  Double
toDouble quantisationFactor a =
  (fromIntegral :: Word64 -> Double) (toMinimalQuantisations a)
    / (fromIntegral :: Word32 -> Double) quantisationFactor

-- | Turn a 'Rational' into an amount of money.
--
-- This function will fail if the 'Rational':
--
-- * Is NaN (0 :% 0)
-- * Is infinite (1 :% 0) or (-1 :% 0)
-- * Is non-normalised (5 :% 5)
-- * Is negative
-- * Does represent an integer number of minimal quantisations.
--
-- >>> fromRational 100 (1 % 100)
-- Just (Amount 1)
--
-- >>> fromRational 100 (1 % 1000)
-- Nothing
--
-- >>> fromRational 100 (-1 % 100)
-- Nothing
fromRational :: Word32 -> Rational -> Maybe Amount
fromRational quantisationFactor r
  | isInvalid r = Nothing
  | r < 0 = Nothing
  | otherwise =
      let resultRational :: Rational
          resultRational = r * (fromIntegral :: Word32 -> Rational) quantisationFactor
          ceiled :: Word64
          ceiled = (ceiling :: Rational -> Word64) resultRational
          floored :: Word64
          floored = (floor :: Rational -> Word64) resultRational
       in if ceiled == floored
            then Just $ Amount ceiled
            else Nothing

-- | Turn an amount of money into a 'Rational'.
--
-- WARNING: the result will be @Amount :% 0@ if the quantisation factor is @0@.
--
-- >>> toRational 100 (Amount 1)
-- 1 % 100
toRational :: Word32 -> Amount -> Rational
toRational quantisationFactor amount = (Prelude.toRational :: Ratio Natural -> Rational) $ toRatio quantisationFactor amount

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
-- API Note: The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
--
-- >>> multiply 3 (Amount 1)
-- Just (Amount 3)
--
-- >>> multiply 2 (Amount (2 ^ 63))
-- Nothing
multiply ::
  Word32 ->
  Amount ->
  Maybe Amount
multiply s (Amount a) =
  let maxBoundI :: Integer
      maxBoundI = (fromIntegral :: Word64 -> Integer) (maxBound :: Word64)
      r :: Integer
      r = (fromIntegral :: Word32 -> Integer) s * (fromIntegral :: Word64 -> Integer) a
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

-- | Validate that an 'Amount' is strictly positive. I.e. not 'zero'.
validateStrictlyPositive :: Amount -> Validation
validateStrictlyPositive amount = declare "The Amount is strictly positive" $ amount > zero
