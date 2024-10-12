{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Account
--
-- === Importing this module
--
-- This module is designed to be imported as follows:
--
-- @
-- import Money.Account (Account)
-- import qualified Money.Account as Account
-- @
module Money.Account
  ( Account (..),

    -- * Construction
    zero,
    fromMinimalQuantisations,
    fromAmount,
    fromDouble,
    fromRational,
    fromRatio,
    fromDecimalLiteral,

    -- * Destruction
    toMinimalQuantisations,
    toDouble,
    toRational,
    toRatio,
    toDecimalLiteral,

    -- * Operations

    -- ** Addition
    add,
    sum,

    -- ** Subtraction
    subtract,

    -- ** Absolute value
    abs,

    -- ** Negation
    Money.Account.negate,

    -- ** Integral multiplication
    multiply,

    -- ** Integral distribution
    distribute,
    AccountDistribution,
    Distribution (..),

    -- ** Fractional multiplication
    Rounding (..),
    fraction,
    fractionRatio,

    -- ** Currency conversions
    rate,
    convert,

    -- * Formatting
    format,
    quantisationFactorFormatString,
  )
where

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.Foldable hiding (sum)
import Data.Function
import Data.Int
import Data.Ratio
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Money.Amount (Amount (..), Distribution (..), Rounding (..), quantisationFactorFormatString)
import qualified Money.Amount as Amount
import Money.ConversionRate (ConversionRate (..))
import Money.QuantisationFactor (QuantisationFactor (..))
import qualified Money.QuantisationFactor as QuantisationFactor
import Numeric.DecimalLiteral (DecimalLiteral (..))
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Numeric.Natural
import Text.Printf
import Prelude hiding (abs, fromRational, negate, subtract, sum, toRational)
import qualified Prelude

-- | An account of money. Like 'Amount' but can also be negative.
data Account
  = Positive !Amount
  | Negative !Amount
  deriving (Show, Read, Data, Generic)

instance Validity Account

instance NFData Account

instance Eq Account where
  (==) = (==) `on` toMinimalQuantisations

instance Ord Account where
  compare = compare `on` toMinimalQuantisations

-- | Turn a number of minimal quantisations into an account.
--
-- This will fail if the integer is not in the range @[- 2^64 .. 2^64]@
--
-- >>> fromMinimalQuantisations 2
-- Just (Positive (Amount 2))
fromMinimalQuantisations :: Integer -> Maybe Account
fromMinimalQuantisations i =
  let maxBoundI :: Integer
      maxBoundI = (toInteger :: Word64 -> Integer) (maxBound :: Word64)
      a :: Integer
      a = (Prelude.abs :: Integer -> Integer) i
   in if a > maxBoundI
        then Nothing
        else
          let w :: Word64
              w = (fromIntegral :: Integer -> Word64) a
              amount :: Amount
              amount = Amount.fromMinimalQuantisations w
           in Just $
                if i >= 0
                  then Positive amount
                  else Negative amount

-- | Turn an amount into a positive account
--
-- >>> fromAmount (Amount 3)
-- Positive (Amount 3)
fromAmount :: Amount -> Account
fromAmount = Positive

-- | Turn an amount into a number of minimal quantisations.
--
-- >>> toMinimalQuantisations (Positive (Amount 1))
-- 1
--
-- >>> toMinimalQuantisations (Negative (Amount 2))
-- -2
--
-- ==== API Note
--
-- We return 'Integer' because the result does not fit into a 'Word64'
toMinimalQuantisations :: Account -> Integer
toMinimalQuantisations account =
  let f = case account of
        Positive _ -> id
        Negative _ -> Prelude.negate
   in f $ (fromIntegral :: Word64 -> Integer) $ Amount.toMinimalQuantisations (abs account)

-- | Turn an amount of money into a 'Double'.
--
-- WARNING: the result will be infinite or NaN if the quantisation factor is @0@
--
-- >>> toDouble (QuantisationFactor 100) (Positive (Amount 100))
-- 1.0
--
-- >>> toDouble (QuantisationFactor 20) (Negative (Amount 5))
-- -0.25
toDouble :: QuantisationFactor -> Account -> Double
toDouble quantisationFactor account =
  let f = case account of
        Positive _ -> id
        Negative _ -> Prelude.negate
   in f $ Amount.toDouble quantisationFactor (abs account)

-- | Turn a 'Double' into an amount of money.
--
-- This function will fail if the 'Double':
--
-- * is @NaN@
-- * is infinite
-- * does not represent an integral amount of minimal quantisations
--
-- WARNING: This function _does not_ roundtrip with toDouble because 'Account' contains more precision than 'Double' does.
--
-- >>> fromDouble (QuantisationFactor 100) 0.01
-- Just (Positive (Amount 1))
--
-- >>> fromDouble (QuantisationFactor 20) (-0.10)
-- Just (Negative (Amount 2))
--
-- >>> fromDouble (QuantisationFactor 100) (-0.001)
-- Nothing
fromDouble :: QuantisationFactor -> Double -> Maybe Account
fromDouble quantisationFactor d =
  let d' = Prelude.abs d
      f = if d >= 0 then Positive else Negative
   in f <$> Amount.fromDouble quantisationFactor d'

-- | Turn an amount of money into a 'Rational'.
--
-- WARNING: that the result will be @Account :% 0@ if the quantisation factor is @0@.
--
-- >>> toRational (QuantisationFactor 100) (Positive (Amount 2))
-- 1 % 50
--
-- >>> toRational (QuantisationFactor 20) (Negative (Amount 3))
-- (-3) % 20
toRational :: QuantisationFactor -> Account -> Rational
toRational quantisationFactor account =
  let f = case account of
        Positive _ -> id
        Negative _ -> Prelude.negate
   in f $ Amount.toRational quantisationFactor (abs account)

-- | Turn a 'Rational' into an amount of money.
--
-- This function will fail if the 'Rational':
--
-- * Is NaN (0 :% 0)
-- * Is infinite (1 :% 0) or (-1 :% 0)
-- * Is non-normalised (5 :% 5)
-- * Does represent an integer number of minimal quantisations.
--
-- >>> fromRational (QuantisationFactor 100) ((-1) % 100)
-- Just (Negative (Amount 1))
--
-- >>> fromRational (QuantisationFactor 100) (1 % 1000)
-- Nothing
fromRational :: QuantisationFactor -> Rational -> Maybe Account
fromRational quantisationFactor r =
  let r' = Prelude.abs r
      f = if r >= 0 then Positive else Negative
   in f <$> Amount.fromRational quantisationFactor r'

-- | Turn a 'Rational' into an amount of money.
--
-- This function will fail if the 'Rational':
--
-- * Is NaN (0 :% 0)
-- * Is infinite (1 :% 0) or (-1 :% 0)
-- * Is non-normalised (5 :% 5)
-- * Does represent an integer number of minimal quantisations.
--
-- >>> fromRatio (QuantisationFactor 100) (2 % 100)
-- Just (Positive (Amount 2))
--
-- >>> fromRatio (QuantisationFactor 100) (1 % 1000)
-- Nothing
fromRatio :: QuantisationFactor -> Ratio Natural -> Maybe Account
fromRatio qf r = Positive <$> Amount.fromRatio qf r

-- | Turn an amount of money into a 'Rational'.
--
-- This will fail if the account is negative.
--
-- >>> toRatio (QuantisationFactor 100) (Positive (Amount 2))
-- Just (1 % 50)
--
-- >>> toRatio (QuantisationFactor 20) (Negative (Amount 3))
-- Nothing
toRatio :: QuantisationFactor -> Account -> Maybe (Ratio Natural)
toRatio _ (Negative _) = Nothing
toRatio qf (Positive a) = Just $ Amount.toRatio qf a

-- | Parse a 'DecimalLiteral' from an 'Account' of a currency with a given quantisation factor.
--
-- This fails when the 'QuantisationFactor' would prevent the account to be
-- represented as a finite decimal literal.
--
-- Note that:
--
-- * The resulting literals always have a sign.
-- * The resulting literals always have digits corresponding to the precision
--   that the quantisation factor prescribes.
--
-- >>> toDecimalLiteral (QuantisationFactor 100) (Positive (Amount 1))
-- Just (DecimalLiteral (Just True) 1 2)
-- >>> toDecimalLiteral (QuantisationFactor 100) (Negative (Amount 100))
-- Just (DecimalLiteral (Just False) 100 2)
-- >>> toDecimalLiteral (QuantisationFactor 20) (Negative (Amount 100))
-- Just (DecimalLiteral (Just False) 500 2)
-- >>> toDecimalLiteral (QuantisationFactor 1) (Positive (Amount 100))
-- Just (DecimalLiteral (Just True) 100 0)
-- >>> toDecimalLiteral (QuantisationFactor 17) (Positive (Amount 100))
-- Nothing
toDecimalLiteral :: QuantisationFactor -> Account -> Maybe DecimalLiteral
toDecimalLiteral qf acc =
  let r = toRational qf acc
   in DecimalLiteral.setSignRequired . DecimalLiteral.setMinimumDigits (QuantisationFactor.digits qf) <$> DecimalLiteral.fromRational r

-- | Convert a 'DecimalLiteral' to an 'Account' of a currency with a given quantisation factor.
--
-- This fails when:
--
-- * the result would be too big to fit into an 'Account'.
-- * the decimal literal is too precise.
--
-- >>> fromDecimalLiteral (QuantisationFactor 100) (DecimalLiteral Nothing 100 0)
-- Just (Positive (Amount 10000))
-- >>> fromDecimalLiteral (QuantisationFactor 100) (DecimalLiteral Nothing 1 3)
-- Nothing
-- >>> fromDecimalLiteral (QuantisationFactor 1000000000) (DecimalLiteral Nothing 1000000000000 0)
-- Nothing
fromDecimalLiteral :: QuantisationFactor -> DecimalLiteral -> Maybe Account
fromDecimalLiteral qf = fromRational qf . DecimalLiteral.toRational

-- | No money in the account
--
-- >>> zero
-- Positive (Amount 0)
zero :: Account
zero = Positive Amount.zero

-- | Add two accounts of money.
--
-- This operation may fail when overflow over either bound occurs.
--
-- WARNING: This function can be used to accidentally add up two accounts of different currencies.
--
-- >>> add (Positive (Amount 3)) (Positive (Amount 4))
-- Just (Positive (Amount 7))
--
-- >>> add (Positive (Amount 3)) (Negative (Amount 4))
-- Just (Negative (Amount 1))
--
-- >>> add (Positive (Amount (2 ^ 64 - 1))) (Positive (Amount 4))
-- Nothing
--
-- >>> add (Negative (Amount (2 ^ 64 - 1))) (Negative (Amount 4))
-- Nothing
add :: Account -> Account -> Maybe Account
add (Positive a1) (Positive a2) = Positive <$> Amount.add a1 a2
add (Negative a1) (Negative a2) = Negative <$> Amount.add a1 a2
add a1 a2 =
  let i1 :: Integer
      i1 = toMinimalQuantisations a1
      i2 :: Integer
      i2 = toMinimalQuantisations a2
      r :: Integer
      r = i1 + i2
   in fromMinimalQuantisations r

-- | Add a number of accounts of money together.
--
-- See 'add'
--
-- Note that this function will fail in the same ways that iteratively 'add' will fail.
--
-- >>> sum [Positive (Amount 4), Negative (Amount 5), Positive (Amount 6)]
-- Just (Positive (Amount 5))
--
-- >>> sum [Negative (Amount (2 ^ 64 - 3)), Negative (Amount 4), Positive (Amount 2)]
-- Nothing
sum :: forall f. (Foldable f) => f Account -> Maybe Account
sum = foldM add zero

-- | Add two accounts of money.
--
-- This operation may fail when overflow over either bound occurs.
--
-- WARNING: This function can be used to accidentally subtract two accounts of different currencies.
--
-- >>> subtract (Positive (Amount 7)) (Negative (Amount 6))
-- Just (Positive (Amount 13))
--
-- >>> subtract (Positive (Amount (2 ^ 64 - 1))) (Negative (Amount 2))
-- Nothing
subtract :: Account -> Account -> Maybe Account
subtract (Positive a1) (Negative a2) = Positive <$> Amount.add a1 a2
subtract (Negative a1) (Positive a2) = Negative <$> Amount.add a1 a2
subtract a1 a2 =
  let i1 :: Integer
      i1 = toMinimalQuantisations a1
      i2 :: Integer
      i2 = toMinimalQuantisations a2
      r :: Integer
      r = i1 - i2
   in fromMinimalQuantisations r

-- | The absolute value of the account
--
-- The 'Account' type has a symmetrical range so this function will always return a correct result.
--
-- Note that this returns an 'Amount' and not an 'Account' because the result is always positive.
--
-- >>> abs (Positive (Amount 4))
-- Amount 4
--
-- >>> abs (Negative (Amount 5))
-- Amount 5
abs :: Account -> Amount
abs = \case
  Negative a -> a
  Positive a -> a

-- | Negate the value of the account
--
-- >>> negate (Positive (Amount 1))
-- Negative (Amount 1)
--
-- >>> negate (Negative (Amount 2))
-- Positive (Amount 2)
negate :: Account -> Account
negate = \case
  Positive a -> Negative a
  Negative a -> Positive a

-- | Multiply an account by an integer scalar
--
-- This operation will fail when overflow over either bound occurs.
--
-- >>> multiply 3 (Negative (Amount 1))
-- Just (Negative (Amount 3))
--
-- >>> multiply (-2) (Positive (Amount (2 ^ 63)))
-- Nothing
multiply :: Int32 -> Account -> Maybe Account
multiply factor account =
  let af = (fromIntegral :: Int32 -> Word32) ((Prelude.abs :: Int32 -> Int32) factor)
      f = case (compare factor 0, compare account zero) of
        (EQ, _) -> const zero
        (_, EQ) -> const zero
        (GT, GT) -> Positive
        (GT, LT) -> Negative
        (LT, GT) -> Negative
        (LT, LT) -> Positive
   in f <$> Amount.multiply af (abs account)

-- | Distribute an amount of money into chunks that are as evenly distributed as possible.
--
-- >>> distribute (Positive (Amount 0)) 1
-- DistributedZero
--
-- >>> distribute (Negative (Amount 2)) 0
-- DistributedIntoZeroChunks
--
-- >>> distribute (Negative (Amount 2)) 2
-- DistributedIntoEqualChunks 2 (Negative (Amount 1))
--
-- >>> distribute (Positive (Amount 11)) 3
-- DistributedIntoUnequalChunks 2 (Positive (Amount 4)) 1 (Positive (Amount 3))
distribute :: Account -> Word16 -> AccountDistribution
distribute a f =
  let aa = abs a
      af = (fromIntegral :: Word16 -> Word32) (Prelude.abs f)
   in case Amount.distribute aa af of
        DistributedIntoZeroChunks -> DistributedIntoZeroChunks
        DistributedZero -> DistributedZero
        DistributedIntoEqualChunks numberOfChunks chunk ->
          if a >= zero
            then DistributedIntoEqualChunks numberOfChunks (Positive chunk)
            else DistributedIntoEqualChunks numberOfChunks (Negative chunk)
        DistributedIntoUnequalChunks numberOfLargerChunks largerChunk numberOfSmallerChunks smallerChunk ->
          if a >= zero
            then DistributedIntoUnequalChunks numberOfLargerChunks (Positive largerChunk) numberOfSmallerChunks (Positive smallerChunk)
            else DistributedIntoUnequalChunks numberOfSmallerChunks (Negative smallerChunk) numberOfLargerChunks (Negative largerChunk)

type AccountDistribution = Amount.Distribution Account

-- | Fractional multiplication, see 'Amount.fraction'
--
-- >>> fraction RoundNearest (Positive (Amount 100)) (1 % 2)
-- (Just (Positive (Amount 50)),1 % 2)
--
-- In this example the given fraction cannot be used to produce an integer number of minimal quantisations, so the actual fraction is rounded (down) to 0.15 instead of 0.1666...
--
-- >>> fraction RoundNearest (Negative (Amount 20)) (1 % 6)
-- (Just (Negative (Amount 3)),3 % 20)
--
-- If instead we ask to round the rate up, we get this result:
--
-- >>> fraction RoundUp (Negative (Amount 20)) (1 % 6)
-- (Just (Negative (Amount 4)),1 % 5)
--
-- In this example the same problem occurs, but we can choose to round down instead.
--
-- >>> fraction RoundNearest (Negative (Amount 21)) (1 % 6)
-- (Just (Negative (Amount 4)),4 % 21)
-- >>> fraction RoundDown (Negative (Amount 21)) (1 % 6)
-- (Just (Negative (Amount 3)),1 % 7)
--
-- In this example the result would be too big:
--
-- >>> fraction RoundNearest (Positive (Amount (2^63))) 3
-- (Nothing,3 % 1)
fraction ::
  Rounding ->
  Account ->
  Rational ->
  (Maybe Account, Rational)
fraction rounding account f =
  let af = (realToFrac :: Rational -> Ratio Natural) ((Prelude.abs :: Rational -> Rational) f)
      ro =
        if f >= 0
          then rounding
          else case rounding of
            RoundUp -> RoundDown
            RoundDown -> RoundUp
            RoundNearest -> RoundNearest
      (ma, ar) = fractionRatio ro account af
      r = (realToFrac :: Ratio Natural -> Rational) ar
   in if f >= 0
        then (ma, r)
        else (negate <$> ma, -r)

-- | Fractional multiplication with a positive fraction, see 'Amount.fraction' and 'Account.fraction'.
--
-- >>> fraction RoundNearest (Positive (Amount 100)) (1 % 2)
-- (Just (Positive (Amount 50)),1 % 2)
-- >>> fraction RoundNearest (Negative (Amount 100)) (1 % 4)
-- (Just (Negative (Amount 25)),1 % 4)
fractionRatio ::
  Rounding ->
  Account ->
  Ratio Natural ->
  (Maybe Account, Ratio Natural)
fractionRatio ro account af =
  let aa = abs account
      (amount, actualFraction) = Amount.fraction ro aa af
      func :: Maybe Amount -> Ratio Natural -> (Maybe Account, Ratio Natural)
      func ma r = case compare account zero of
        EQ -> (Just zero, r)
        GT -> (Positive <$> ma, r)
        LT -> (Negative <$> ma, r)
   in func amount actualFraction

-- | Compute the currency conversion rate between two accounts of money of
-- different currencies.
--
-- The result will be the conversion rate to use to go from the first currency
-- to the second.
-- In other words it will be the number of seconds you get for a first.
--
-- This will fail if the rate is zero or infinite (if either amount is zero),
-- or if the rate is negative (if accounts are of opposite signs).
--
--
-- For example, here we compute the rate to convert 1 USD into 1.10 CHF.
--
-- >>> rate (QuantisationFactor 100) (Positive (Amount 100)) (QuantisationFactor 20) (Positive (Amount 22))
-- Just (ConversionRate {unConversionRate = 11 % 10})
rate ::
  QuantisationFactor ->
  Account ->
  QuantisationFactor ->
  Account ->
  Maybe ConversionRate
rate qf1 a1 qf2 a2 =
  let aa1 = abs a1
      aa2 = abs a2
      mr = Amount.rate qf1 aa1 qf2 aa2
   in case (a1, a2) of
        (Positive _, Positive _) -> mr
        (Positive _, Negative _) -> Nothing
        (Negative _, Positive _) -> Nothing
        (Negative _, Negative _) -> mr

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
-- >>> convert RoundNearest (QuantisationFactor 100) (Negative (Amount 100)) (ConversionRate (11 % 10)) (QuantisationFactor 20)
-- (Just (Negative (Amount 22)),Just (ConversionRate {unConversionRate = 11 % 10}))
convert ::
  -- | Where to round the real ratio to
  Rounding ->
  -- Quantisation factor of the currency of the account
  QuantisationFactor ->
  -- | Account to convert
  Account ->
  -- | Conversion rate to use: Number of units of the following currency per number of units of the previous currency.
  ConversionRate ->
  -- | QuantisationFactor of the target currency
  QuantisationFactor ->
  -- | The amount and the real rate that was used, considering the 'Rounding'
  (Maybe Account, Maybe ConversionRate)
convert r qf1 a cr qf2 =
  let (ma, mr) = Amount.convert r qf1 (abs a) cr qf2
   in case a of
        Positive _ -> (Positive <$> ma, mr)
        Negative _ -> (Negative <$> ma, mr)

-- | Format an account of money without a symbol.
--
-- >>> format (QuantisationFactor 100) (Negative (Amount 1))
-- "-0.01"
--
-- >>> format (QuantisationFactor 20) (Positive (Amount 100))
-- "5.00"
--
-- >>> format (QuantisationFactor 1) (Negative (Amount 1000))
-- "-1000"
--
-- >>> format (QuantisationFactor 100000000) (Positive (Amount 50000))
-- "0.00050000"
--
-- >>> format (QuantisationFactor 0) (Positive (Amount 1))
-- "Infinity"
format :: QuantisationFactor -> Account -> String
format qf a =
  printf (quantisationFactorFormatString qf) (toDouble qf a)
