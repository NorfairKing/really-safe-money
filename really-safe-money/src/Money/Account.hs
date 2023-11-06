{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- === Importing this module
--
-- This module is designed to be imported as follows:
--
-- @
-- import Money.Account (Account)
-- import qualified Money.Account as Account
-- @
--
-- Or, if you have an @Account@ type already, maybe in a
--
-- @
-- import qualified Money.Account as Money (Account)
-- import qualified Money.Account as Account
-- @
module Money.Account
  ( Account (..),
    fromMinimalQuantisations,
    toMinimalQuantisations,
    fromAmount,
    fromDouble,
    toDouble,
    fromRational,
    toRational,
    zero,
    add,
    sum,
    subtract,
    abs,
    multiply,
    distribute,
    AccountDistribution (..),
    fraction,
    Rounding (..),
  )
where

import Control.DeepSeq
import Control.Monad
import Data.Foldable hiding (sum)
import Data.Function
import Data.Int
import Data.Monoid
import Data.Ratio
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Money.Amount (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import Numeric.Natural
import Prelude hiding (abs, fromRational, subtract, sum, toRational)
import qualified Prelude

-- | An account of money. Like 'Amount' but can also be negative.
data Account
  = Positive !Amount
  | Negative !Amount
  deriving (Show, Read, Generic)

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
-- === API Note
--
-- We return 'Integer' because the result does not fit into a 'Word64'
--
-- >>> toMinimalQuantisations (Positive (Amount 1))
-- 1
--
-- >>> toMinimalQuantisations (Negative (Amount 2))
-- -2
toMinimalQuantisations :: Account -> Integer
toMinimalQuantisations account =
  let f = case account of
        Positive _ -> id
        Negative _ -> negate
   in f $ (fromIntegral :: Word64 -> Integer) $ Amount.toMinimalQuantisations (abs account)

-- | Turn an amount of money into a 'Double'.
--
-- WARNING: the result will be infinite or NaN if the quantisation factor is @0@
--
-- >>> toDouble 100 (Positive (Amount 100))
-- 1.0
--
-- >>> toDouble 20 (Negative (Amount 5))
-- -0.25
toDouble :: Word32 -> Account -> Double
toDouble quantisationFactor account =
  let f = case account of
        Positive _ -> id
        Negative _ -> negate
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
-- >>> fromDouble 100 0.01
-- Just (Positive (Amount 1))
--
-- >>> fromDouble 20 (-0.10)
-- Just (Negative (Amount 2))
--
-- >>> fromDouble 100 (-0.001)
-- Nothing
fromDouble :: Word32 -> Double -> Maybe Account
fromDouble quantisationFactor d =
  let d' = Prelude.abs d
      f = if d >= 0 then Positive else Negative
   in f <$> Amount.fromDouble quantisationFactor d'

-- | Turn an amount of money into a 'Rational'.
--
-- WARNING: that the result will be @Account :% 0@ if the quantisation factor is @0@.
--
-- >>> toRational 100 (Positive (Amount 2))
-- 1 % 50
--
-- >>> toRational 20 (Negative (Amount 3))
-- (-3) % 20
toRational :: Word32 -> Account -> Rational
toRational quantisationFactor account =
  let f = case account of
        Positive _ -> id
        Negative _ -> negate
   in f $ Amount.toRational quantisationFactor (abs account)

-- | Turn a 'Rational' into an amount of money.
--
-- This function will fail if the 'Rational':
--
-- * Is NaN (0 :% 0)
-- * Is infinite (1 :% 0) or (-1 :% 0)
-- * Is non-normalised (5 :% 5)
-- * Does represent an integer number of minimal quantisations.
fromRational :: Word32 -> Rational -> Maybe Account
fromRational quantisationFactor r =
  let r' = Prelude.abs r
      f = if r >= 0 then Positive else Negative
   in f <$> Amount.fromRational quantisationFactor r'

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
sum :: forall f. Foldable f => f Account -> Maybe Account
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
-- DistributedZeroAccount
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
      func =
        if a >= zero
          then Positive
          else Negative
   in case Amount.distribute aa af of
        Amount.DistributedIntoZeroChunks -> DistributedIntoZeroChunks
        Amount.DistributedZeroAmount -> DistributedZeroAccount
        Amount.DistributedIntoEqualChunks numberOfChunks chunk ->
          DistributedIntoEqualChunks
            numberOfChunks
            (func chunk)
        Amount.DistributedIntoUnequalChunks numberOfLargerChunks largerChunk numberOfSmallerChunks smallerChunk ->
          DistributedIntoUnequalChunks numberOfLargerChunks (func largerChunk) numberOfSmallerChunks (func smallerChunk)

-- | The result of 'distribute'
data AccountDistribution
  = -- | The second argument was zero.
    DistributedIntoZeroChunks
  | -- | The first argument was a zero amount.
    DistributedZeroAccount
  | -- | Distributed into this many equal chunks of this amount
    DistributedIntoEqualChunks !Word32 !Account
  | -- | Distributed into unequal chunks, this many of the first (larger, in absolute value) amount, and this many of the second (slightly smaller) amount.
    DistributedIntoUnequalChunks !Word32 !Account !Word32 !Account
  deriving (Show, Read, Eq, Generic)

instance Validity AccountDistribution where
  validate ad =
    mconcat
      [ genericValidate ad,
        case ad of
          DistributedIntoUnequalChunks _ a1 _ a2 ->
            declare "The larger chunks are larger in absolute value" $
              abs a1 > abs a2
          _ -> valid
      ]

instance NFData AccountDistribution

-- | Fractional multiplication, see 'Amount.fraction'
--
-- Note that the rounding happens to the absolute value of the fraction.
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
      aa = abs account
      (amount, actualFraction) = Amount.fraction rounding aa af
      func :: Maybe Amount -> Rational -> (Maybe Account, Rational)
      func ma r = case (compare account zero, compare f 0) of
        (EQ, _) -> (Just zero, r)
        (_, EQ) -> (Just zero, 0)
        (GT, GT) -> (Positive <$> ma, r)
        (GT, LT) -> (Negative <$> ma, -r)
        (LT, GT) -> (Negative <$> ma, r)
        (LT, LT) -> (Positive <$> ma, -r)
   in func amount ((realToFrac :: Ratio Natural -> Rational) actualFraction)
