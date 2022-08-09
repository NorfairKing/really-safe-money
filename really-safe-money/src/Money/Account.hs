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
    divide,
    distribute,
    AccountDistribution (..),
    fraction,
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
import Money.Amount (Amount (..))
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

-- | Turn an amount into a number of minimal quantisations.
--
-- === API Note
--
-- We return 'Integer' because the result does not fit into a 'Word64'
toMinimalQuantisations :: Account -> Integer
toMinimalQuantisations account =
  let f = case account of
        Positive _ -> id
        Negative _ -> negate
   in f $ (fromIntegral :: Word64 -> Integer) $ Amount.toMinimalQuantisations (abs account)

-- | Turn an amount of money into a 'Double'.
--
-- WARNING: the result will be infinite or NaN if the quantisation factor is @0@
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
fromDouble :: Word32 -> Double -> Maybe Account
fromDouble quantisationFactor d =
  let d' = Prelude.abs d
      f = if d >= 0 then Positive else Negative
   in f <$> Amount.fromDouble quantisationFactor d'

-- | Turn an amount of money into a 'Rational'.
--
-- WARNING: that the result will be @Account :% 0@ if the quantisation factor is @0@.
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
zero :: Account
zero = Positive Amount.zero

-- | Add two accounts of money.
--
-- This operation may fail when overflow over either bound occurs.
--
-- WARNING: This function can be used to accidentally add up two accounts of different currencies.
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
sum :: forall f. Foldable f => f Account -> Maybe Account
sum = foldM add zero

-- | Add two accounts of money.
--
-- This operation may fail when overflow over either bound occurs.
--
-- WARNING: This function can be used to accidentally subtract two accounts of different currencies.
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
abs :: Account -> Amount
abs = \case
  Negative a -> a
  Positive a -> a

-- | Multiply an account by an integer scalar
--
-- This operation will fail when overflow over either bound occurs.
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

-- | Divide an account by an integer denominator
--
-- This operation will fail when dividing by zero.
--
-- WARNING: This function uses integer division, which means that money can
-- "dissappear" if the function is used incorrectly.
-- For example, when dividing 10 by 4, which results in 2, we cannot then multiply 4 by 2 again to get 10.
--
-- See also 'distribute'.
divide :: Account -> Int32 -> Maybe Account
divide account d =
  let ad = (fromIntegral :: Int32 -> Word32) ((Prelude.abs :: Int32 -> Int32) d)
      f = case (compare account zero, compare d 0) of
        (EQ, _) -> const (Just zero)
        (_, EQ) -> const Nothing
        (GT, GT) -> Just . Positive
        (GT, LT) -> Just . Negative
        (LT, GT) -> Just . Negative
        (LT, LT) -> Just . Positive
   in Amount.divide (abs account) ad >>= f

-- | Distribute an amount of money into chunks that are as evenly distributed as possible.
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

-- | Fractional multiplication
fraction ::
  Account ->
  Rational ->
  (Account, Rational)
fraction account f =
  let af = (realToFrac :: Rational -> Ratio Natural) ((Prelude.abs :: Rational -> Rational) f)
      aa = abs account
      (amount, actualFraction) = Amount.fraction aa af
      func :: Amount -> Rational -> (Account, Rational)
      func a r = case (compare account zero, compare f 0) of
        (EQ, _) -> (zero, r)
        (_, EQ) -> (zero, 0)
        (GT, GT) -> (Positive a, r)
        (GT, LT) -> (Negative a, -r)
        (LT, GT) -> (Negative a, r)
        (LT, LT) -> (Positive a, -r)
   in func amount ((realToFrac :: Ratio Natural -> Rational) actualFraction)
