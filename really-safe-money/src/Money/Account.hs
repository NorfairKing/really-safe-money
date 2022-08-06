{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

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
    subtract,
    abs,
    multiply,
  )
where

import Control.DeepSeq
import Data.Function
import Data.Int
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Money.Amount (Amount (..))
import qualified Money.Amount as Amount
import Prelude hiding (abs, fromRational, subtract, toRational)
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
-- This operation may fail when overflow over either bound occurs.
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
