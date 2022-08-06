{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Money.Account
  ( Account (..),
    fromMinimalQuantisations,
    toMinimalQuantisations,
    fromRational,
    toRational,
    abs,
  )
where

import Data.Function
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Money.Amount (Amount (..))
import qualified Money.Amount as Amount
import Prelude hiding (abs, fromRational, toRational)
import qualified Prelude

-- | An account of money. Like 'Amount' but can also be negative.
data Account
  = Positive !Amount
  | Negative !Amount
  deriving (Show, Read, Generic)

instance Validity Account

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
toMinimalQuantisations = \case
  Positive a -> (fromIntegral :: Word64 -> Integer) $ Amount.toMinimalQuantisations a
  Negative a -> negate $ (fromIntegral :: Word64 -> Integer) $ Amount.toMinimalQuantisations a

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

-- | The absolute value of the account
--
-- Note that this returns an 'Amount' and not an 'Account' because the result is always positive.
abs :: Account -> Amount
abs = \case
  Negative a -> a
  Positive a -> a
