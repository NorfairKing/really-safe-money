{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports -Wno-dodgy-exports -Wno-redundant-constraints #-}

module Money.AmountOf
  ( AmountOf (..),
    zero,
    fromAmount,
    toAmount,
    toMinimalQuantisations,
    fromMinimalQuantisations,
    fromDouble,
    toDouble,
    fromRational,
    toRational,
    Currency (..),
    add,
    sum,
    subtract,
    multiply,
    divide,
    AmountDistributionOf (..),
    distribute,
    fraction,
  )
where

import Control.DeepSeq
import Data.Foldable as Foldable hiding (sum)
import Data.Proxy
import Data.Ratio
import Data.Typeable
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Money.Amount (Amount)
import qualified Money.Amount as Amount
import Money.Currency as Currency
import Numeric.Natural
import Prelude hiding (fromRational, subtract, sum, toRational)

-- | An amount of money of a specific currency. May not be negative.
--
-- === Representation
--
-- The underlying representation is 'Amount'.
-- See its documentation for more details.
newtype AmountOf (currency :: k) = AmountOf
  { unAmountOf :: Amount
  }
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

instance Validity (AmountOf currency)

instance NFData (AmountOf currency)

instance
  Enum Amount =>
  Enum (AmountOf currency)
  where
  toEnum = undefined
  fromEnum = undefined

instance
  Bounded Amount =>
  Bounded (AmountOf currency)
  where
  minBound = undefined
  maxBound = undefined

instance
  Num Amount =>
  Num (AmountOf currency)
  where
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined
  (-) = undefined

zero :: AmountOf currency
zero = fromAmount Amount.zero

fromAmount :: Amount -> AmountOf currency
fromAmount = AmountOf

toAmount :: AmountOf currency -> Amount
toAmount = unAmountOf

toMinimalQuantisations :: AmountOf currency -> Word64
toMinimalQuantisations = Amount.toMinimalQuantisations . toAmount

fromMinimalQuantisations :: Word64 -> AmountOf currency
fromMinimalQuantisations = fromAmount . Amount.fromMinimalQuantisations

fromDouble :: forall currency. Currency currency => Double -> Maybe (AmountOf currency)
fromDouble = fmap fromAmount . Amount.fromDouble (quantisationFactor (Proxy @currency))

toDouble :: forall currency. Currency currency => AmountOf currency -> Double
toDouble = Amount.toDouble (quantisationFactor (Proxy @currency)) . toAmount

fromRational :: forall currency. Currency currency => Rational -> Maybe (AmountOf currency)
fromRational = fmap fromAmount . Amount.fromRational (quantisationFactor (Proxy @currency))

toRational :: forall currency. Currency currency => AmountOf currency -> Rational
toRational = Amount.toRational (quantisationFactor (Proxy @currency)) . toAmount

-- | Add two amounts of money.
--
-- This operation may fail with an 'AdditionFailure' for the following reasons:
--
-- TODO
add ::
  AmountOf currency ->
  AmountOf currency ->
  Maybe (AmountOf currency)
add (AmountOf a1) (AmountOf a2) = fromAmount <$> Amount.add a1 a2

sum ::
  forall f currency.
  Foldable f =>
  f (AmountOf currency) ->
  Maybe (AmountOf currency)
sum as = fromAmount <$> Amount.sum (map unAmountOf (Foldable.toList as))

subtract ::
  AmountOf currency ->
  AmountOf currency ->
  Maybe (AmountOf currency)
subtract (AmountOf a1) (AmountOf a2) = fromAmount <$> Amount.subtract a1 a2

-- API Note: The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
multiply ::
  Word32 ->
  AmountOf currency ->
  Maybe (AmountOf currency)
multiply f (AmountOf a) = fromAmount <$> Amount.multiply f a

-- API Note: The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
divide ::
  AmountOf currency ->
  Word32 ->
  Maybe (AmountOf currency)
divide (AmountOf a) i = fromAmount <$> Amount.divide a i

-- | Distribute an amount of money into chunks that are as evenly distributed as possible.
distribute :: AmountOf currency -> Word32 -> AmountDistributionOf currency
distribute (AmountOf a) w = case Amount.distribute a w of
  Amount.DistributedIntoZeroChunks -> DistributedIntoZeroChunks
  Amount.DistributedZeroAmount -> DistributedZeroAmount
  Amount.DistributedIntoEqualChunks w' a' -> DistributedIntoEqualChunks w' (fromAmount a')
  Amount.DistributedIntoUnequalChunks w1 a1 w2 a2 -> DistributedIntoUnequalChunks w1 (fromAmount a1) w2 (fromAmount a2)

-- | The result of 'distribute'
data AmountDistributionOf currency
  = -- | The second argument was zero.
    DistributedIntoZeroChunks
  | -- | The first argument was a zero amount.
    DistributedZeroAmount
  | -- | Distributed into this many equal chunks of this amount
    DistributedIntoEqualChunks !Word32 !(AmountOf currency)
  | -- | Distributed into unequal chunks, this many of the first (larger) amount, and this many of the second (slightly smaller) amount.
    DistributedIntoUnequalChunks !Word32 !(AmountOf currency) !Word32 !(AmountOf currency)
  deriving (Show, Read, Eq, Generic)

instance Validity (AmountDistributionOf currency) where
  validate ad =
    mconcat
      [ genericValidate ad,
        case ad of
          DistributedIntoUnequalChunks _ a1 _ a2 ->
            declare "The larger chunks are larger" $
              a1 > a2
          _ -> valid
      ]

instance NFData (AmountDistributionOf currency)

fraction ::
  AmountOf currency ->
  Ratio Natural ->
  (AmountOf currency, Ratio Natural)
fraction (AmountOf a) f =
  let (a', r) = Amount.fraction a f
   in (fromAmount a', r)
