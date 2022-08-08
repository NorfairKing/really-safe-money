{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Money.AccountOf
  ( AccountOf (..),
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
    AccountDistributionOf (..),
    fraction,
  )
where

import Control.DeepSeq
import Data.Foldable hiding (sum)
import Data.Function
import Data.Int
import Data.Monoid
import Data.Ratio
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Money.Account (Account (..))
import Money.Amount (Amount (..))
import qualified Money.Amount as Amount
import Money.AmountOf (AmountOf (..))
import Numeric.Natural
import Prelude hiding (abs, fromRational, subtract, sum, toRational)
import qualified Prelude

newtype AccountOf (currency :: k) = AccountOf {unAccountOf :: Account}
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity (AccountOf currency)

instance NFData (AccountOf currency)

fromMinimalQuantisations :: Integer -> Maybe (AccountOf currency)
fromMinimalQuantisations = undefined

toMinimalQuantisations :: AccountOf currency -> Integer
toMinimalQuantisations = undefined

toDouble :: Word32 -> AccountOf currency -> Double
toDouble = undefined

fromDouble :: Word32 -> Double -> Maybe (AccountOf currency)
fromDouble = undefined

toRational :: Word32 -> AccountOf currency -> Rational
toRational = undefined

fromRational :: Word32 -> Rational -> Maybe (AccountOf currency)
fromRational = undefined

zero :: AccountOf currency
zero = undefined

add :: AccountOf currency -> AccountOf currency -> Maybe (AccountOf currency)
add = undefined

sum :: forall f currency. Foldable f => f (AccountOf currency) -> Maybe (AccountOf currency)
sum = undefined

subtract :: AccountOf currency -> AccountOf currency -> Maybe (AccountOf currency)
subtract = undefined

abs :: AccountOf currency -> AmountOf currency
abs = undefined

multiply :: Int32 -> AccountOf currency -> Maybe (AccountOf currency)
multiply = undefined

divide :: AccountOf currency -> Int32 -> Maybe (AccountOf currency)
divide = undefined

distribute :: AccountOf currency -> Word16 -> AccountDistributionOf currency
distribute = undefined

-- | The result of 'distribute'
data AccountDistributionOf (currency :: k)
  = -- | The second argument was zero.
    DistributedIntoZeroChunks
  | -- | The first argument was a zero amount.
    DistributedZeroAccount
  | -- | Distributed into this many equal chunks of this amount
    DistributedIntoEqualChunks !Word32 !(AccountOf currency)
  | -- | Distributed into unequal chunks, this many of the first (larger, in absolute value) amount, and this many of the second (slightly smaller) amount.
    DistributedIntoUnequalChunks !Word32 !(AccountOf currency) !Word32 !(AccountOf currency)
  deriving (Show, Read, Eq, Generic)

instance Validity (AccountDistributionOf currency) where
  validate ad =
    mconcat
      [ genericValidate ad,
        case ad of
          DistributedIntoUnequalChunks _ a1 _ a2 ->
            declare "The larger chunks are larger in absolute value" $
              abs a1 > abs a2
          _ -> valid
      ]

instance NFData (AccountDistributionOf currency)

-- | Fractional multiplication
fraction ::
  AccountOf currency ->
  Rational ->
  (AccountOf currency, Rational)
fraction = undefined
