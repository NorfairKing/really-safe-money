{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AccountOf
  ( AccountOf (..),
    IsCurrencyType (..),
    zero,
    fromAccount,
    toAccount,
    fromMinimalQuantisations,
    toMinimalQuantisations,
    fromDouble,
    toDouble,
    fromRational,
    toRational,
    add,
    sum,
    subtract,
    abs,
    multiply,
    distribute,
    AccountDistributionOf (..),
    fraction,
    Rounding (..),
  )
where

import Control.DeepSeq
import Data.Foldable as Foldable hiding (sum)
import Data.Function
import Data.Int
import Data.Monoid
import Data.Proxy
import Data.Ratio
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Money.Account (Account (..), Rounding (..))
import qualified Money.Account as Account
import Money.AmountOf (AmountOf (..))
import qualified Money.AmountOf as AmountOf
import Money.Currency
import Prelude hiding (abs, fromRational, subtract, sum, toRational)

newtype AccountOf (currency :: k) = AccountOf {unAccountOf :: Account}
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity (AccountOf currency)

instance NFData (AccountOf currency)

fromAccount :: Account -> AccountOf currency
fromAccount = AccountOf

toAccount :: AccountOf currency -> Account
toAccount = unAccountOf

fromMinimalQuantisations :: Integer -> Maybe (AccountOf currency)
fromMinimalQuantisations = fmap fromAccount . Account.fromMinimalQuantisations

toMinimalQuantisations :: AccountOf currency -> Integer
toMinimalQuantisations = Account.toMinimalQuantisations . toAccount

toDouble :: forall currency. IsCurrencyType currency => AccountOf currency -> Double
toDouble = Account.toDouble (quantisationFactor (Proxy @currency)) . toAccount

fromDouble :: forall currency. IsCurrencyType currency => Double -> Maybe (AccountOf currency)
fromDouble = fmap fromAccount . Account.fromDouble (quantisationFactor (Proxy @currency))

toRational :: forall currency. IsCurrencyType currency => AccountOf currency -> Rational
toRational = Account.toRational (quantisationFactor (Proxy @currency)) . toAccount

fromRational :: forall currency. IsCurrencyType currency => Rational -> Maybe (AccountOf currency)
fromRational = fmap fromAccount . Account.fromRational (quantisationFactor (Proxy @currency))

zero :: AccountOf currency
zero = fromAccount Account.zero

add :: AccountOf currency -> AccountOf currency -> Maybe (AccountOf currency)
add (AccountOf a1) (AccountOf a2) = fromAccount <$> Account.add a1 a2

sum :: forall f currency. Foldable f => f (AccountOf currency) -> Maybe (AccountOf currency)
sum as = fromAccount <$> Account.sum (map toAccount (Foldable.toList as))

subtract :: AccountOf currency -> AccountOf currency -> Maybe (AccountOf currency)
subtract (AccountOf a1) (AccountOf a2) = fromAccount <$> Account.subtract a1 a2

abs :: AccountOf currency -> AmountOf currency
abs = AmountOf.fromAmount . Account.abs . toAccount

multiply :: Int32 -> AccountOf currency -> Maybe (AccountOf currency)
multiply f (AccountOf a) = fromAccount <$> Account.multiply f a

distribute :: AccountOf currency -> Word16 -> AccountDistributionOf currency
distribute (AccountOf a) w = case Account.distribute a w of
  Account.DistributedIntoZeroChunks -> DistributedIntoZeroChunks
  Account.DistributedZeroAccount -> DistributedZeroAccount
  Account.DistributedIntoEqualChunks w' a' -> DistributedIntoEqualChunks w' (fromAccount a')
  Account.DistributedIntoUnequalChunks w1 a1 w2 a2 -> DistributedIntoUnequalChunks w1 (fromAccount a1) w2 (fromAccount a2)

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

-- | Fractional multiplication, see 'Account.fraction'
fraction ::
  Rounding ->
  AccountOf currency ->
  Rational ->
  (Maybe (AccountOf currency), Rational)
fraction rounding (AccountOf a) f =
  let (a', r) = Account.fraction rounding a f
   in (fromAccount <$> a', r)
