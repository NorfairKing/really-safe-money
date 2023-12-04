{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Account of a given type-level currency
--
-- === Importing this module
--
-- This module is designed to be imported as follows:
--
-- @
-- import Money.AccountOf (AccountOf)
-- import qualified Money.AccountOf as AccountOf
-- @
module Money.AccountOf
  ( AccountOf (..),
    IsCurrencyType (..),

    -- * Construction
    zero,
    fromMinimalQuantisations,
    fromAccount,
    fromAmountOf,
    fromAmount,
    fromDouble,
    fromRational,

    -- * Destruction
    toMinimalQuantisations,
    toAccount,
    toDouble,
    toRational,

    -- * Operations

    -- ** Addition
    add,
    sum,

    -- ** Subtraction
    subtract,

    -- ** Absolute value
    abs,

    -- ** Negation
    negate,

    -- ** Integral multiplication
    multiply,

    -- ** Integral distribution
    distribute,
    AccountDistributionOf,
    Distribution (..),

    -- ** Fractional multiplication
    Rounding (..),
    fraction,

    -- * Formatting
    format,
    quantisationFactorFormatString,
  )
where

import Control.DeepSeq
import Data.Foldable as Foldable hiding (sum)
import Data.Function
import Data.Int
import Data.Proxy
import Data.Ratio
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Money.Account (Account (..), Distribution (..), Rounding (..), quantisationFactorFormatString)
import qualified Money.Account as Account
import Money.Amount (Amount (..))
import Money.AmountOf (AmountOf (..))
import qualified Money.AmountOf as AmountOf
import Money.Currency
import Prelude hiding (abs, fromRational, negate, subtract, sum, toRational)

newtype AccountOf (currency :: k) = AccountOf {unAccountOf :: Account}
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity (AccountOf currency)

instance NFData (AccountOf currency)

-- | Annotate an account with a currency
fromAccount :: Account -> AccountOf currency
fromAccount = AccountOf

-- | Remove an account's currency annotation
--
-- WARNING: This removes the type-safety of the phantom type.
toAccount :: AccountOf currency -> Account
toAccount = unAccountOf

-- | Produce a positive account from a typed amount
fromAmountOf :: AmountOf currency -> AccountOf currency
fromAmountOf = fromAccount . Account.fromAmount . AmountOf.toAmount

-- | See 'Account.fromAmount'
fromAmount :: Amount -> AccountOf currency
fromAmount = fromAccount . Account.fromAmount

-- | See 'Account.fromMinimalQuantisations'
fromMinimalQuantisations :: Integer -> Maybe (AccountOf currency)
fromMinimalQuantisations = fmap fromAccount . Account.fromMinimalQuantisations

-- | See 'Account.toMinimalQuantisations'
toMinimalQuantisations :: AccountOf currency -> Integer
toMinimalQuantisations = Account.toMinimalQuantisations . toAccount

-- | See 'Account.toDouble'
toDouble :: forall currency. IsCurrencyType currency => AccountOf currency -> Double
toDouble = Account.toDouble (quantisationFactor (Proxy @currency)) . toAccount

-- | See 'Account.fromDouble'
fromDouble :: forall currency. IsCurrencyType currency => Double -> Maybe (AccountOf currency)
fromDouble = fmap fromAccount . Account.fromDouble (quantisationFactor (Proxy @currency))

-- | See 'Account.toRational'
toRational :: forall currency. IsCurrencyType currency => AccountOf currency -> Rational
toRational = Account.toRational (quantisationFactor (Proxy @currency)) . toAccount

-- | See 'Account.fromRational'
fromRational :: forall currency. IsCurrencyType currency => Rational -> Maybe (AccountOf currency)
fromRational = fmap fromAccount . Account.fromRational (quantisationFactor (Proxy @currency))

-- | See 'Account.zero'
zero :: AccountOf currency
zero = fromAccount Account.zero

-- | See 'Account.add'
add :: AccountOf currency -> AccountOf currency -> Maybe (AccountOf currency)
add (AccountOf a1) (AccountOf a2) = fromAccount <$> Account.add a1 a2

-- | See 'Account.sum'
sum :: forall f currency. Foldable f => f (AccountOf currency) -> Maybe (AccountOf currency)
sum as = fromAccount <$> Account.sum (map toAccount (Foldable.toList as))

-- | See 'Account.subtract'
subtract :: AccountOf currency -> AccountOf currency -> Maybe (AccountOf currency)
subtract (AccountOf a1) (AccountOf a2) = fromAccount <$> Account.subtract a1 a2

-- | See 'Account.abs'
abs :: AccountOf currency -> AmountOf currency
abs = AmountOf.fromAmount . Account.abs . toAccount

-- | See 'Account.negate'
negate :: AccountOf currency -> AccountOf currency
negate = AccountOf . Account.negate . toAccount

-- | See 'Account.multiply'
multiply :: Int32 -> AccountOf currency -> Maybe (AccountOf currency)
multiply f (AccountOf a) = fromAccount <$> Account.multiply f a

-- | See 'Account.distribute'
distribute :: AccountOf currency -> Word16 -> AccountDistributionOf currency
distribute (AccountOf a) w = case Account.distribute a w of
  DistributedIntoZeroChunks -> DistributedIntoZeroChunks
  DistributedZero -> DistributedZero
  DistributedIntoEqualChunks w' a' -> DistributedIntoEqualChunks w' (fromAccount a')
  DistributedIntoUnequalChunks w1 a1 w2 a2 -> DistributedIntoUnequalChunks w1 (fromAccount a1) w2 (fromAccount a2)

-- | The result of 'distribute'
type AccountDistributionOf (currency :: k) = Distribution (AccountOf currency)

-- | Fractional multiplication, see 'Account.fraction'
fraction ::
  Rounding ->
  AccountOf currency ->
  Rational ->
  (Maybe (AccountOf currency), Rational)
fraction rounding (AccountOf a) f =
  let (a', r) = Account.fraction rounding a f
   in (fromAccount <$> a', r)

-- | See 'formatAccount'
format :: forall currency. IsCurrencyType currency => AccountOf currency -> String
format ao = Account.format (quantisationFactor (Proxy @currency)) (toAccount ao)
