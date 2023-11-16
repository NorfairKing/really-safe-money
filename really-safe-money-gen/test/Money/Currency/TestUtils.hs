{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Money.Currency.TestUtils where

import Data.Typeable
import Money.Amount.Gen ()
import Money.Currency as Currency
import Money.QuantisationFactor
import Test.Syd
import Test.Syd.Validity.Utils

forallCurrencies ::
  ( forall currency.
    (Typeable currency, IsCurrencyType currency) =>
    Proxy currency ->
    Spec
  ) ->
  Spec
forallCurrencies func = do
  let d :: forall currency. (Typeable currency, IsCurrencyType currency) => Proxy currency -> Spec
      d p = describe (nameOf @currency) $ func p
  d (Proxy @USD)
  d (Proxy @CHF)
  d (Proxy @INR)
  d (Proxy @BTC)
  d (Proxy @ADA)

data CHF
  deriving (Typeable)

instance IsCurrencyType CHF where
  quantisationFactor Proxy = QuantisationFactor 20

data USD
  deriving (Typeable)

instance IsCurrencyType USD where
  quantisationFactor Proxy = QuantisationFactor 100

data INR
  deriving (Typeable)

instance IsCurrencyType INR where
  quantisationFactor Proxy = QuantisationFactor 1

data BTC
  deriving (Typeable)

instance IsCurrencyType BTC where
  quantisationFactor Proxy = QuantisationFactor 100_000_000

data ADA
  deriving (Typeable)

instance IsCurrencyType ADA where
  quantisationFactor Proxy = QuantisationFactor 1_000_000
