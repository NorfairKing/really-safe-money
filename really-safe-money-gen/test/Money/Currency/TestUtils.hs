{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Money.Currency.TestUtils where

import Data.Typeable
import Money.Amount.Gen ()
import Money.Currency as Currency
import Test.Syd
import Test.Syd.Validity.Utils

forallCurrencies ::
  ( forall currency.
    (Typeable currency, Currency currency) =>
    Proxy currency ->
    Spec
  ) ->
  Spec
forallCurrencies func = do
  let d :: forall currency. (Typeable currency, Currency currency) => Proxy currency -> Spec
      d p = describe (nameOf @currency) $ func p
  d (Proxy @Currency.USD)
  d (Proxy @Currency.CHF)
  d (Proxy @Currency.BTC)
  d (Proxy @Currency.ADA)
