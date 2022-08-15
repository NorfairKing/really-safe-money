{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.CurrencySpec (spec) where

import Money.Currency (Currency (..))
import Money.Currency.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  showReadSpec @Currency
  modifyMaxSuccess (* 100) $ do
    eqSpec @Currency
    ordSpec @Currency
