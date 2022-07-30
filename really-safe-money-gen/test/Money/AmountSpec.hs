{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AmountSpec (spec) where

import Data.Proxy
import Data.Typeable
import Money.Amount (Amount (..))
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Money.Currency (Currency (..))
import qualified Money.Currency as Currency
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  describe "fromMinimalQuantisations" $
    it "produces valid amounts" $
      producesValid Amount.fromMinimalQuantisations

  describe "toMinimalQuantisations" $ do
    it "produces valid Int64s" $
      producesValid Amount.toMinimalQuantisations

    it "roundtrips with fromMinimalQuantisations" $
      forAllValid $ \amount ->
        Amount.fromMinimalQuantisations (Amount.toMinimalQuantisations amount) `shouldBe` amount

  describe "fromDouble" $
    forallCurrencies $ \p@(Proxy :: Proxy currency) -> do
      let from = Amount.fromDouble :: Double -> Maybe (Amount currency)

      it "produces valid amounts" $
        producesValid from

      it "succeeds on 0" $
        from 0.0 `shouldBe` Just (Amount 0)

      it "succeeds on 1" $
        from 1 `shouldBe` Just (Amount (fromIntegral (quantisationFactor p)))

      it "succeeds on -1" $
        from (-1) `shouldBe` Just (Amount (-(fromIntegral (quantisationFactor p))))

      it "fails on NaN" $
        let nan = read "NaN"
         in from nan `shouldBe` Nothing

      it "fails on +Infinity" $
        let pinf = read "Infinity"
         in from pinf `shouldBe` Nothing

      it "fails on -Infinity" $
        let minf = read "-Infinity"
         in from minf `shouldBe` Nothing

      it "roundtrips with toDouble" $
        forAllValid $ \amount ->
          from (Amount.toDouble amount) `shouldBe` Just (amount :: Amount currency)

  describe "toDouble" $
    forallCurrencies $ \(Proxy :: Proxy currency) -> do
      let to = Amount.toDouble :: Amount currency -> Double
      it "produces valid Doubles" $
        producesValid to

  describe "fromRational" $
    forallCurrencies $ \p@(Proxy :: Proxy currency) -> do
      let from = Amount.fromRational :: Rational -> Maybe (Amount currency)

      it "produces valid Amounts" $
        producesValid from

      it "succeeds on 0" $
        from 0.0 `shouldBe` Just (Amount 0)

      it "succeeds on 1" $
        from 1 `shouldBe` Just (Amount (fromIntegral (quantisationFactor p)))

      it "succeeds on -1" $
        from (-1) `shouldBe` Just (Amount (-(fromIntegral (quantisationFactor p))))

      it "fails on NaN" $
        let nan = read "NaN"
         in from nan `shouldBe` Nothing

      it "fails on +Infinity" $
        let pinf = read "Infinity"
         in from pinf `shouldBe` Nothing

      it "fails on -Infinity" $
        let minf = read "-Infinity"
         in from minf `shouldBe` Nothing

      it "roundtrips with toRational" $
        forAllValid $ \amount ->
          from (Amount.toRational amount) `shouldBe` Just (amount :: Amount currency)
  describe "toRational" $
    forallCurrencies $ \(Proxy :: Proxy currency) -> do
      let to = Amount.toRational :: Amount currency -> Rational
      it "produces valid Rationals" $
        producesValid to

forallCurrencies :: (forall currency. Currency currency => Proxy currency -> Spec) -> Spec
forallCurrencies func = do
  let d :: forall currency. (Typeable currency, Currency currency) => Proxy currency -> Spec
      d p = describe (nameOf @currency) $ func p
  d (Proxy @Currency.USD)
  d (Proxy @Currency.CHF)
  d (Proxy @Currency.BTC)
  d (Proxy @Currency.ADA)
