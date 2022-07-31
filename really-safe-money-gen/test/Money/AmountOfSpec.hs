{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AmountOfSpec (spec) where

import Data.Proxy
import Data.Typeable
import GHC.Real
import Money.Amount (Amount (..))
import Money.AmountOf (AmountOf (..))
import qualified Money.AmountOf as AmountOf
import Money.AmountOf.Gen ()
import Money.Currency (Currency (..))
import qualified Money.Currency as Currency
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = forallCurrencies $ \p@(Proxy :: Proxy currency) -> do
  describe "fromMinimalQuantisations" $
    it "produces valid amounts" $
      producesValid AmountOf.fromMinimalQuantisations

  describe "toMinimalQuantisations" $ do
    it "produces valid Int64s" $
      producesValid AmountOf.toMinimalQuantisations

    it "roundtrips with fromMinimalQuantisations" $
      forAllValid $ \amount ->
        AmountOf.fromMinimalQuantisations (AmountOf.toMinimalQuantisations amount) `shouldBe` amount

  describe "fromDouble" $ do
    let from = AmountOf.fromDouble :: Double -> Maybe (AmountOf currency)

    it "produces valid amounts" $
      producesValid from

    it "succeeds on 0" $
      from 0.0 `shouldBe` Just (AmountOf (Amount 0))

    it "succeeds on 1" $
      from 1 `shouldBe` Just (AmountOf (Amount (fromIntegral (quantisationFactor p))))

    it "succeeds on -1" $
      from (-1) `shouldBe` Just (AmountOf (Amount (-(fromIntegral (quantisationFactor p)))))

    it "fails on NaN" $
      let nan = read "NaN" :: Double
       in from nan `shouldBe` Nothing

    it "fails on +Infinity" $
      let pinf = read "Infinity"
       in from pinf `shouldBe` Nothing

    it "fails on -Infinity" $
      let minf = read "-Infinity"
       in from minf `shouldBe` Nothing

    it "roundtrips with toDouble" $
      forAllValid $ \amount ->
        from (AmountOf.toDouble amount) `shouldBe` Just (amount :: AmountOf currency)

  describe "toDouble" $ do
    let to = AmountOf.toDouble :: AmountOf currency -> Double
    it "produces valid Doubles" $
      producesValid to

  describe "fromRational" $ do
    let from = AmountOf.fromRational :: Rational -> Maybe (AmountOf currency)

    it "produces valid Amounts" $
      producesValid from

    it "succeeds on 0" $
      from 0.0 `shouldBe` Just (AmountOf (Amount 0))

    it "succeeds on 1" $
      from 1 `shouldBe` Just (AmountOf (Amount (fromIntegral (quantisationFactor p))))

    it "succeeds on -1" $
      from (-1) `shouldBe` Just (AmountOf (Amount (-(fromIntegral (quantisationFactor p)))))

    xit "fails on NaN" $ do
      let !nan = 0 :% 0 :: Rational
      from nan `shouldBe` Nothing

    xit "fails on +Infinity" $ do
      let !pinf = 1 :% 0 :: Rational
      from pinf `shouldBe` Nothing

    xit "fails on -Infinity" $ do
      let !minf = -1 :% 0 :: Rational
      from minf `shouldBe` Nothing

    it "roundtrips with toRational" $
      forAllValid $ \amount ->
        from (AmountOf.toRational amount) `shouldBe` Just (amount :: AmountOf currency)

  describe "toRational" $ do
    let to = AmountOf.toRational :: AmountOf currency -> Rational
    it "produces valid Rationals" $
      producesValid to

  let zero = AmountOf.zero @currency
  describe "zero" $
    it "is valid" $
      shouldBeValid zero

  describe "add" $ do
    let add = AmountOf.add @currency
    it "produces valid amounts" $
      producesValid2 add

    it "has a left-identity: zero" $
      forAllValid $ \a ->
        add AmountOf.zero a `shouldBe` Right a

    it "has a right-identity: zero" $
      forAllValid $ \a ->
        add a AmountOf.zero `shouldBe` Right a

    it "is associative" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          forAllValid $ \a3 -> do
            let l = add <$> add a1 a2 <*> pure a3
            let r = add <$> pure a1 <*> add a2 a3
            l `shouldBe` r

    it "is commutative" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          add a1 a2 `shouldBe` add a2 a1

  describe "multiply" $ do
    let multiply = AmountOf.multiply @currency

    it "produces valid amounts" $
      producesValid2 multiply

    it "has an identity: 1" $
      forAllValid $ \a ->
        multiply 1 a `shouldBe` Right a

    it "is absorbed by 0" $
      forAllValid $ \a ->
        multiply 0 a `shouldBe` Right zero

forallCurrencies :: (forall currency. Currency currency => Proxy currency -> Spec) -> Spec
forallCurrencies func = do
  let d :: forall currency. (Typeable currency, Currency currency) => Proxy currency -> Spec
      d p = describe (nameOf @currency) $ func p
  d (Proxy @Currency.USD)
  d (Proxy @Currency.CHF)
  d (Proxy @Currency.BTC)
  d (Proxy @Currency.ADA)
