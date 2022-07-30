{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AmountSpec (spec) where

import Data.Proxy
import Data.Typeable
import GHC.Real
import Money.Amount (Amount (..))
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Money.Currency (Currency (..))
import qualified Money.Currency as Currency
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = forallCurrencies $ \p@(Proxy :: Proxy currency) -> do
  describe "fromMinimalQuantisations" $
    it "produces valid amounts" $
      producesValid Amount.fromMinimalQuantisations

  describe "toMinimalQuantisations" $ do
    it "produces valid Int64s" $
      producesValid Amount.toMinimalQuantisations

    it "roundtrips with fromMinimalQuantisations" $
      forAllValid $ \amount ->
        Amount.fromMinimalQuantisations (Amount.toMinimalQuantisations amount) `shouldBe` amount

  describe "fromDouble" $ do
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
        from (Amount.toDouble amount) `shouldBe` Just (amount :: Amount currency)

  describe "toDouble" $ do
    let to = Amount.toDouble :: Amount currency -> Double
    it "produces valid Doubles" $
      producesValid to

  describe "fromRational" $ do
    let from = Amount.fromRational :: Rational -> Maybe (Amount currency)

    it "produces valid Amounts" $
      producesValid from

    it "succeeds on 0" $
      from 0.0 `shouldBe` Just (Amount 0)

    it "succeeds on 1" $
      from 1 `shouldBe` Just (Amount (fromIntegral (quantisationFactor p)))

    it "succeeds on -1" $
      from (-1) `shouldBe` Just (Amount (-(fromIntegral (quantisationFactor p))))

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
        from (Amount.toRational amount) `shouldBe` Just (amount :: Amount currency)
  describe "toRational" $ do
    let to = Amount.toRational :: Amount currency -> Rational
    it "produces valid Rationals" $
      producesValid to

  let zero = Amount.zero @currency
  describe "zero" $
    it "is valid" $
      shouldBeValid zero

  describe "add" $ do
    let add = Amount.add @currency
    it "produces valid amounts" $
      producesValid2 add

    it "has a left-identity: zero" $
      forAllValid $ \a ->
        add Amount.zero a `shouldBe` Right a

    it "has a right-identity: zero" $
      forAllValid $ \a ->
        add a Amount.zero `shouldBe` Right a

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
    let multiply = Amount.multiply @currency

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
