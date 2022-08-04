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
import Prelude hiding (subtract)

spec :: Spec
spec = forallCurrencies $ \p@(Proxy :: Proxy currency) -> do
  eqSpec @(AmountOf currency)
  ordSpec @(AmountOf currency)
  showReadSpec @(AmountOf currency)

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

    it "fails on -1" $
      from (-1) `shouldBe` Nothing

    it "fails on NaN" $
      let nan = read "NaN" :: Double
       in from nan `shouldBe` Nothing

    it "fails on +Infinity" $
      let pinf = read "Infinity"
       in from pinf `shouldBe` Nothing

    it "fails on -Infinity" $
      let minf = read "-Infinity"
       in from minf `shouldBe` Nothing

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

    it "fails on -1" $
      from (-1) `shouldBe` Nothing

    it "fails on NaN" $
      let nan = 0 :% 0 :: Rational
       in from nan `shouldBe` Nothing

    it "fails on +Infinity" $
      let pinf = 1 :% 0 :: Rational
       in from pinf `shouldBe` Nothing

    it "fails on -Infinity" $
      let minf = -1 :% 0 :: Rational
       in from minf `shouldBe` Nothing

    it "roundtrips with toRational" $
      forAllValid $ \amount ->
        from (AmountOf.toRational amount)
          `shouldBe` Just (amount :: AmountOf currency)

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
        add AmountOf.zero a `shouldBe` Just a

    it "has a right-identity: zero" $
      forAllValid $ \a ->
        add a AmountOf.zero `shouldBe` Just a

    it "is associative when both succeed" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          forAllValid $ \a3 -> do
            let errOrL = add <$> add a1 a2 <*> pure a3
            let errOrR = add <$> pure a1 <*> add a2 a3
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine
              Just (l, r) -> l `shouldBe` r

    it "is commutative" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          add a1 a2 `shouldBe` add a2 a1

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAmount = add a1 a2
          case errOrAmount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger (AmountOf.toMinimalQuantisations a1)
                      + toInteger (AmountOf.toMinimalQuantisations a2)
              toInteger (AmountOf.toMinimalQuantisations amountResult) `shouldBe` integerResult

  describe "subtract" $ do
    let subtract = AmountOf.subtract @currency
    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAmount = subtract a1 a2
          case errOrAmount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger (AmountOf.toMinimalQuantisations a1)
                      - toInteger (AmountOf.toMinimalQuantisations a2)
              toInteger (AmountOf.toMinimalQuantisations amountResult) `shouldBe` integerResult

  describe "multiply" $ do
    let multiply = AmountOf.multiply @currency

    it "produces valid amounts" $
      producesValid2 multiply

    it "has an identity: 1" $
      forAllValid $ \a ->
        multiply 1 a `shouldBe` Just a

    it "is absorbed by 0" $
      forAllValid $ \a ->
        multiply 0 a `shouldBe` Just zero

    -- A x (B + C) == A x B + A x C
    it "is distributive with add when both succeed" $
      forAllValid $ \a ->
        forAllValid $ \b ->
          forAllValid $ \c -> do
            let errOrL :: Maybe (AmountOf currency)
                errOrL = do
                  d <- AmountOf.add b c
                  multiply a d
            let errOrR :: Maybe (AmountOf currency)
                errOrR = do
                  d <- multiply a b
                  e <- multiply a c
                  AmountOf.add d e
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine
              Just (l, r) -> l `shouldBe` r

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \f ->
        forAllValid $ \a -> do
          let errOrAmountOf = multiply f a
          case errOrAmountOf of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger f
                      * toInteger (AmountOf.toMinimalQuantisations a)
              toInteger (AmountOf.toMinimalQuantisations amountResult) `shouldBe` integerResult

  describe "divide" $ do
    let divide = AmountOf.divide @currency
    it "produces valid amounts" $
      producesValid2 divide

    it "fails with a zero divisor" $
      forAllValid $ \a ->
        divide a 0 `shouldBe` Nothing

    it "succeeds when dividing by 1" $
      forAllValid $ \a ->
        divide a 1 `shouldBe` Just a

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a ->
        forAllValid $ \d -> do
          let errOrAmount = divide a d
          case errOrAmount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger (AmountOf.toMinimalQuantisations a)
                      `div` toInteger d
              toInteger (AmountOf.toMinimalQuantisations amountResult)
                `shouldBe` integerResult

  describe "fraction" $ do
    let fraction = AmountOf.fraction @currency
    it "produces valid amounts" $
      producesValid2 fraction

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
