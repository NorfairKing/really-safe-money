{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.MultiAmountSpec (spec) where

import Data.GenValidity.Vector ()
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import Money.Amount (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import Money.ConversionRate (ConversionRate (..))
import qualified Money.ConversionRate as ConversionRate
import Money.ConversionRate.Gen ()
import Money.Currency
import Money.MultiAmount (MultiAmount (..), Rounded (..))
import qualified Money.MultiAmount as MultiAmount
import Money.MultiAmount.Gen ()
import Money.QuantisationFactor (QuantisationFactor (..))
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  showReadSpec @(MultiAmount Currency)
  modifyMaxSuccess (* 10) $ do
    genValidSpec @(MultiAmount Currency)
    eqSpec @(MultiAmount Currency)
    ordSpec @(MultiAmount Currency)

    describe "fromAmount" $ do
      it "produces valid amounts" $ do
        producesValid2 (MultiAmount.fromAmount @Currency)

    describe "zero" $ do
      it "is valid" $
        shouldBeValid (MultiAmount.zero @Currency)

    describe "add" $ do
      it "produces valid amounts" $
        producesValid2 (MultiAmount.add @Currency)

      it "has a left-identity: zero" $
        forAllValid $ \a ->
          MultiAmount.add @Currency MultiAmount.zero a `shouldBe` Just a

      it "has a right-identity: zero" $
        forAllValid $ \a ->
          MultiAmount.add @Currency a MultiAmount.zero `shouldBe` Just a

      it "is associative when both succeed" $
        forAllValid $ \a1 ->
          forAllValid $ \a2 ->
            forAllValid $ \a3 -> do
              let errOrL = MultiAmount.add @Currency <$> MultiAmount.add a1 a2 <*> pure a3
              let errOrR = MultiAmount.add @Currency <$> pure a1 <*> MultiAmount.add a2 a3
              case (,) <$> errOrL <*> errOrR of
                Nothing -> pure () -- Fine.
                Just (l, r) -> l `shouldBe` r

      it "is commutative" $
        forAllValid $ \a1 ->
          forAllValid $ \a2 ->
            MultiAmount.add @Currency a1 a2 `shouldBe` MultiAmount.add @Currency a2 a1

    describe "subtract" $ do
      it "produces valid amounts" $
        producesValid2 (MultiAmount.subtract @Currency)

      it "has a right-identity: zero" $
        forAllValid $ \a ->
          MultiAmount.subtract @Currency a MultiAmount.zero `shouldBe` Just a

    describe "sum" $ do
      it "produces valid amounts" $
        producesValid (MultiAmount.sum @Vector @Currency)

    describe "addAmount" $ do
      it "produces valid amounts" $
        producesValid3 (MultiAmount.addAmount @Currency)

    describe "subtractAmount" $ do
      it "produces valid amounts" $
        producesValid3 (MultiAmount.subtractAmount @Currency)

    describe "convertAll" $ do
      it "produces the right result in this example" $ do
        forAllValid $ \rounding -> do
          let qfEur = QuantisationFactor 100
              eur = Currency "EUR" qfEur
          let qfChf = QuantisationFactor 20
              chf = Currency "CHF" qfChf
          let qfUsd = QuantisationFactor 100
              usd = Currency "USD" qfUsd
          let aEur = Amount 100
          let aChf = Amount 20
          let aUsd = Amount 100
          let ma =
                MultiAmount $
                  M.fromList
                    [ (eur, aEur),
                      (usd, aUsd),
                      (chf, aChf)
                    ]
              func c = case currencySymbol c of
                "CHF" -> pure (ConversionRate 1, qfChf)
                "EUR" -> pure (ConversionRate 1.5, qfEur)
                "USD" -> pure (ConversionRate 2.0, qfUsd)
                _ -> expectationFailure "Should not happen."
          (mResult, _) <- MultiAmount.convertAllA rounding qfChf func ma
          mResult `shouldBe` Just (Amount 90)

      it "produces valid results when converting two currencies to one" $
        forAllValid $ \c1 ->
          forAllValid $ \c2 ->
            forAllValid $ \a1 ->
              forAllValid $ \a2 ->
                forAllValid $ \cr ->
                  forAllValid $ \rounding -> do
                    let (mResult, rounded) =
                          MultiAmount.convertAll
                            rounding
                            (currencyQuantisationFactor c1)
                            ( \c ->
                                ( if c == c1 then ConversionRate.oneToOne else cr,
                                  currencyQuantisationFactor c
                                )
                            )
                            (MultiAmount (M.fromList [(c1, a1), (c2, a2)]))
                    shouldBeValid mResult
                    rounded
                      `shouldSatisfy` ( \r -> case rounding of
                                          RoundDown -> r == RoundedDown || r == DidNotRound
                                          RoundNearest -> True
                                          RoundUp -> r == RoundedUp || r == DidNotRound
                                      )
      it "does the same as 'convert' when there is only one amount" $
        forAllValid $ \c1 ->
          forAllValid $ \a ->
            forAllValid $ \c2 ->
              forAllValid $ \cr ->
                forAllValid $ \rounding ->
                  let qf1 = currencyQuantisationFactor c1
                      qf2 = currencyQuantisationFactor c2
                      ma = MultiAmount.fromAmount c1 a
                      (mMA, _) = MultiAmount.convertAll rounding qf1 (\_ -> (cr, qf2)) ma
                      (mA, _) = Amount.convert rounding qf2 a cr qf1
                   in mMA `shouldBe` mA
