{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.ConversionRateOfSpec (spec) where

import Data.Proxy
import qualified Money.ConversionRate as ConversionRate
import Money.ConversionRateOf
import qualified Money.ConversionRateOf as ConversionRateOf
import Money.ConversionRateOf.Gen ()
import Money.Currency.TestUtils
import Numeric.DecimalLiteral.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  forallCurrencies $ \(Proxy :: Proxy from) ->
    forallCurrencies $ \(Proxy :: Proxy to) -> do
      genValidSpec @(ConversionRateOf from to)

      describe "Ratio" $ do
        let to = ConversionRateOf.toRatio @from @to
        describe "toRatio" $
          it "produces valid ratios" $
            producesValid to

        let from = ConversionRateOf.fromRatio @from @to
        describe "fromRatio" $ do
          it "produces valid conversion rates" $
            producesValid from
          it "roundtrips with toRatio" $
            forAllValid $ \cr ->
              from (to cr) `shouldBe` Just cr

      describe "Rational" $ do
        let to = ConversionRateOf.toRational @from @to
        describe "toRational" $ do
          it "produces valid rationals" $
            producesValid to

        let from = ConversionRateOf.fromRational @from @to
        describe "fromRational" $ do
          it "produces valid conversion rates" $
            producesValid from

          it "roundtrips with toRational" $
            forAllValid $ \cr ->
              from (to cr) `shouldBe` Just cr

      describe "DecimalLiteral" $ do
        let to = ConversionRateOf.toDecimalLiteral @from @to
        describe "toDecimalLiteral" $ do
          it "produces valid literals" $
            producesValid to

          it "delegates to ConversionRate.toDecimalLiteral" $
            forAllValid $ \cr ->
              to cr `shouldBe` ConversionRate.toDecimalLiteral (unConversionRateOf cr)

        let from = ConversionRateOf.fromDecimalLiteral @from @to
        describe "fromDecimalLiteral" $ do
          it "produces valid conversion rates" $
            producesValid from
          it "roundtrips with toDecimalLiteral" $
            forAllValid $ \cr ->
              case to cr of
                Nothing -> pure () -- Fine
                Just dl -> from dl `shouldBe` Just cr

      describe "invert" $ do
        it "produces valid rates" $
          producesValid (ConversionRateOf.invert @from @to)

        it "inverts the rate" $
          forAllValid $ \cr ->
            toRatio (ConversionRateOf.invert @from @to cr) `shouldBe` recip (toRatio cr)

      describe "compose" $ do
        it "produces valid rates" $
          producesValid2 ConversionRateOf.compose

        it "multiplies the rates" $
          forAllValid $ \cr1 ->
            forAllValid $ \cr2 ->
              toRatio (ConversionRateOf.compose @from @to @to cr1 cr2)
                `shouldBe` toRatio cr1 * toRatio cr2
