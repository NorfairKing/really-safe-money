{-# LANGUAGE TypeApplications #-}

module Money.ConversionRateSpec (spec) where

import Data.GenValidity.Vector ()
import GHC.Real (Ratio (..))
import Money.Amount.Gen ()
import Money.ConversionRate
import qualified Money.ConversionRate as ConversionRate
import Money.ConversionRate.Gen ()
import Money.QuantisationFactor.Gen ()
import Numeric.DecimalLiteral.Gen ()
import Numeric.Natural (Natural)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (* 100) . modifyMaxSize (* 3) $ do
  genValidSpec @ConversionRate

  it "is invalid when the rate is zero" $
    shouldBeInvalid (ConversionRate 0)

  it "is invalid when the rate is infinite (denominator zero)" $
    shouldBeInvalid (ConversionRate ((1 :: Natural) :% 0))

  describe "Ratio" $ do
    describe "toRatio" $ do
      it "produces valid ratios" $
        producesValid ConversionRate.toRatio
    describe "fromRatio" $ do
      it "produces valid conversion rates" $
        producesValid ConversionRate.fromRatio
      it "roundtrips with toRatio" $
        forAllValid $ \cr ->
          ConversionRate.fromRatio (ConversionRate.toRatio cr) `shouldBe` Just cr

  describe "Rational" $ do
    describe "toRational" $ do
      it "produces valid rationals" $
        producesValid ConversionRate.toRational
    describe "fromRational" $ do
      it "produces valid conversion rates" $
        producesValid ConversionRate.fromRational
      it "roundtrips with toRational" $
        forAllValid $ \cr ->
          ConversionRate.fromRational (ConversionRate.toRational cr) `shouldBe` Just cr

  describe "DecimalLiteral" $ do
    describe "toDecimalLiteral" $ do
      it "produces valid literals" $
        producesValid ConversionRate.toDecimalLiteral
    describe "fromDecimalLiteral" $ do
      it "produces valid conversion rates" $
        producesValid
          ConversionRate.fromDecimalLiteral
      it "roundtrips with toDecimalLiteral" $
        forAllValid $ \cr ->
          case toDecimalLiteral cr of
            Nothing -> pure () -- Fine
            Just dl -> fromDecimalLiteral dl `shouldBe` Just cr

  describe "oneToOne" $ do
    it "is valid" $
      shouldBeValid ConversionRate.oneToOne

    it "converts without changing the amount" $
      ConversionRate.toRational ConversionRate.oneToOne `shouldBe` 1

  describe "invert" $
    it "produces valid rates" $
      producesValid ConversionRate.invert

  describe "compose" $ do
    it "produces valid rates" $
      producesValid2 ConversionRate.compose

    it "multiplies the rates" $
      ConversionRate.compose (ConversionRate (2 :% 1)) (ConversionRate (3 :% 1))
        `shouldBe` ConversionRate (6 :% 1)

    it "results in the product of the two rates as ratios" $
      forAllValid $ \cr1 ->
        forAllValid $ \cr2 ->
          ConversionRate.toRatio (ConversionRate.compose cr1 cr2)
            `shouldBe` ConversionRate.toRatio cr1 * ConversionRate.toRatio cr2
