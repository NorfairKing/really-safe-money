{-# LANGUAGE TypeApplications #-}

module Money.ConversionRateSpec (spec) where

import Data.GenValidity.Vector ()
import Money.ConversionRate
import qualified Money.ConversionRate as ConversionRate
import Money.ConversionRate.Gen ()
import Numeric.DecimalLiteral.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (* 100) . modifyMaxSize (* 3) $ do
  genValidSpec @ConversionRate

  describe "toRatio" $ do
    it "produces valid ratios" $
      producesValid ConversionRate.toRatio
  describe "fromRatio" $ do
    it "produces valid conversion rates" $
      producesValid ConversionRate.fromRatio
    it "roundtrips with toRatio" $
      forAllValid $ \cr ->
        ConversionRate.fromRatio (ConversionRate.toRatio cr) `shouldBe` Just cr

  describe "toRational" $ do
    it "produces valid rationals" $
      producesValid ConversionRate.toRational
  describe "fromRational" $ do
    it "produces valid conversion rates" $
      producesValid ConversionRate.fromRational
    it "roundtrips with toRational" $
      forAllValid $ \cr ->
        ConversionRate.fromRational (ConversionRate.toRational cr) `shouldBe` Just cr

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
