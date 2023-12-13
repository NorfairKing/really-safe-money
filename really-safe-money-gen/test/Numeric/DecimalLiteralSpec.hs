{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.DecimalLiteralSpec (spec) where

import GHC.Stack
import Money.Account.Gen ()
import Money.QuantisationFactor
import Money.QuantisationFactor.Gen ()
import Numeric.DecimalLiteral as DecimalLiteral
import Numeric.DecimalLiteral.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @DecimalLiteral
  describe "examples" $ do
    exampleSpec "1" (DecimalLiteral Nothing 1 0)
    exampleSpec "+2" (DecimalLiteral (Just True) 2 0)
    exampleSpec "-3" (DecimalLiteral (Just False) 3 0)
    exampleSpec "400" (DecimalLiteral Nothing 400 0)
    exampleSpec "5.0" (DecimalLiteral Nothing 50 1)
    exampleSpec "6.00" (DecimalLiteral Nothing 600 2)
    exampleSpec "7.000" (DecimalLiteral Nothing 7_000 3)
    exampleSpec "0.8" (DecimalLiteral Nothing 8 1)
    exampleSpec "0.09" (DecimalLiteral Nothing 9 2)
    exampleSpec "0.001" (DecimalLiteral Nothing 1 3)
    exampleSpec "0.0020" (DecimalLiteral Nothing 20 4)
    exampleSpec "0.00300" (DecimalLiteral Nothing 300 5)
    exampleSpec "12.00045" (DecimalLiteral Nothing 1_200_045 5)

  describe "format" $ do
    it "can render any decimal literal" $
      producesValid DecimalLiteral.format

  describe "DecimalLiteral.fromStringM" $ do
    it "does the same as DecimalLiteral.fromString" $
      forAllValid $ \s ->
        DecimalLiteral.fromStringM s `shouldBe` DecimalLiteral.fromString s

  describe "DecimalLiteral.fromString" $ do
    it "fails to parse scientific notation" $
      DecimalLiteral.fromString "1E1" `shouldBe` Nothing

    it "can parse any rendered decimal literal" $
      forAllValid $ \decimalLiteral -> do
        let rendered = DecimalLiteral.format decimalLiteral
        context (show rendered) $ case DecimalLiteral.fromString rendered of
          Nothing -> expectationFailure "could not parse."
          Just dl -> dl `shouldBe` decimalLiteral

  describe "Rational" $ do
    rationalExampleSpec (DecimalLiteral Nothing 1 0) 1
    rationalExampleSpec (DecimalLiteral Nothing 10 0) 10
    rationalExampleSpec (DecimalLiteral Nothing 2 0) 2
    rationalExampleSpec (DecimalLiteral (Just False) 3 0) (-3)
    rationalExampleSpec (DecimalLiteral Nothing 400 0) 400
    rationalParseExampleSpec (DecimalLiteral Nothing 5 0) 5
    rationalRenderExampleSpec (DecimalLiteral Nothing 50 1) 5
    rationalParseExampleSpec (DecimalLiteral (Just False) 6 0) (-6)
    rationalRenderExampleSpec (DecimalLiteral (Just False) 600 2) (-6)
    rationalParseExampleSpec (DecimalLiteral Nothing 7 0) 7
    rationalRenderExampleSpec (DecimalLiteral Nothing 7_000 3) 7
    rationalExampleSpec (DecimalLiteral (Just False) 8 1) (-0.8)
    rationalExampleSpec (DecimalLiteral Nothing 9 2) 0.09
    rationalExampleSpec (DecimalLiteral (Just False) 1 3) (-0.001)
    rationalParseExampleSpec (DecimalLiteral Nothing 2 3) 0.002
    rationalRenderExampleSpec (DecimalLiteral Nothing 20 4) 0.002
    rationalParseExampleSpec (DecimalLiteral (Just False) 3 3) (-0.003)
    rationalRenderExampleSpec (DecimalLiteral (Just False) 300 5) (-0.003)
    rationalExampleSpec (DecimalLiteral Nothing 1_200_045 5) 12.000_45

    describe "toRational" $ do
      it "renders to valid rationals" $
        producesValid DecimalLiteral.toRational

    describe "fromRational" $ do
      it "renders to valid decimal literals" $
        producesValid DecimalLiteral.fromRational

      it "can parse any rendered rational" $
        forAllValid $ \decimalLiteral -> do
          let r = DecimalLiteral.toRational decimalLiteral
          context (show r) $ case DecimalLiteral.fromRational r of
            Nothing -> expectationFailure "could not parse rational."
            Just actual ->
              context (show actual) $
                DecimalLiteral.toRational actual `shouldBe` DecimalLiteral.toRational decimalLiteral

  describe "Ratio" $ do
    describe "toRatio" $ do
      it "renders to valid rationals" $
        producesValid DecimalLiteral.toRatio

    describe "fromRatio" $ do
      it "renders to valid decimal literals" $
        producesValid DecimalLiteral.fromRatio

      it "can parse any rendered rational" $
        forAllValid $ \decimalLiteral -> do
          case DecimalLiteral.toRatio decimalLiteral of
            Nothing -> pure () -- Fine
            Just r ->
              context (show r) $ case DecimalLiteral.fromRatio r of
                Nothing -> expectationFailure "could not parse rational."
                Just actual ->
                  context (show actual) $
                    DecimalLiteral.toRatio actual `shouldBe` DecimalLiteral.toRatio decimalLiteral

  describe "QuantisationFactor" $ do
    quantisationFactorExampleSpec (DecimalLiteral Nothing 1 0) (QuantisationFactor 1)
    quantisationFactorExampleSpec (DecimalLiteral Nothing 1 1) (QuantisationFactor 10)
    quantisationFactorExampleSpec (DecimalLiteral Nothing 1 2) (QuantisationFactor 100)
    quantisationFactorExampleSpec (DecimalLiteral Nothing 1 3) (QuantisationFactor 1_000)
    quantisationFactorExampleSpec (DecimalLiteral Nothing 5 2) (QuantisationFactor 20)
    quantisationFactorExampleSpec (DecimalLiteral Nothing 2 2) (QuantisationFactor 50)

    describe "toQuantisationFactor" $ do
      it "produces valid factors" $
        producesValid toQuantisationFactor

      it "fails to render negative fractionals" $
        forAllValid $ \m ->
          forAllValid $ \e ->
            toQuantisationFactor (DecimalLiteral (Just False) m e) `shouldBe` Nothing

      it "fails to render a 0" $
        forAllValid $ \mSign ->
          forAllValid $ \e ->
            toQuantisationFactor (DecimalLiteral mSign 0 e) `shouldBe` Nothing

      it "fails to render a non-1 integer" $
        forAllValid $ \mSign ->
          forAllValid $ \a ->
            toQuantisationFactor (DecimalLiteral mSign (succ (succ a)) 0) `shouldBe` Nothing

    describe "fromQuantisationFactor" $ do
      it "produces valid literals" $
        producesValid fromQuantisationFactor

      it "roundtrips with toQuantisationFactor" $
        forAllValid $ \qf -> do
          case fromQuantisationFactor qf of
            Nothing -> pure () -- Fine
            Just dl ->
              context (show dl) $
                case toQuantisationFactor dl of
                  Nothing -> expectationFailure "Should have been able to parse as an account"
                  Just q -> q `shouldBe` qf

  describe "setSignRequired" $
    it "produces valid values" $
      producesValid setSignRequired

  describe "setSignOptional" $
    it "produces valid values" $
      producesValid setSignOptional

  describe "digits" $
    it "produces valid numbers of digits" $
      producesValid DecimalLiteral.digits

  describe "setMinimumDigits" $ do
    it "produces valid literals" $
      producesValid2 DecimalLiteral.setMinimumDigits

    it "does not change the value of the literal" $
      forAllValid $ \d ->
        forAllValid $ \dl ->
          let dl' = DecimalLiteral.setMinimumDigits d dl
           in DecimalLiteral.toRational dl' `shouldBe` DecimalLiteral.toRational dl

    it "produces values with more than the given number of digits" $
      forAllValid $ \d ->
        forAllValid $ \dl ->
          let dl' = DecimalLiteral.setMinimumDigits d dl
           in DecimalLiteral.digits dl' `shouldSatisfy` (>= d)

exampleSpec :: HasCallStack => String -> DecimalLiteral -> Spec
exampleSpec s dl =
  withFrozenCallStack $ do
    parseExampleSpec s dl
    renderExampleSpec s dl

parseExampleSpec :: HasCallStack => String -> DecimalLiteral -> Spec
parseExampleSpec s dl =
  withFrozenCallStack $
    it ("can parse " <> show s) $
      DecimalLiteral.fromString s `shouldBe` Just dl

renderExampleSpec :: HasCallStack => String -> DecimalLiteral -> Spec
renderExampleSpec s dl =
  withFrozenCallStack $
    it ("can render " <> show dl) $
      DecimalLiteral.format dl `shouldBe` s

rationalExampleSpec :: HasCallStack => DecimalLiteral -> Rational -> Spec
rationalExampleSpec dl qf =
  withFrozenCallStack $ do
    rationalParseExampleSpec dl qf
    rationalRenderExampleSpec dl qf

rationalParseExampleSpec :: HasCallStack => DecimalLiteral -> Rational -> Spec
rationalParseExampleSpec dl r =
  withFrozenCallStack $
    it (unwords ["can turn rational", show r, "into", show dl]) $
      DecimalLiteral.fromRational r `shouldBe` Just dl

rationalRenderExampleSpec :: HasCallStack => DecimalLiteral -> Rational -> Spec
rationalRenderExampleSpec dl r =
  withFrozenCallStack $
    it (unwords ["can turn", show dl, "into rational", show r]) $
      DecimalLiteral.toRational dl `shouldBe` r

quantisationFactorExampleSpec :: HasCallStack => DecimalLiteral -> QuantisationFactor -> Spec
quantisationFactorExampleSpec dl qf =
  withFrozenCallStack $ do
    quantisationFactorParseExampleSpec dl qf
    quantisationFactorRenderExampleSpec dl qf

quantisationFactorParseExampleSpec :: HasCallStack => DecimalLiteral -> QuantisationFactor -> Spec
quantisationFactorParseExampleSpec dl qf =
  withFrozenCallStack $
    it (unwords ["can turn quantisation factor", show (unQuantisationFactor qf), "into", show dl]) $
      fromQuantisationFactor qf `shouldBe` Just dl

quantisationFactorRenderExampleSpec :: HasCallStack => DecimalLiteral -> QuantisationFactor -> Spec
quantisationFactorRenderExampleSpec dl qf =
  withFrozenCallStack $
    it (unwords ["can turn", show dl, "into quantisation factor", show (unQuantisationFactor qf)]) $
      toQuantisationFactor dl `shouldBe` Just qf
