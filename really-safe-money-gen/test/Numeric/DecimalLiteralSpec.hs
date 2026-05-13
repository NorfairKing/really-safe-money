{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.DecimalLiteralSpec (spec) where

import Control.Exception
import Data.Ratio
import GHC.Stack
import Money.Account.Gen ()
import Numeric.DecimalLiteral as DecimalLiteral
import Numeric.DecimalLiteral.Gen ()
import Numeric.Natural
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @DecimalLiteral
  describe "IsString" $ do
    it "parses a valid literal via OverloadedStrings" $
      ("1.5" :: DecimalLiteral) `shouldBe` DecimalLiteral Nothing 15 1

    it "throws an informative error for an invalid string literal" $ do
      result <- try (evaluate ("1E1" :: DecimalLiteral))
      case result of
        Left (err :: SomeException) -> show err `shouldContain` "Invalid DecimalLiteral"
        Right _ -> expectationFailure "Should have thrown an exception"

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

  describe "toString" $ do
    it "can render any decimal literal" $
      producesValid DecimalLiteral.toString

  describe "DecimalLiteral.fromStringM" $ do
    it "does the same as DecimalLiteral.fromString" $
      forAllValid $ \s ->
        DecimalLiteral.fromStringM s `shouldBe` DecimalLiteral.fromString s

    it "succeeds when fromString succeeds" $
      DecimalLiteral.fromStringM "1.5" `shouldBe` Just (DecimalLiteral Nothing 15 1)

    it "fails when fromString fails" $
      (DecimalLiteral.fromStringM "1E1" :: Maybe DecimalLiteral) `shouldBe` Nothing

  describe "DecimalLiteral.fromString" $ do
    it "fails to parse scientific notation" $
      DecimalLiteral.fromString "1E1" `shouldBe` Nothing

    it "parses an integer followed by non-digit characters (stops at the non-digit)" $
      DecimalLiteral.fromString "5" `shouldBe` Just (DecimalLiteral Nothing 5 0)

    it "parses a decimal number with a fractional part" $
      DecimalLiteral.fromString "0.8" `shouldBe` Just (DecimalLiteral Nothing 8 1)

    it "can parse any rendered decimal literal" $
      forAllValid $ \decimalLiteral -> do
        let rendered = DecimalLiteral.toString decimalLiteral
        context (show rendered) $ case DecimalLiteral.fromString rendered of
          Nothing -> expectationFailure "could not parse."
          Just dl -> dl `shouldBe` decimalLiteral

  describe "Natural" $ do
    describe "toNatural" $ do
      it "renders to valid naturals" $
        producesValid DecimalLiteral.toNatural

    describe "fromNatural" $ do
      it "renders to valid decimal literals" $
        producesValid DecimalLiteral.fromNatural

      it "can parse any rendered literal" $
        forAllValid $ \nat -> do
          let dl = DecimalLiteral.fromWord nat
          context (show dl) $ DecimalLiteral.toWord dl `shouldBe` Just nat

  describe "Word" $ do
    describe "toWord" $ do
      it "renders to valid words" $
        producesValid DecimalLiteral.toWord

      it "fails on a value exceeding maxBound Word" $
        DecimalLiteral.toWord (DecimalLiteral Nothing (fromIntegral (maxBound :: Word) + 1) 0) `shouldBe` Nothing

      it "succeeds on maxBound Word" $
        DecimalLiteral.toWord (DecimalLiteral Nothing (fromIntegral (maxBound :: Word)) 0) `shouldBe` Just maxBound

    describe "fromWord" $ do
      it "renders to valid decimal literals" $
        producesValid DecimalLiteral.fromWord

      it "can parse any rendered literal" $
        forAllValid $ \word -> do
          let dl = DecimalLiteral.fromWord word
          context (show dl) $ DecimalLiteral.toWord dl `shouldBe` Just word

  describe "Integer" $ do
    describe "toInteger" $ do
      it "renders to valid words" $
        producesValid DecimalLiteral.toInteger

    describe "fromInteger" $ do
      it "renders to valid decimal literals" $
        producesValid DecimalLiteral.fromInteger

      it "represents 0 without a negative sign" $
        DecimalLiteral.fromInteger 0 `shouldBe` DecimalLiteral Nothing 0 0

      it "represents positive integers with no sign" $
        DecimalLiteral.fromInteger 5 `shouldBe` DecimalLiteral Nothing 5 0

      it "represents negative integers with Just False sign" $
        DecimalLiteral.fromInteger (-3) `shouldBe` DecimalLiteral (Just False) 3 0

      it "can parse any rendered literal" $
        forAllValid $ \int -> do
          let dl = DecimalLiteral.fromInteger int
          context (show dl) $ DecimalLiteral.toInteger dl `shouldBe` Just int

  describe "Int" $ do
    describe "toInt" $ do
      it "renders to valid words" $
        producesValid DecimalLiteral.toInt

      it "fails on a value exceeding maxBound Int" $
        -- maxBound Int is 2^63-1, so 2^63 exceeds it
        DecimalLiteral.toInt (DecimalLiteral Nothing (2 ^ (63 :: Int)) 0) `shouldBe` Nothing

      it "fails on a value below minBound Int" $
        -- minBound Int is -(2^63), so -(2^63+1) is below it
        DecimalLiteral.toInt (DecimalLiteral (Just False) (2 ^ (63 :: Int) + 1) 0) `shouldBe` Nothing

      it "succeeds on maxBound Int" $
        DecimalLiteral.toInt (DecimalLiteral (Just True) (fromIntegral (maxBound :: Int)) 0) `shouldBe` Just maxBound

      it "succeeds on minBound Int" $
        -- minBound Int is -(2^63), represented as sign=False, mantissa=2^63
        DecimalLiteral.toInt (DecimalLiteral (Just False) (2 ^ (63 :: Int)) 0) `shouldBe` Just minBound

    describe "fromInt" $ do
      it "renders to valid decimal literals" $
        producesValid DecimalLiteral.fromInt

      it "can parse any rendered literal" $
        forAllValid $ \int -> do
          let dl = DecimalLiteral.fromInt int
          context (show dl) $ DecimalLiteral.toInt dl `shouldBe` Just int

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

      it "renders 0 as a non-negative literal" $
        case DecimalLiteral.fromRational 0 of
          Nothing -> expectationFailure "Should have parsed 0"
          Just dl -> dl `shouldSatisfy` (\(DecimalLiteral mS _ _) -> mS /= Just False)

      it "renders -1 with a negative sign" $
        case DecimalLiteral.fromRational (-1) of
          Nothing -> expectationFailure "Should have parsed -1"
          Just dl -> dl `shouldSatisfy` (\(DecimalLiteral mS _ _) -> mS == Just False)

      it "renders -3 with a negative sign" $
        case DecimalLiteral.fromRational (-3) of
          Nothing -> expectationFailure "Should have parsed -3"
          Just dl -> dl `shouldSatisfy` (\(DecimalLiteral mS _ _) -> mS == Just False)

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

      it "gives 1 % 10 for DecimalLiteral Nothing 1 1" $
        DecimalLiteral.toRatio (DecimalLiteral Nothing 1 1) `shouldBe` Just (1 % 10)

      it "gives Nothing for a negative literal" $
        DecimalLiteral.toRatio (DecimalLiteral (Just False) 3 1) `shouldBe` Nothing

    describe "fromRatio" $ do
      it "renders to valid decimal literals" $
        producesValid DecimalLiteral.fromRatio

      it "fails at the 256-digit limit (1 / 2^256 needs 256 fractional digits)" $
        DecimalLiteral.fromRatio (1 % ((2 :: Natural) ^ (256 :: Int)))
          `shouldBe` Nothing

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

  describe "setSignRequired" $ do
    it "produces valid values" $
      producesValid setSignRequired

    it "turns a Nothing sign into Just True (positive)" $
      setSignRequired (DecimalLiteral Nothing 5 0) `shouldBe` DecimalLiteral (Just True) 5 0

    it "does not change an existing positive sign" $
      setSignRequired (DecimalLiteral (Just True) 5 0) `shouldBe` DecimalLiteral (Just True) 5 0

    it "does not change an existing negative sign" $
      setSignRequired (DecimalLiteral (Just False) 5 0) `shouldBe` DecimalLiteral (Just False) 5 0

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

exampleSpec :: (HasCallStack) => String -> DecimalLiteral -> Spec
exampleSpec s dl =
  withFrozenCallStack $ do
    parseExampleSpec s dl
    renderExampleSpec s dl

parseExampleSpec :: (HasCallStack) => String -> DecimalLiteral -> Spec
parseExampleSpec s dl =
  withFrozenCallStack $
    it ("can parse " <> show s) $
      DecimalLiteral.fromString s `shouldBe` Just dl

renderExampleSpec :: (HasCallStack) => String -> DecimalLiteral -> Spec
renderExampleSpec s dl =
  withFrozenCallStack $
    it ("can render " <> show dl) $
      DecimalLiteral.toString dl `shouldBe` s

rationalExampleSpec :: (HasCallStack) => DecimalLiteral -> Rational -> Spec
rationalExampleSpec dl qf =
  withFrozenCallStack $ do
    rationalParseExampleSpec dl qf
    rationalRenderExampleSpec dl qf

rationalParseExampleSpec :: (HasCallStack) => DecimalLiteral -> Rational -> Spec
rationalParseExampleSpec dl r =
  withFrozenCallStack $
    it (unwords ["can turn rational", show r, "into", show dl]) $
      DecimalLiteral.fromRational r `shouldBe` Just dl

rationalRenderExampleSpec :: (HasCallStack) => DecimalLiteral -> Rational -> Spec
rationalRenderExampleSpec dl r =
  withFrozenCallStack $
    it (unwords ["can turn", show dl, "into rational", show r]) $
      DecimalLiteral.toRational dl `shouldBe` r
