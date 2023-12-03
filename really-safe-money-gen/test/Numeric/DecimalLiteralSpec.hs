{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.DecimalLiteralSpec (spec) where

import Data.Scientific
import Money.Account (Account (..))
import Money.Account.Gen ()
import Money.Amount (Amount (..))
import Money.QuantisationFactor
import Money.QuantisationFactor.Gen ()
import Numeric.DecimalLiteral
import Numeric.DecimalLiteral.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @DecimalLiteral
  describe "renderDecimalLiteral" $ do
    it "can render any decimal literal" $
      producesValid renderDecimalLiteral

    it "can render 1 without a decimal point" $
      renderDecimalLiteral (DecimalLiteral False 0 (scientific 1 0)) `shouldBe` "1"

    it "can render 1 with a sign" $
      renderDecimalLiteral (DecimalLiteral True 0 (scientific 1 0)) `shouldBe` "+1"

    it "can render 1 with enough digits" $
      renderDecimalLiteral (DecimalLiteral True 2 (scientific 1 0)) `shouldBe` "+1.00"

    it "can render 0.5 with a decimal point" $
      renderDecimalLiteral (DecimalLiteral False 1 (scientific 5 (-1))) `shouldBe` "0.5"

    it "can render 57646075230342348.8 correctly" $
      renderDecimalLiteral (DecimalLiteral False 1 (scientific 576_460_752_303_423_488 (-1)))
        `shouldBe` "57646075230342348.8"

  describe "parseDecimalLiteral" $ do
    it "can parse 1" $
      parseDecimalLiteral "1" `shouldBe` Just (DecimalLiteral False 0 (scientific 1 0))

    it "can parse 100" $
      parseDecimalLiteral "100" `shouldBe` Just (DecimalLiteral False 0 (scientific 1 2))

    it "can parse 1 cent" $
      parseDecimalLiteral "0.01" `shouldBe` Just (DecimalLiteral False 2 (scientific 1 (-2)))

    it "can parse 12.00045" $
      parseDecimalLiteral "12.00045" `shouldBe` Just (DecimalLiteral False 5 (scientific 1_200_045 (-5)))

    it "fails to parse a number in scientific notation" $
      parseDecimalLiteral "1E0" `shouldBe` Nothing

    it "can parse any rendered decimal literal" $
      forAllValid $ \decimalLiteral -> do
        let rendered = renderDecimalLiteral decimalLiteral
        context (show rendered) $ case parseDecimalLiteral rendered of
          Nothing -> expectationFailure "could not parse."
          Just dl -> decimalLiteralScientific dl `shouldBe` decimalLiteralScientific decimalLiteral

  describe "toQuantisationFactor" $ do
    it "produces valid factors" $
      producesValid toQuantisationFactor

    it "succeeds on the units example" $
      toQuantisationFactor (DecimalLiteral False 0 (scientific 1 0)) `shouldBe` Just (QuantisationFactor 1)

    it "succeeds on this tens example" $
      toQuantisationFactor (DecimalLiteral True 1 (scientific 1 (-1))) `shouldBe` Just (QuantisationFactor 10)

    it "succeeds on the cents example" $
      toQuantisationFactor (DecimalLiteral False 2 (scientific 1 (-2))) `shouldBe` Just (QuantisationFactor 100)

    it "succeeds on the rappen example" $
      toQuantisationFactor (DecimalLiteral True 3 (scientific 5 (-2))) `shouldBe` Just (QuantisationFactor 20)

    it "fails on a non-integer factor 0.01" $
      toQuantisationFactor (DecimalLiteral False 4 (scientific 1 2)) `shouldBe` Nothing

    it "fails on a negative factor" $
      toQuantisationFactor (DecimalLiteral True 5 (scientific (-1) 0)) `shouldBe` Nothing

  describe "fromQuantisationFactor" $ do
    it "succeeds on this unit example" $
      fromQuantisationFactor (QuantisationFactor 1) `shouldBe` Just (DecimalLiteral False 0 (scientific 1 0))

    it "succeeds on this cent example" $
      fromQuantisationFactor (QuantisationFactor 100) `shouldBe` Just (DecimalLiteral False 2 (scientific 1 (-2)))

    it "succeeds on this rappen example" $
      fromQuantisationFactor (QuantisationFactor 20) `shouldBe` Just (DecimalLiteral False 2 (scientific 5 (-2)))

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

  describe "toAccount" $ do
    it "produces valid factors" $
      producesValid2 toAccount

    it "succeeds on this unit example" $
      toAccount (QuantisationFactor 100) (DecimalLiteral False 2 (scientific 1 0)) `shouldBe` Just (Positive (Amount 100))

    it "succeeds on this cent example" $
      toAccount (QuantisationFactor 100) (DecimalLiteral True 2 (scientific 1 (-2))) `shouldBe` Just (Positive (Amount 1))

    it "succeeds on this rappen example" $
      toAccount (QuantisationFactor 20) (DecimalLiteral False 2 (scientific 5 (-2))) `shouldBe` Just (Positive (Amount 1))

    it "succeeds on this BTC example" $
      toAccount (QuantisationFactor 100_000_000) (DecimalLiteral True 8 (scientific 5 (-6))) `shouldBe` Just (Positive (Amount 500))

    it "fails on an amount that is too precise" $
      toAccount (QuantisationFactor 100) (DecimalLiteral False 2 (scientific 1 (-3))) `shouldBe` Nothing

  describe "fromAccount" $ do
    it "succeeds on this unit example" $
      fromAccount (QuantisationFactor 1) (Positive (Amount 1)) `shouldBe` Just (DecimalLiteral True 0 (scientific 1 0))

    it "succeeds on this cent example" $
      fromAccount (QuantisationFactor 100) (Positive (Amount 1)) `shouldBe` Just (DecimalLiteral True 2 (scientific 1 (-2)))

    it "succeeds on this rappen example" $
      fromAccount (QuantisationFactor 20) (Positive (Amount 1)) `shouldBe` Just (DecimalLiteral True 2 (scientific 5 (-2)))

    it "produces valid literals" $
      producesValid2 fromAccount

    it "roundtrips with toAccount" $
      forAllValid $ \acc ->
        forAllValid $ \qf -> do
          case fromAccount qf acc of
            Nothing -> pure () -- Fine
            Just dl ->
              context (show dl) $
                case toAccount qf dl of
                  Nothing -> expectationFailure "Should have been able to parse as an account"
                  Just a -> a `shouldBe` acc
