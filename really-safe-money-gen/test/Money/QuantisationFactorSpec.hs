{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Money.QuantisationFactorSpec (spec) where

import Data.GenValidity.Vector ()
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Money.QuantisationFactor
import qualified Money.QuantisationFactor as QuantisationFactor
import Money.QuantisationFactor.Gen ()
import Numeric.DecimalLiteral (DecimalLiteral (..))
import Numeric.DecimalLiteral.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (* 100) . modifyMaxSize (* 3) $ do
  genValidSpec @QuantisationFactor

  describe "fromWord32" $
    it "produces valid quantisation factors" $
      producesValid QuantisationFactor.fromWord32

  describe "digits" $ do
    it "works on 1" $
      QuantisationFactor.digits (QuantisationFactor 1) `shouldBe` 0

    it "works on 10" $
      QuantisationFactor.digits (QuantisationFactor 10) `shouldBe` 1

    it "works on 20" $
      QuantisationFactor.digits (QuantisationFactor 20) `shouldBe` 2

    it "works on 100" $
      QuantisationFactor.digits (QuantisationFactor 100) `shouldBe` 2

    it "produces valid numbers of digits" $
      producesValid QuantisationFactor.digits

  describe "DecimalLiteral" $ do
    decimalLiteralExampleSpec (DecimalLiteral Nothing 1 0) (QuantisationFactor 1)
    decimalLiteralExampleSpec (DecimalLiteral Nothing 1 1) (QuantisationFactor 10)
    decimalLiteralExampleSpec (DecimalLiteral Nothing 1 2) (QuantisationFactor 100)
    decimalLiteralExampleSpec (DecimalLiteral Nothing 1 3) (QuantisationFactor 1_000)
    decimalLiteralExampleSpec (DecimalLiteral Nothing 5 2) (QuantisationFactor 20)
    decimalLiteralExampleSpec (DecimalLiteral Nothing 2 2) (QuantisationFactor 50)

    describe "fromDecimalLiteral" $ do
      it "produces valid factors" $
        producesValid fromDecimalLiteral

      it "fails to render negative fractionals" $
        forAllValid $ \m ->
          forAllValid $ \e ->
            fromDecimalLiteral (DecimalLiteral (Just False) m e) `shouldBe` Nothing

      it "fails to render a 0" $
        forAllValid $ \mSign ->
          forAllValid $ \e ->
            fromDecimalLiteral (DecimalLiteral mSign 0 e) `shouldBe` Nothing

      it "fails to render a non-1 integer" $
        forAllValid $ \mSign ->
          forAllValid $ \a ->
            fromDecimalLiteral (DecimalLiteral mSign (succ (succ a)) 0) `shouldBe` Nothing

    describe "toDecimalLiteral" $ do
      it "produces valid literals" $
        producesValid toDecimalLiteral

      it "roundtrips with fromDecimalLiteral" $
        forAllValid $ \qf -> do
          case toDecimalLiteral qf of
            Nothing -> pure () -- Fine
            Just dl ->
              context (show dl) $
                case fromDecimalLiteral dl of
                  Nothing -> expectationFailure "Should have been able to parse as an account"
                  Just q -> q `shouldBe` qf

decimalLiteralExampleSpec :: (HasCallStack) => DecimalLiteral -> QuantisationFactor -> Spec
decimalLiteralExampleSpec dl qf =
  withFrozenCallStack $ do
    decimalLiteralParseExampleSpec dl qf
    decimalLiteralRenderExampleSpec dl qf

decimalLiteralRenderExampleSpec :: (HasCallStack) => DecimalLiteral -> QuantisationFactor -> Spec
decimalLiteralRenderExampleSpec dl qf =
  withFrozenCallStack $
    it (unwords ["can turn quantisation factor", show (unQuantisationFactor qf), "into", show dl]) $
      toDecimalLiteral qf `shouldBe` Just dl

decimalLiteralParseExampleSpec :: (HasCallStack) => DecimalLiteral -> QuantisationFactor -> Spec
decimalLiteralParseExampleSpec dl qf =
  withFrozenCallStack $
    it (unwords ["can turn", show dl, "into quantisation factor", show (unQuantisationFactor qf)]) $
      fromDecimalLiteral dl `shouldBe` Just qf
