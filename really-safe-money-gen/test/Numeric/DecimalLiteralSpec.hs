{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.DecimalLiteralSpec (spec) where

import GHC.Stack
import Money.Account (Account (..))
import Money.Account.Gen ()
import Money.Amount (Amount (..))
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
    exampleSpec "1" (DecimalLiteralInteger Nothing 1)
    exampleSpec "+2" (DecimalLiteralInteger (Just True) 2)
    exampleSpec "-3" (DecimalLiteralInteger (Just False) 3)
    exampleSpec "400" (DecimalLiteralInteger Nothing 400)
    exampleSpec "5.0" (DecimalLiteralFractional Nothing 50 0)
    exampleSpec "6.00" (DecimalLiteralFractional Nothing 600 1)
    exampleSpec "7.000" (DecimalLiteralFractional Nothing 7_000 2)
    exampleSpec "0.8" (DecimalLiteralFractional Nothing 8 0)
    exampleSpec "0.09" (DecimalLiteralFractional Nothing 9 1)
    exampleSpec "0.001" (DecimalLiteralFractional Nothing 1 2)
    exampleSpec "0.0020" (DecimalLiteralFractional Nothing 20 3)
    exampleSpec "0.00300" (DecimalLiteralFractional Nothing 300 4)
    exampleSpec "12.00045" (DecimalLiteralFractional Nothing 1_200_045 4)

  describe "renderDecimalLiteral" $ do
    it "can render any decimal literal" $
      producesValid renderDecimalLiteral

  describe "parseDecimalLiteralM" $ do
    it "does the same as parseDecimalLiteral" $
      forAllValid $ \s ->
        parseDecimalLiteralM s `shouldBe` parseDecimalLiteral s

  describe "parseDecimalLiteral" $ do
    it "fails to parse scientific notation" $
      parseDecimalLiteral "1E1" `shouldBe` Nothing

    it "can parse any rendered decimal literal" $
      forAllValid $ \decimalLiteral -> do
        let rendered = renderDecimalLiteral decimalLiteral
        context (show rendered) $ case parseDecimalLiteral rendered of
          Nothing -> expectationFailure "could not parse."
          Just dl -> dl `shouldBe` decimalLiteral

  describe "Rational" $ do
    rationalExampleSpec (DecimalLiteralInteger Nothing 1) 1
    rationalExampleSpec (DecimalLiteralInteger Nothing 10) 10
    rationalExampleSpec (DecimalLiteralInteger Nothing 2) 2
    rationalExampleSpec (DecimalLiteralInteger (Just False) 3) (-3)
    rationalExampleSpec (DecimalLiteralInteger Nothing 400) 400
    rationalParseExampleSpec (DecimalLiteralInteger Nothing 5) 5
    rationalRenderExampleSpec (DecimalLiteralFractional Nothing 50 0) 5
    rationalParseExampleSpec (DecimalLiteralInteger (Just False) 6) (-6)
    rationalRenderExampleSpec (DecimalLiteralFractional (Just False) 600 1) (-6)
    rationalParseExampleSpec (DecimalLiteralInteger Nothing 7) 7
    rationalRenderExampleSpec (DecimalLiteralFractional Nothing 7_000 2) 7
    rationalExampleSpec (DecimalLiteralFractional (Just False) 8 0) (-0.8)
    rationalExampleSpec (DecimalLiteralFractional Nothing 9 1) 0.09
    rationalExampleSpec (DecimalLiteralFractional (Just False) 1 2) (-0.001)
    rationalParseExampleSpec (DecimalLiteralFractional Nothing 2 2) 0.002
    rationalRenderExampleSpec (DecimalLiteralFractional Nothing 20 3) 0.002
    rationalParseExampleSpec (DecimalLiteralFractional (Just False) 3 2) (-0.003)
    rationalRenderExampleSpec (DecimalLiteralFractional (Just False) 300 4) (-0.003)
    rationalExampleSpec (DecimalLiteralFractional Nothing 1_200_045 4) 12.000_45

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

  describe "QuantisationFactor" $ do
    quantisationFactorExampleSpec (DecimalLiteralInteger Nothing 1) (QuantisationFactor 1)
    quantisationFactorExampleSpec (DecimalLiteralFractional Nothing 1 0) (QuantisationFactor 10)
    quantisationFactorExampleSpec (DecimalLiteralFractional Nothing 1 1) (QuantisationFactor 100)
    quantisationFactorExampleSpec (DecimalLiteralFractional Nothing 1 2) (QuantisationFactor 1_000)
    quantisationFactorExampleSpec (DecimalLiteralFractional Nothing 5 1) (QuantisationFactor 20)
    quantisationFactorExampleSpec (DecimalLiteralFractional Nothing 2 1) (QuantisationFactor 50)

    describe "toQuantisationFactor" $ do
      it "produces valid factors" $
        producesValid toQuantisationFactor

      it "fails to render negative integers" $
        forAllValid $ \a ->
          toQuantisationFactor (DecimalLiteralInteger (Just False) a) `shouldBe` Nothing

      it "fails to render negative fractionals" $
        forAllValid $ \m ->
          forAllValid $ \e ->
            toQuantisationFactor (DecimalLiteralFractional (Just False) m e) `shouldBe` Nothing

      it "fails to render a 0 integer" $
        forAllValid $ \mSign ->
          toQuantisationFactor (DecimalLiteralInteger mSign 0) `shouldBe` Nothing

      it "fails to render a non-1 integer" $
        forAllValid $ \mSign ->
          forAllValid $ \a ->
            toQuantisationFactor (DecimalLiteralInteger mSign (succ (succ a))) `shouldBe` Nothing

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

  describe "Amount" $ do
    amountExampleSpec (QuantisationFactor 100) (DecimalLiteralFractional (Just True) 100 1) (Amount 100)
    amountExampleSpec (QuantisationFactor 100) (DecimalLiteralFractional (Just True) 200 1) (Amount 200)
    amountExampleSpec (QuantisationFactor 100) (DecimalLiteralFractional (Just True) 3 1) (Amount 3)
    amountExampleSpec (QuantisationFactor 100) (DecimalLiteralFractional (Just True) 4 1) (Amount 4)
    amountExampleSpec (QuantisationFactor 20) (DecimalLiteralFractional (Just True) 500 1) (Amount 100)
    amountExampleSpec (QuantisationFactor 20) (DecimalLiteralFractional (Just True) 600 1) (Amount 120)
    amountExampleSpec (QuantisationFactor 20) (DecimalLiteralFractional (Just True) 10 1) (Amount 2)
    amountExampleSpec (QuantisationFactor 20) (DecimalLiteralFractional (Just True) 20 1) (Amount 4)
    amountExampleSpec (QuantisationFactor 1) (DecimalLiteralInteger (Just True) 1) (Amount 1)
    amountExampleSpec (QuantisationFactor 1) (DecimalLiteralInteger (Just True) 2) (Amount 2)
    amountExampleSpec (QuantisationFactor 100_000_000) (DecimalLiteralFractional (Just True) 500 7) (Amount 500)

    describe "toAmount" $ do
      it "produces valid factors" $
        producesValid2 toAmount

      it "fails on this amount that is too precise" $
        toAmount (QuantisationFactor 100) (DecimalLiteralFractional (Just True) 1 3) `shouldBe` Nothing
      it "fails on this amount that is too precise" $
        toAmount (QuantisationFactor 20) (DecimalLiteralFractional (Just True) 1 2) `shouldBe` Nothing

    describe "fromAmount" $ do
      it "produces valid decimal literals" $
        producesValid2 fromAmount

      it "roundtrips with toAmount" $
        forAllValid $ \acc ->
          forAllValid $ \qf -> do
            case fromAmount qf acc of
              Nothing -> pure () -- Fine
              Just dl ->
                context (show dl) $
                  case toAmount qf dl of
                    Nothing -> expectationFailure "Should have been able to parse as an account"
                    Just a -> a `shouldBe` acc

  describe "Account" $ do
    accountExampleSpec (QuantisationFactor 100) (DecimalLiteralFractional (Just True) 100 1) (Positive (Amount 100))
    accountExampleSpec (QuantisationFactor 100) (DecimalLiteralFractional (Just False) 200 1) (Negative (Amount 200))
    accountExampleSpec (QuantisationFactor 100) (DecimalLiteralFractional (Just True) 3 1) (Positive (Amount 3))
    accountExampleSpec (QuantisationFactor 100) (DecimalLiteralFractional (Just False) 4 1) (Negative (Amount 4))
    accountExampleSpec (QuantisationFactor 20) (DecimalLiteralFractional (Just True) 500 1) (Positive (Amount 100))
    accountExampleSpec (QuantisationFactor 20) (DecimalLiteralFractional (Just False) 600 1) (Negative (Amount 120))
    accountExampleSpec (QuantisationFactor 20) (DecimalLiteralFractional (Just True) 10 1) (Positive (Amount 2))
    accountExampleSpec (QuantisationFactor 20) (DecimalLiteralFractional (Just False) 20 1) (Negative (Amount 4))
    accountExampleSpec (QuantisationFactor 1) (DecimalLiteralInteger (Just True) 1) (Positive (Amount 1))
    accountExampleSpec (QuantisationFactor 1) (DecimalLiteralInteger (Just False) 2) (Negative (Amount 2))
    accountExampleSpec (QuantisationFactor 100_000_000) (DecimalLiteralFractional (Just True) 500 7) (Positive (Amount 500))

    describe "toAccount" $ do
      it "produces valid factors" $
        producesValid2 toAccount

      it "fails on this amount that is too precise" $
        toAccount (QuantisationFactor 100) (DecimalLiteralFractional (Just False) 1 3) `shouldBe` Nothing
      it "fails on this amount that is too precise" $
        toAccount (QuantisationFactor 20) (DecimalLiteralFractional (Just False) 1 2) `shouldBe` Nothing

    describe "fromAccount" $ do
      it "produces valid decimal literals" $
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

exampleSpec :: HasCallStack => String -> DecimalLiteral -> Spec
exampleSpec s dl = do
  parseExampleSpec s dl
  renderExampleSpec s dl

parseExampleSpec :: HasCallStack => String -> DecimalLiteral -> Spec
parseExampleSpec s dl =
  withFrozenCallStack $
    it ("can parse " <> show s) $
      parseDecimalLiteral s `shouldBe` Just dl

renderExampleSpec :: HasCallStack => String -> DecimalLiteral -> Spec
renderExampleSpec s dl =
  withFrozenCallStack $
    it ("can render " <> show dl) $
      renderDecimalLiteral dl `shouldBe` s

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

amountExampleSpec :: HasCallStack => QuantisationFactor -> DecimalLiteral -> Amount -> Spec
amountExampleSpec qf dl a =
  withFrozenCallStack $ do
    amountParseExampleSpec qf dl a
    amountRenderExampleSpec qf dl a

amountParseExampleSpec :: HasCallStack => QuantisationFactor -> DecimalLiteral -> Amount -> Spec
amountParseExampleSpec qf dl a =
  withFrozenCallStack $
    it (unwords ["can turn amount", show qf, "into", show dl]) $
      fromAmount qf a `shouldBe` Just dl

amountRenderExampleSpec :: HasCallStack => QuantisationFactor -> DecimalLiteral -> Amount -> Spec
amountRenderExampleSpec qf dl a =
  withFrozenCallStack $
    it (unwords ["can turn", show dl, "into amount", show qf]) $
      toAmount qf dl `shouldBe` Just a

accountExampleSpec :: HasCallStack => QuantisationFactor -> DecimalLiteral -> Account -> Spec
accountExampleSpec qf dl a =
  withFrozenCallStack $ do
    accountParseExampleSpec qf dl a
    accountRenderExampleSpec qf dl a

accountParseExampleSpec :: HasCallStack => QuantisationFactor -> DecimalLiteral -> Account -> Spec
accountParseExampleSpec qf dl a =
  withFrozenCallStack $
    it (unwords ["can turn account", show qf, "into", show dl]) $
      fromAccount qf a `shouldBe` Just dl

accountRenderExampleSpec :: HasCallStack => QuantisationFactor -> DecimalLiteral -> Account -> Spec
accountRenderExampleSpec qf dl a =
  withFrozenCallStack $
    it (unwords ["can turn", show dl, "into account", show qf]) $
      toAccount qf dl `shouldBe` Just a
