{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.DecimalLiteralSpec (spec) where

import GHC.Stack
import Money.Account (Account (..))
import Money.Account.Gen ()
import Money.Amount (Amount (..))
import Money.QuantisationFactor
import Money.QuantisationFactor.Gen ()
import Numeric.DecimalLiteral
import Numeric.DecimalLiteral.Gen ()
import Test.Syd
import Test.Syd.Validity

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

  describe "parseDecimalLiteral" $ do
    it "can parse any rendered decimal literal" $
      forAllValid $ \decimalLiteral -> do
        let rendered = renderDecimalLiteral decimalLiteral
        context (show rendered) $ case parseDecimalLiteral rendered of
          Nothing -> expectationFailure "could not parse."
          Just dl -> dl `shouldBe` decimalLiteral

--
--   describe "toQuantisationFactor" $ do
--     it "produces valid factors" $
--       producesValid toQuantisationFactor
--
--     it "succeeds on the units example" $
--       toQuantisationFactor (DecimalLiteral False 0 (scientific 1 0)) `shouldBe` Just (QuantisationFactor 1)
--
--     it "succeeds on this tens example" $
--       toQuantisationFactor (DecimalLiteral True 1 (scientific 1 (-1))) `shouldBe` Just (QuantisationFactor 10)
--
--     it "succeeds on the cents example" $
--       toQuantisationFactor (DecimalLiteral False 2 (scientific 1 (-2))) `shouldBe` Just (QuantisationFactor 100)
--
--     it "succeeds on the rappen example" $
--       toQuantisationFactor (DecimalLiteral True 3 (scientific 5 (-2))) `shouldBe` Just (QuantisationFactor 20)
--
--     it "fails on a non-integer factor 0.01" $
--       toQuantisationFactor (DecimalLiteral False 4 (scientific 1 2)) `shouldBe` Nothing
--
--     it "fails on a negative factor" $
--       toQuantisationFactor (DecimalLiteral True 5 (scientific (-1) 0)) `shouldBe` Nothing
--
--   describe "fromQuantisationFactor" $ do
--     it "succeeds on this unit example" $
--       fromQuantisationFactor (QuantisationFactor 1) `shouldBe` Just (DecimalLiteral False 0 (scientific 1 0))
--
--     it "succeeds on this cent example" $
--       fromQuantisationFactor (QuantisationFactor 100) `shouldBe` Just (DecimalLiteral False 2 (scientific 1 (-2)))
--
--     it "succeeds on this rappen example" $
--       fromQuantisationFactor (QuantisationFactor 20) `shouldBe` Just (DecimalLiteral False 2 (scientific 5 (-2)))
--
--     it "produces valid literals" $
--       producesValid fromQuantisationFactor
--
--     it "roundtrips with toQuantisationFactor" $
--       forAllValid $ \qf -> do
--         case fromQuantisationFactor qf of
--           Nothing -> pure () -- Fine
--           Just dl ->
--             context (show dl) $
--               case toQuantisationFactor dl of
--                 Nothing -> expectationFailure "Should have been able to parse as an account"
--                 Just q -> q `shouldBe` qf
--
--   describe "toAccount" $ do
--     it "produces valid factors" $
--       producesValid2 toAccount
--
--     it "succeeds on this unit example" $
--       toAccount (QuantisationFactor 100) (DecimalLiteral False 2 (scientific 1 0)) `shouldBe` Just (Positive (Amount 100))
--
--     it "succeeds on this cent example" $
--       toAccount (QuantisationFactor 100) (DecimalLiteral True 2 (scientific 1 (-2))) `shouldBe` Just (Positive (Amount 1))
--
--     it "succeeds on this rappen example" $
--       toAccount (QuantisationFactor 20) (DecimalLiteral False 2 (scientific 5 (-2))) `shouldBe` Just (Positive (Amount 1))
--
--     it "succeeds on this BTC example" $
--       toAccount (QuantisationFactor 100_000_000) (DecimalLiteral True 8 (scientific 5 (-6))) `shouldBe` Just (Positive (Amount 500))
--
--     it "fails on an amount that is too precise" $
--       toAccount (QuantisationFactor 100) (DecimalLiteral False 2 (scientific 1 (-3))) `shouldBe` Nothing
--
--   describe "fromAccount" $ do
--     it "succeeds on this unit example" $
--       fromAccount (QuantisationFactor 1) (Positive (Amount 1)) `shouldBe` Just (DecimalLiteral True 0 (scientific 1 0))
--
--     it "succeeds on this cent example" $
--       fromAccount (QuantisationFactor 100) (Positive (Amount 1)) `shouldBe` Just (DecimalLiteral True 2 (scientific 1 (-2)))
--
--     it "succeeds on this rappen example" $
--       fromAccount (QuantisationFactor 20) (Positive (Amount 1)) `shouldBe` Just (DecimalLiteral True 2 (scientific 5 (-2)))
--
--     it "produces valid literals" $
--       producesValid2 fromAccount
--
--     it "roundtrips with toAccount" $
--       forAllValid $ \acc ->
--         forAllValid $ \qf -> do
--           case fromAccount qf acc of
--             Nothing -> pure () -- Fine
--             Just dl ->
--               context (show dl) $
--                 case toAccount qf dl of
--                   Nothing -> expectationFailure "Should have been able to parse as an account"
--                   Just a -> a `shouldBe` acc
