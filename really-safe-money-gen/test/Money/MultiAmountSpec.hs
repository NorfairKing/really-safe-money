{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.MultiAmountSpec (spec) where

import Data.GenValidity.Vector ()
import qualified Data.Map as M
import Data.Vector (Vector)
import Money.Amount (Rounding (..))
import qualified Money.Amount as Amount
import Money.ConversionRate as ConversionRate
import Money.ConversionRate.Gen ()
import Money.Currency
import Money.MultiAmount (MultiAmount (..), Rounded (..))
import qualified Money.MultiAmount as MultiAmount
import Money.MultiAmount.Gen ()
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

    describe "sum" $ do
      it "produces valid amounts" $
        producesValid (MultiAmount.sum @Vector @Currency)

    describe "convertAll" $ do
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
                                if currencySymbol c == currencySymbol c1
                                  then (ConversionRate 1, currencyQuantisationFactor c1)
                                  else (cr, currencyQuantisationFactor c2)
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
                      (mA, _) = Amount.convert rounding qf1 a cr qf2
                   in mMA `shouldBe` mA
