{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.MultiAccountSpec (spec) where

import Data.GenValidity.Vector ()
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import Money.Account (Account (..), Rounding (..))
import Money.Amount (Amount (..))
import Money.ConversionRate (ConversionRate (..))
import qualified Money.ConversionRate as ConversionRate
import Money.ConversionRate.Gen ()
import Money.Currency
import Money.MultiAccount (MultiAccount (..), Rounded (..))
import qualified Money.MultiAccount as MultiAccount
import Money.MultiAccount.Gen ()
import Money.QuantisationFactor (QuantisationFactor (..))
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  showReadSpec @(MultiAccount Currency)
  modifyMaxSuccess (* 10) $ do
    genValidSpec @(MultiAccount Currency)
    eqSpec @(MultiAccount Currency)
    ordSpec @(MultiAccount Currency)

    describe "fromAccount" $ do
      it "produces valid amounts" $ do
        producesValid2 (MultiAccount.fromAccount @Currency)

    describe "zero" $ do
      it "is valid" $
        shouldBeValid (MultiAccount.zero @Currency)

    describe "add" $ do
      it "produces valid amounts" $
        producesValid2 (MultiAccount.add @Currency)

      it "has a left-identity: zero" $
        forAllValid $ \a ->
          MultiAccount.add @Currency MultiAccount.zero a `shouldBe` Just a

      it "has a right-identity: zero" $
        forAllValid $ \a ->
          MultiAccount.add @Currency a MultiAccount.zero `shouldBe` Just a

      it "is associative when both succeed" $
        forAllValid $ \a1 ->
          forAllValid $ \a2 ->
            forAllValid $ \a3 -> do
              let errOrL = MultiAccount.add @Currency <$> MultiAccount.add a1 a2 <*> pure a3
              let errOrR = MultiAccount.add @Currency <$> pure a1 <*> MultiAccount.add a2 a3
              case (,) <$> errOrL <*> errOrR of
                Nothing -> pure () -- Fine.
                Just (l, r) -> l `shouldBe` r

      it "is commutative" $
        forAllValid $ \a1 ->
          forAllValid $ \a2 ->
            MultiAccount.add @Currency a1 a2 `shouldBe` MultiAccount.add @Currency a2 a1

    describe "subtract" $ do
      it "produces valid amounts" $
        producesValid2 (MultiAccount.subtract @Currency)

      it "has a right-identity: zero" $
        forAllValid $ \a ->
          MultiAccount.subtract @Currency a MultiAccount.zero `shouldBe` Just a

    describe "sum" $ do
      it "produces valid amounts" $
        producesValid (MultiAccount.sum @Vector @Currency)

    describe "lookupAccount" $ do
      it "produces valid amounts" $
        producesValid2 (MultiAccount.lookupAccount @Currency)

      it "can find an added amount" $
        forAllValid $ \currency ->
          forAllValid $ \a ->
            (MultiAccount.lookupAccount @Currency currency <$> MultiAccount.addAccount @Currency MultiAccount.zero currency a) `shouldBe` Just a

    describe "addAmount" $ do
      it "produces valid amounts" $
        producesValid3 (MultiAccount.addAmount @Currency)

    describe "subtractAmount" $ do
      it "produces valid amounts" $
        producesValid3 (MultiAccount.subtractAmount @Currency)

    describe "addAccount" $ do
      it "produces valid amounts" $
        producesValid3 (MultiAccount.addAccount @Currency)

    describe "subtractAccount" $ do
      it "produces valid amounts" $
        producesValid3 (MultiAccount.subtractAccount @Currency)

    describe "convertAll" $ do
      it "produces the right result in this example" $ do
        forAllValid $ \rounding -> do
          let qfEur = QuantisationFactor 100
              eur = Currency "EUR" qfEur
          let qfChf = QuantisationFactor 20
              chf = Currency "CHF" qfChf
          let qfUsd = QuantisationFactor 100
              usd = Currency "USD" qfUsd
          let aEur = Positive (Amount 100)
          let aChf = Negative (Amount 20)
          let aUsd = Positive (Amount 100)
          let ma =
                MultiAccount $
                  M.fromList
                    [ (eur, aEur),
                      (usd, aUsd),
                      (chf, aChf)
                    ]
              func c = case currencySymbol c of
                "CHF" -> pure (ConversionRate 1, qfChf)
                "EUR" -> pure (ConversionRate 1.5, qfEur)
                "USD" -> pure (ConversionRate 2.0, qfUsd)
                _ -> expectationFailure "Should not happen."
          (mResult, _) <- MultiAccount.convertAllA rounding qfChf func ma
          mResult `shouldBe` Just (Positive (Amount 50))

      it "produces valid results when converting two currencies to one" $
        forAllValid $ \c1 ->
          forAllValid $ \c2 ->
            forAllValid $ \a1 ->
              forAllValid $ \a2 ->
                forAllValid $ \cr ->
                  forAllValid $ \rounding -> do
                    let (mResult, rounded) =
                          MultiAccount.convertAll
                            rounding
                            (currencyQuantisationFactor c1)
                            ( \c ->
                                if currencySymbol c == currencySymbol c1
                                  then (ConversionRate.oneToOne, currencyQuantisationFactor c1)
                                  else (cr, currencyQuantisationFactor c2)
                            )
                            (MultiAccount (M.fromList [(c1, a1), (c2, a2)]))
                    shouldBeValid mResult
                    rounded
                      `shouldSatisfy` ( \r -> case rounding of
                                          RoundDown -> r == RoundedDown || r == DidNotRound
                                          RoundNearest -> True
                                          RoundUp -> r == RoundedUp || r == DidNotRound
                                      )
