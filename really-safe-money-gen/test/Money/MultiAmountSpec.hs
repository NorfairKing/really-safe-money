{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.MultiAmountSpec (spec) where

import Data.GenValidity.Vector ()
import Data.Vector (Vector)
import Money.Currency
import Money.MultiAmount (MultiAmount (..))
import qualified Money.MultiAmount as MultiAmount
import Money.MultiAmount.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  showReadSpec @(MultiAmount Currency)
  modifyMaxSuccess (* 10) $ do
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
