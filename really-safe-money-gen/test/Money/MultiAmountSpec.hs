{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.MultiAmountSpec (spec) where

import Money.MultiAmount (MultiAmount (..))
import qualified Money.MultiAmount as MultiAmount
import Money.MultiAmount.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  showReadSpec @MultiAmount
  modifyMaxSuccess (* 10) $ do
    eqSpec @MultiAmount
    ordSpec @MultiAmount

    describe "add" $ do
      it "produces valid amounts" $
        producesValid2 MultiAmount.add

      it "has a left-identity: zero" $
        forAllValid $ \a ->
          MultiAmount.add MultiAmount.zero a `shouldBe` Just a

      it "has a right-identity: zero" $
        forAllValid $ \a ->
          MultiAmount.add a MultiAmount.zero `shouldBe` Just a

      it "is associative when both succeed" $
        forAllValid $ \a1 ->
          forAllValid $ \a2 ->
            forAllValid $ \a3 -> do
              let errOrL = MultiAmount.add <$> MultiAmount.add a1 a2 <*> pure a3
              let errOrR = MultiAmount.add <$> pure a1 <*> MultiAmount.add a2 a3
              case (,) <$> errOrL <*> errOrR of
                Nothing -> pure () -- Fine.
                Just (l, r) -> l `shouldBe` r

      it "is commutative" $
        forAllValid $ \a1 ->
          forAllValid $ \a2 ->
            MultiAmount.add a1 a2 `shouldBe` MultiAmount.add a2 a1
