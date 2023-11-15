{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.MultiAccountSpec (spec) where

import Money.Currency
import Money.MultiAccount (MultiAccount (..))
import qualified Money.MultiAccount as MultiAccount
import Money.MultiAccount.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  showReadSpec @(MultiAccount Currency)
  modifyMaxSuccess (* 10) $ do
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
