{-# LANGUAGE TypeApplications #-}

module Money.QuantisationFactorSpec (spec) where

import Data.GenValidity.Vector ()
import Money.QuantisationFactor
import qualified Money.QuantisationFactor as QuantisationFactor
import Money.QuantisationFactor.Gen ()
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
