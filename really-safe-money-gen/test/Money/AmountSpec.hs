{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AmountSpec (spec) where

import Control.Arrow (left)
import Data.Either
import GHC.Real
import Money.Amount (Amount (..))
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "fromMinimalQuantisations" $
    it "produces valid amounts" $
      producesValid Amount.fromMinimalQuantisations

  describe "toMinimalQuantisations" $ do
    it "produces valid Int64s" $
      producesValid Amount.toMinimalQuantisations

    it "roundtrips with fromMinimalQuantisations" $
      forAllValid $ \amount ->
        Amount.fromMinimalQuantisations (Amount.toMinimalQuantisations amount) `shouldBe` amount

  describe "fromDouble" $ do
    it "produces valid amounts" $
      producesValid2 Amount.fromDouble

    it "succeeds on 0" $
      forAllValid $ \quantisationFactor ->
        Amount.fromDouble quantisationFactor 0.0 `shouldBe` Just (Amount 0)

    it "succeeds on 1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromDouble quantisationFactor 1
          `shouldBe` Just (Amount (fromIntegral quantisationFactor))

    it "succeeds on -1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromDouble quantisationFactor (-1)
          `shouldBe` Just (Amount (-(fromIntegral quantisationFactor)))

    it "fails on NaN" $
      forAllValid $ \quantisationFactor ->
        let nan = read "NaN" :: Double
         in Amount.fromDouble quantisationFactor nan `shouldBe` Nothing

    it "fails on +Infinity" $
      forAllValid $ \quantisationFactor ->
        let pinf = read "Infinity"
         in Amount.fromDouble quantisationFactor pinf `shouldBe` Nothing

    it "fails on -Infinity" $
      forAllValid $ \quantisationFactor ->
        let minf = read "-Infinity"
         in Amount.fromDouble quantisationFactor minf `shouldBe` Nothing

    it "roundtrips with toDouble" $
      forAllValid $ \quantisationFactor ->
        forAllValid $ \amount ->
          Amount.fromDouble
            quantisationFactor
            (Amount.toDouble quantisationFactor amount)
            `shouldBe` Just amount

  describe "toDouble" $ do
    it "produces valid Doubles" $
      producesValid2 Amount.toDouble

  describe "fromRational" $ do
    it "produces valid Amounts" $
      producesValid2 Amount.fromRational

    it "succeeds on 0" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRational quantisationFactor 0.0 `shouldBe` Just (Amount 0)

    it "succeeds on 1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRational quantisationFactor 1
          `shouldBe` Just (Amount (fromIntegral quantisationFactor))

    it "succeeds on -1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRational quantisationFactor (-1)
          `shouldBe` Just (Amount (-(fromIntegral quantisationFactor)))

    xit "fails on NaN" $ do
      forAllValid $ \quantisationFactor ->
        let nan = 0 :% 0 :: Rational
         in Amount.fromRational quantisationFactor nan `shouldBe` Nothing

    xit "fails on +Infinity" $
      forAllValid $ \quantisationFactor ->
        let pinf = 1 :% 0 :: Rational
         in Amount.fromRational quantisationFactor pinf `shouldBe` Nothing

    xit "fails on -Infinity" $
      forAllValid $ \quantisationFactor ->
        let minf = -1 :% 0 :: Rational
         in Amount.fromRational quantisationFactor minf `shouldBe` Nothing

    it "roundtrips with toRational" $
      forAllValid $ \quantisationFactor ->
        forAllValid $ \amount ->
          Amount.fromRational quantisationFactor (Amount.toRational quantisationFactor amount)
            `shouldBe` Just amount

  describe "toRational" $ do
    it "produces valid Rationals" $
      producesValid2 Amount.toRational

  describe "zero" $
    it "is valid" $
      shouldBeValid Amount.zero

  describe "add" $ do
    it "fails for maxBound + 1" $
      -- TODO specific failure
      Amount.add (Amount maxBound) (Amount 1)
        `shouldSatisfy` isLeft

    it "fails for maxBound + maxBound" $
      -- TODO specific failure
      Amount.add (Amount maxBound) (Amount maxBound)
        `shouldSatisfy` isLeft

    it "fails for minBound - 1" $
      -- TODO specific failure
      Amount.add (Amount maxBound) (Amount 1)
        `shouldSatisfy` isLeft

    -- TODO how to write this test? Maybe we need a 'subtract' as well.
    xit "fails for minBound - minBound" $
      -- TODO specific failure
      Amount.add (Amount minBound) (Amount $ -minBound)
        `shouldSatisfy` isLeft

    it "produces valid amounts" $
      producesValid2 Amount.add

    it "has a left-identity: zero" $
      forAllValid $ \a ->
        Amount.add Amount.zero a `shouldBe` Right a

    it "has a right-identity: zero" $
      forAllValid $ \a ->
        Amount.add a Amount.zero `shouldBe` Right a

    it "is associative when both succeed" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          forAllValid $ \a3 -> do
            let errOrL = Amount.add <$> Amount.add a1 a2 <*> pure a3
            let errOrR = Amount.add <$> pure a1 <*> Amount.add a2 a3
            case (,) <$> errOrL <*> errOrR of
              Left _ -> pure () -- Fine.
              Right (l, r) -> l `shouldBe` r

    it "is commutative" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          Amount.add a1 a2 `shouldBe` Amount.add a2 a1

  describe "multiply" $ do
    it "produces valid amounts" $
      producesValid2 Amount.multiply

    it "has an identity: 1" $
      forAllValid $ \a ->
        Amount.multiply 1 a `shouldBe` Right a

    it "is absorbed by 0" $
      forAllValid $ \a ->
        Amount.multiply 0 a `shouldBe` Right Amount.zero

    -- A x (B + C) == A x B + A x C
    it "is distributive with add" $
      forAllValid $ \a ->
        forAllValid $ \b ->
          forAllValid $ \c -> do
            let l :: Either (Either Amount.AdditionFailure Amount.MultiplicationFailure) Amount
                l = do
                  d <- left Left (Amount.add b c)
                  left Right $ Amount.multiply a d
            let r :: Either (Either Amount.AdditionFailure Amount.MultiplicationFailure) Amount
                r = do
                  d <- left Right (Amount.multiply a b)
                  e <- left Right (Amount.multiply a c)
                  left Left $ Amount.add d e
            l `shouldBe` r

  describe "divide" $ do
    it "produces valid amounts" $
      producesValid2 Amount.divide

    it "fails with a zero divisor" $
      forAllValid $ \a ->
        -- TODO specific failure
        Amount.divide a 0 `shouldSatisfy` isLeft

    it "succeeds when dividing by 1" $
      forAllValid $ \a ->
        Amount.divide a 1 `shouldBe` Right a

    it "Correctly divides 10 by 3" $
      Amount.divide (Amount 10) 3 `shouldBe` Right (Amount 3)

  describe "fraction" $ do
    it "produces valid amounts" $
      producesValid2 Amount.fraction

    it "Correctly fractions 100 with 1 % 100" $
      Amount.fraction (Amount 100) (1 % 100)
        `shouldBe` Right (Amount 1, 1 % 100)

    it "Correctly fractions 101 with 1 % 100" $
      Amount.fraction (Amount 101) (1 % 100)
        `shouldBe` Right (Amount 1, 1 % 101)
