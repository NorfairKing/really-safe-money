{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Money.AmountSpec (spec) where

import Control.Arrow (left)
import Data.Either
import Data.Ratio
import GHC.Real (Ratio ((:%)))
import Money.Amount (Amount (..))
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (* 100) . modifyMaxSize (* 10) $ do
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

    it "succeeds on 77.02 with quantisation factor 100" $
      Amount.fromDouble 100 77.02 `shouldBe` Just (Amount 7702)

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

    xdescribe "just does not hold because multiplying by the quantisation factor introduces floating point errors" $
      it "roundtrips with toDouble" $
        forAllValid $ \quantisationFactor ->
          forAllValid $ \amount ->
            let double = Amount.toDouble quantisationFactor amount
                result = Amount.fromDouble quantisationFactor double
                ctx = show double
             in context ctx $ case result of
                  Nothing -> pure () -- Fine
                  Just amount' -> amount' `shouldBe` amount

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

    it "succeeds on 77.02 with quantisation factor 100" $
      Amount.fromRational 100 77.02 `shouldBe` Just (Amount 7702)

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
          let r = Amount.toRational quantisationFactor amount
           in context (show r) $ case Amount.fromRational quantisationFactor r of
                Nothing -> pure () -- Fine
                Just amount' -> amount' `shouldBe` amount

  describe "toRational" $ do
    it "produces valid Rationals when the quantisation factor is nonzero" $
      forAll (genValid `suchThat` (/= 0)) $ \quantisationFactor ->
        producesValid (Amount.toRational quantisationFactor)

    it "produces 0 with quantisation factor 0" $
      forAllValid $ \a@(Amount m) ->
        Amount.toRational 0 a `shouldBe` (fromIntegral m :% 0)

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

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAmount = Amount.add a1 a2
          case errOrAmount of
            Left _ -> pure () -- Fine.
            Right (Amount amountResult) -> do
              let integerResult =
                    toInteger (Amount.toMinimalQuantisations a1)
                      + toInteger (Amount.toMinimalQuantisations a2)
              toInteger amountResult `shouldBe` integerResult

  describe "subtract" $ do
    it "fails for minBound - 1" $
      -- TODO specific failure
      Amount.subtract (Amount minBound) (Amount 1)
        `shouldSatisfy` isLeft

    xit "fails for minBound - minBound" $
      -- TODO specific failure
      Amount.subtract (Amount minBound) (Amount minBound)
        `shouldSatisfy` isLeft

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAmount = Amount.subtract a1 a2
          case errOrAmount of
            Left _ -> pure () -- Fine.
            Right (Amount amountResult) -> do
              let integerResult =
                    toInteger (Amount.toMinimalQuantisations a1)
                      - toInteger (Amount.toMinimalQuantisations a2)
              toInteger amountResult `shouldBe` integerResult

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
    it "is distributive with add when both succeed" $
      forAllValid $ \a ->
        forAllValid $ \b ->
          forAllValid $ \c -> do
            let errOrL :: Either (Either Amount.AdditionFailure Amount.MultiplicationFailure) Amount
                errOrL = do
                  d <- left Left (Amount.add b c)
                  left Right $ Amount.multiply a d
            let errOrR :: Either (Either Amount.AdditionFailure Amount.MultiplicationFailure) Amount
                errOrR = do
                  d <- left Right (Amount.multiply a b)
                  e <- left Right (Amount.multiply a c)
                  left Left $ Amount.add d e
            case (,) <$> errOrL <*> errOrR of
              Left _ -> pure () -- Fine
              Right (l, r) -> l `shouldBe` r

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \f ->
        forAllValid $ \a -> do
          let errOrAmount = Amount.multiply f a
          case errOrAmount of
            Left _ -> pure () -- Fine.
            Right (Amount amountResult) -> do
              let integerResult =
                    toInteger f
                      * toInteger (Amount.toMinimalQuantisations a)
              toInteger amountResult `shouldBe` integerResult

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

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a ->
        forAllValid $ \d -> do
          let errOrAmount = Amount.divide a d
          case errOrAmount of
            Left _ -> pure () -- Fine.
            Right (Amount amountResult) -> do
              let integerResult =
                    toInteger (Amount.toMinimalQuantisations a)
                      `div` toInteger d
              toInteger amountResult `shouldBe` integerResult

  describe "fraction" $ do
    it "produces valid amounts" $
      producesValid2 Amount.fraction

    it "Correctly fractions 100 with 1 % 100" $
      Amount.fraction (Amount 100) (1 % 100)
        `shouldBe` (Amount 1, 1 % 100)

    it "Correctly fractions 101 with 1 % 100" $
      Amount.fraction (Amount 101) (1 % 100)
        `shouldBe` (Amount 1, 1 % 101)
