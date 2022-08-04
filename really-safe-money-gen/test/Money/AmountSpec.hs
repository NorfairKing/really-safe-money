{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AmountSpec (spec) where

import Data.GenValidity.Vector ()
import Data.Ratio
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Real (Ratio ((:%)))
import Money.Amount (Amount (..))
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Numeric.Natural
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (* 100) . modifyMaxSize (* 3) $ do
  eqSpec @Amount
  ordSpec @Amount
  showReadSpec @Amount

  describe "fromMinimalQuantisations" $
    it "produces valid amounts" $
      producesValid Amount.fromMinimalQuantisations

  describe "toMinimalQuantisations" $ do
    it "produces valid Int64s" $
      producesValid Amount.toMinimalQuantisations

    it "roundtrips with fromMinimalQuantisations" $
      forAllValid $ \amount ->
        Amount.fromMinimalQuantisations (Amount.toMinimalQuantisations amount) `shouldBe` amount

  describe "fromRatio" $ do
    it "fails on NaN" $ do
      forAllValid $ \quantisationFactor ->
        let nan = 0 :% 0 :: Ratio Natural
         in Amount.fromRatio quantisationFactor nan `shouldBe` Nothing

    it "fails on +Infinity" $
      forAllValid $ \quantisationFactor ->
        let pinf = 1 :% 0 :: Ratio Natural
         in Amount.fromRatio quantisationFactor pinf `shouldBe` Nothing

    it "fails on 7.123 with quantisation factor 10" $
      Amount.fromRatio 10 7.123 `shouldBe` Nothing

    it "succeeds on 77.02 with quantisation factor 100" $
      Amount.fromRatio 100 77.02 `shouldBe` Just (Amount 7702)

    it "succeeds on 0" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRatio quantisationFactor 0.0 `shouldBe` Just (Amount 0)

    it "succeeds on 1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRatio quantisationFactor 1
          `shouldBe` Just (Amount (fromIntegral quantisationFactor))

    it "produces valid Amounts" $
      producesValid2 Amount.fromRatio

    it "roundtrips with toRatio" $
      forAllValid $ \quantisationFactor ->
        forAllValid $ \amount ->
          let r = Amount.toRatio quantisationFactor amount
           in context (show r) $ case Amount.fromRatio quantisationFactor r of
                Nothing -> pure () -- Fine
                Just amount' -> amount' `shouldBe` amount

  describe "toRatio" $ do
    it "produces valid Rationals when the quantisation factor is nonzero" $
      forAll (genValid `suchThat` (/= 0)) $ \quantisationFactor ->
        producesValid (Amount.toRatio quantisationFactor)

    it "produces an invalid Rational with quantisation factor 0" $
      forAllValid $ \a@(Amount m) ->
        Amount.toRatio 0 a `shouldBe` (fromIntegral m :% 0)

  describe "fromDouble" $ do
    it "succeeds on 77.02 with quantisation factor 100" $
      Amount.fromDouble 100 77.02 `shouldBe` Just (Amount 7702)

    it "fails on 7.123 with quantisation factor 10" $
      Amount.fromDouble 10 7.123 `shouldBe` Nothing

    it "succeeds on 0" $
      forAllValid $ \quantisationFactor ->
        Amount.fromDouble quantisationFactor 0.0 `shouldBe` Just (Amount 0)

    it "succeeds on 1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromDouble quantisationFactor 1
          `shouldBe` Just (Amount (fromIntegral quantisationFactor))

    it "fails on -1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromDouble quantisationFactor (-1)
          `shouldBe` Nothing

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

    it "produces valid amounts" $
      producesValid2 Amount.fromDouble

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

    it "succeeds on 7702 with quantisation factor 100" $
      Amount.toDouble 100 (Amount 7702) `shouldBe` 77.02

  describe "fromRational" $ do
    it "fails on NaN" $ do
      forAllValid $ \quantisationFactor ->
        let nan = 0 :% 0 :: Rational
         in Amount.fromRational quantisationFactor nan `shouldBe` Nothing

    it "fails on +Infinity" $
      forAllValid $ \quantisationFactor ->
        let pinf = 1 :% 0 :: Rational
         in Amount.fromRational quantisationFactor pinf `shouldBe` Nothing

    it "fails on -Infinity" $
      forAllValid $ \quantisationFactor ->
        let minf = -1 :% 0 :: Rational
         in Amount.fromRational quantisationFactor minf `shouldBe` Nothing

    it "fails on 7.123 with quantisation factor 10" $
      Amount.fromRational 10 7.123 `shouldBe` Nothing

    it "succeeds on 77.02 with quantisation factor 100" $
      Amount.fromRational 100 77.02 `shouldBe` Just (Amount 7702)

    it "succeeds on 0" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRational quantisationFactor 0.0 `shouldBe` Just (Amount 0)

    it "succeeds on 1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRational quantisationFactor 1
          `shouldBe` Just (Amount (fromIntegral quantisationFactor))

    it "fails on -1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRational quantisationFactor (-1)
          `shouldBe` Nothing

    it "produces valid Amounts" $
      producesValid2 Amount.fromRational

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

    it "produces an invalid Rational with quantisation factor 0" $
      forAllValid $ \a@(Amount m) ->
        Amount.toRational 0 a `shouldBe` (fromIntegral m :% 0)

  describe "zero" $
    it "is valid" $
      shouldBeValid Amount.zero

  describe "add" $ do
    it "fails for maxBound + 1" $
      Amount.add (Amount maxBound) (Amount 1)
        `shouldBe` Nothing

    it "fails for maxBound + maxBound" $
      Amount.add (Amount maxBound) (Amount maxBound)
        `shouldBe` Nothing

    it "produces valid amounts" $
      producesValid2 Amount.add

    it "has a left-identity: zero" $
      forAllValid $ \a ->
        Amount.add Amount.zero a `shouldBe` Just a

    it "has a right-identity: zero" $
      forAllValid $ \a ->
        Amount.add a Amount.zero `shouldBe` Just a

    it "is associative when both succeed" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          forAllValid $ \a3 -> do
            let errOrL = Amount.add <$> Amount.add a1 a2 <*> pure a3
            let errOrR = Amount.add <$> pure a1 <*> Amount.add a2 a3
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine.
              Just (l, r) -> l `shouldBe` r

    it "is commutative" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          Amount.add a1 a2 `shouldBe` Amount.add a2 a1

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAmount = Amount.add a1 a2
          case errOrAmount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger (Amount.toMinimalQuantisations a1)
                      + toInteger (Amount.toMinimalQuantisations a2)
              toInteger (Amount.toMinimalQuantisations amountResult)
                `shouldBe` integerResult

  describe "sum" $ do
    it "correctly sums [1,2,3] to 6" $
      Amount.sum [Amount 1, Amount 2, Amount 3] `shouldBe` Just (Amount 6)

    it "fails to sum above maxBound" $
      Amount.sum [Amount maxBound, Amount 1, Amount 2] `shouldBe` Nothing

    it "produces valid amounts" $
      producesValid (Amount.sum @Vector)

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \as -> do
        let errOrAmount = Amount.sum as
        case errOrAmount of
          Nothing -> pure () -- Fine.
          Just amountResult -> do
            let integerResult = sum $ V.map (toInteger . Amount.toMinimalQuantisations) as
            toInteger (Amount.toMinimalQuantisations amountResult)
              `shouldBe` integerResult

  describe "subtract" $ do
    it "fails for 0 - 1" $
      Amount.subtract (Amount 0) (Amount 1)
        `shouldBe` Nothing

    it "fails for 0 - maxBound" $
      Amount.subtract (Amount 0) (Amount maxBound)
        `shouldBe` Nothing

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAmount = Amount.subtract a1 a2
          case errOrAmount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger (Amount.toMinimalQuantisations a1)
                      - toInteger (Amount.toMinimalQuantisations a2)
              toInteger (Amount.toMinimalQuantisations amountResult)
                `shouldBe` integerResult

  describe "multiply" $ do
    it "produces valid amounts" $
      producesValid2 Amount.multiply

    it "has an identity: 1" $
      forAllValid $ \a ->
        Amount.multiply 1 a `shouldBe` Just a

    it "is absorbed by 0" $
      forAllValid $ \a ->
        Amount.multiply 0 a `shouldBe` Just Amount.zero

    -- A x (B + C) == A x B + A x C
    it "is distributive with add when both succeed" $
      forAllValid $ \a ->
        forAllValid $ \b ->
          forAllValid $ \c -> do
            let errOrL :: Maybe Amount
                errOrL = do
                  d <- Amount.add b c
                  Amount.multiply a d
            let errOrR :: Maybe Amount
                errOrR = do
                  d <- Amount.multiply a b
                  e <- Amount.multiply a c
                  Amount.add d e
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine
              Just (l, r) -> l `shouldBe` r

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \f ->
        forAllValid $ \a -> do
          let errOrAmount = Amount.multiply f a
          case errOrAmount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger f
                      * toInteger (Amount.toMinimalQuantisations a)
              toInteger (Amount.toMinimalQuantisations amountResult)
                `shouldBe` integerResult

  describe "divide" $ do
    it "produces valid amounts" $
      producesValid2 Amount.divide

    it "fails with a zero divisor" $
      forAllValid $ \a ->
        Amount.divide a 0 `shouldBe` Nothing

    it "succeeds when dividing by 1" $
      forAllValid $ \a ->
        Amount.divide a 1 `shouldBe` Just a

    it "Correctly divides 10 by 3" $
      Amount.divide (Amount 10) 3 `shouldBe` Just (Amount 3)

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a ->
        forAllValid $ \d -> do
          let errOrAmount = Amount.divide a d
          case errOrAmount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger (Amount.toMinimalQuantisations a)
                      `div` toInteger d
              toInteger (Amount.toMinimalQuantisations amountResult)
                `shouldBe` integerResult

  describe "distribute" $ do
    eqSpec @Amount.AmountDistribution
    showReadSpec @Amount.AmountDistribution

    it "correctly distributes 3 into 3" $
      Amount.distribute (Amount 3) 3 `shouldBe` Amount.DistributedIntoEqualChunks 3 (Amount 1)

    it "correctly distributes 5 into 3" $
      Amount.distribute (Amount 5) 3 `shouldBe` Amount.DistributedIntoUnequalChunks 2 (Amount 2) 1 (Amount 1)

    it "correctly distributes 10 into 4" $
      Amount.distribute (Amount 10) 4 `shouldBe` Amount.DistributedIntoUnequalChunks 2 (Amount 3) 2 (Amount 2)

    it "produces valid amounts" $
      producesValid2 Amount.distribute

    it "produces results that sum up to the greater whole" $
      forAllValid $ \f ->
        forAllValid $ \a ->
          let distribution = Amount.distribute a f
           in context (show distribution) $ case distribution of
                Amount.DistributedIntoZeroChunks -> f `shouldBe` 0
                Amount.DistributedZeroAmount -> a `shouldBe` Amount.zero
                Amount.DistributedIntoEqualChunks chunks chunkSize -> Amount.multiply chunks chunkSize `shouldBe` Just a
                Amount.DistributedIntoUnequalChunks
                  numberOfLargerChunks
                  largerChunk
                  numberOfSmallerChunks
                  smallerChunk -> do
                    context "chunksize" $
                      Amount.add smallerChunk (Amount 1) `shouldBe` Just largerChunk
                    let errOrLargerChunksAmount = Amount.multiply numberOfLargerChunks largerChunk
                    let errOrSmallerChunksAmount = Amount.multiply numberOfSmallerChunks smallerChunk
                    let errOrTotal = do
                          largerChunksAmount <- errOrLargerChunksAmount
                          smallerChunksAmount <- errOrSmallerChunksAmount
                          Amount.add largerChunksAmount smallerChunksAmount
                    let ctx =
                          unlines
                            [ unwords ["errOrLargerChunksAmount  ", show errOrLargerChunksAmount],
                              unwords ["errOrSmallerChunksAmount ", show errOrSmallerChunksAmount],
                              unwords ["errOrTotal               ", show errOrTotal]
                            ]
                    context ctx $ errOrTotal `shouldBe` Just a

  describe "fraction" $ do
    it "Correctly fractions 100 with 1 % 100" $
      Amount.fraction (Amount 100) (1 % 100)
        `shouldBe` (Amount 1, 1 % 100)

    it "Correctly fractions 101 with 1 % 100" $
      Amount.fraction (Amount 101) (1 % 100)
        `shouldBe` (Amount 1, 1 % 101)

    it "produces valid amounts" $
      producesValid2 Amount.fraction

    it "Produces a result that can be multiplied back" $
      forAllValid $ \a@(Amount amount) ->
        forAllValid $ \requestedFraction ->
          let result = Amount.fraction a requestedFraction
              (Amount fractionalAmount, actualFraction) = result
           in if actualFraction == 0
                then pure () -- Fine.
                else
                  context (show result) $
                    fromIntegral fractionalAmount / actualFraction
                      `shouldBe` fromIntegral amount
