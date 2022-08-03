{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AmountSpec (spec) where

import Control.Arrow (left)
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

    it "succeeds on 7702 with quantisation factor 100" $
      Amount.toDouble 100 (Amount 7702) `shouldBe` 77.02

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
        `shouldBe` Left (Amount.OverflowMaxbound 18446744073709551616)

    it "fails for maxBound + maxBound" $
      Amount.add (Amount maxBound) (Amount maxBound)
        `shouldBe` Left (Amount.OverflowMaxbound 36893488147419103230)

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
            Right amountResult -> do
              let integerResult =
                    toInteger (Amount.toMinimalQuantisations a1)
                      + toInteger (Amount.toMinimalQuantisations a2)
              toInteger (Amount.toMinimalQuantisations amountResult)
                `shouldBe` integerResult

  describe "subtract" $ do
    it "fails for 0 - 1" $
      Amount.subtract (Amount 0) (Amount 1)
        `shouldBe` Left (Amount.OverflowMinbound (-1))

    it "fails for 0 - maxBound" $
      Amount.subtract (Amount 0) (Amount maxBound)
        `shouldBe` Left (Amount.OverflowMinbound (-18446744073709551615))

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAmount = Amount.subtract a1 a2
          case errOrAmount of
            Left _ -> pure () -- Fine.
            Right amountResult -> do
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
            Right amountResult -> do
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
        Amount.divide a 0 `shouldBe` Left Amount.DivideByZero

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
            Right amountResult -> do
              let integerResult =
                    toInteger (Amount.toMinimalQuantisations a)
                      `div` toInteger d
              toInteger (Amount.toMinimalQuantisations amountResult)
                `shouldBe` integerResult

  describe "distribute" $ do
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
                Amount.DistributedIntoEqualChunks chunks chunkSize -> Amount.multiply (fromIntegral chunks) chunkSize `shouldBe` Right a
                Amount.DistributedIntoUnequalChunks
                  numberOfLargerChunks
                  largerChunk
                  numberOfSmallerChunks
                  smallerChunk -> do
                    context "chunksize" $
                      Amount.add smallerChunk (Amount 1) `shouldBe` Right largerChunk
                    let errOrLargerChunksAmount = Amount.multiply (fromIntegral numberOfLargerChunks) largerChunk
                    let errOrSmallerChunksAmount = Amount.multiply (fromIntegral numberOfSmallerChunks) smallerChunk
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
                    context ctx $ errOrTotal `shouldBe` Right a

  describe "fraction" $ do
    it "Correctly fractions 100 with 1 % 100" $
      Amount.fraction (Amount 100) (1 % 100)
        `shouldBe` (Amount 1, 1 % 100)

    it "Correctly fractions 101 with 1 % 100" $
      Amount.fraction (Amount 101) (1 % 100)
        `shouldBe` (Amount 1, 1 % 101)

    it "produces valid amounts" $
      producesValid2 Amount.fraction

-- it "Produces a result that can be multiplied back" $
--   forAll (genValid `suchThat` (/= 0)) $ \quantisationFactor ->
--     forAllValid $ \a ->
--       forAll (genValid `suchThat` (/= 0)) $ \requestedFraction ->
--         let result = Amount.fraction a requestedFraction
--             (fractionalAmount, actualFraction) = result
--          in context (show result) $
--               Amount.toRational quantisationFactor fractionalAmount / actualFraction
--                 `shouldBe` Amount.toRational quantisationFactor a
