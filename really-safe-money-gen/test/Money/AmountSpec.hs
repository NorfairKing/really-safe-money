{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AmountSpec (spec) where

import Data.GenValidity.Vector ()
import Data.Ratio
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Real (Ratio ((:%)))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Money.Amount (Amount (..), Distribution (..), Rounding (..))
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Money.ConversionRate (ConversionRate (..))
import Money.ConversionRate.Gen ()
import Money.QuantisationFactor
import Money.QuantisationFactor.Gen ()
import Numeric.DecimalLiteral (DecimalLiteral (..))
import Numeric.DecimalLiteral.Gen ()
import Numeric.Natural
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (* 100) . modifyMaxSize (* 3) $ do
  genValidSpec @Amount
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
      Amount.fromRatio (QuantisationFactor 10) 7.123 `shouldBe` Nothing

    it "succeeds on 77.02 with quantisation factor 100" $
      Amount.fromRatio (QuantisationFactor 100) 77.02 `shouldBe` Just (Amount 7_702)

    it "succeeds on 0" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRatio quantisationFactor 0.0 `shouldBe` Just (Amount 0)

    it "succeeds on 1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRatio quantisationFactor 1
          `shouldBe` Just (Amount (fromIntegral (unQuantisationFactor quantisationFactor)))

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
      forAllValid $ \quantisationFactor ->
        producesValid (Amount.toRatio quantisationFactor)

    it "produces an invalid Rational with quantisation factor 0" $
      forAllValid $ \a@(Amount m) ->
        Amount.toRatio (QuantisationFactor 0) a `shouldBe` (fromIntegral m :% 0)

  describe "fromDouble" $ do
    it "succeeds on 77.02 with quantisation factor 100" $
      Amount.fromDouble (QuantisationFactor 100) 77.02 `shouldBe` Just (Amount 7_702)

    it "fails on 7.123 with quantisation factor 10" $
      Amount.fromDouble (QuantisationFactor 10) 7.123 `shouldBe` Nothing

    it "succeeds on 0" $
      forAllValid $ \quantisationFactor ->
        Amount.fromDouble quantisationFactor 0.0 `shouldBe` Just (Amount 0)

    it "succeeds on 1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromDouble quantisationFactor 1
          `shouldBe` Just (Amount (fromIntegral (unQuantisationFactor quantisationFactor)))

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

    it "fails on really large numbers" $
      Amount.fromDouble (QuantisationFactor 100_000_000) 2E60 `shouldBe` Nothing

    it "fails on really large numbers" $
      Amount.fromDouble (QuantisationFactor 100_000_000) 2E1_024 `shouldBe` Nothing

    it "produces valid amounts" $
      producesValid2 Amount.fromDouble

    it "roundtrips with toDouble, back to double" $
      forAllValid $ \quantisationFactor ->
        forAllValid $ \amount ->
          let double = Amount.toDouble quantisationFactor amount
              result = Amount.fromDouble quantisationFactor double
              ctx = show double
           in context ctx $ case result of
                Nothing -> pure () -- Fine
                Just amount' -> Amount.toDouble quantisationFactor amount' `shouldBe` Amount.toDouble quantisationFactor amount

  describe "toDouble" $ do
    it "produces valid Doubles" $
      producesValid2 Amount.toDouble

    it "succeeds on 7702 with quantisation factor 100" $
      Amount.toDouble (QuantisationFactor 100) (Amount 7_702) `shouldBe` 77.02

    it "produces an infinite Double with quantisation factor 0" $
      forAllValid $ \a ->
        Amount.toDouble (QuantisationFactor 0) a `shouldSatisfy` (\d -> isInfinite d || isNaN d)

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
      Amount.fromRational (QuantisationFactor 10) 7.123 `shouldBe` Nothing

    it "succeeds on 77.02 with quantisation factor 100" $
      Amount.fromRational (QuantisationFactor 100) 77.02 `shouldBe` Just (Amount 7_702)

    it "succeeds on 0" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRational quantisationFactor 0.0 `shouldBe` Just (Amount 0)

    it "succeeds on 1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRational quantisationFactor 1
          `shouldBe` Just (Amount (fromIntegral (unQuantisationFactor quantisationFactor)))

    it "fails on -1" $
      forAllValid $ \quantisationFactor ->
        Amount.fromRational quantisationFactor (-1)
          `shouldBe` Nothing

    it "fails on really large numbers" $
      Amount.fromRational (QuantisationFactor 100_000_000) 2E60 `shouldBe` Nothing

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
      forAllValid $ \quantisationFactor ->
        producesValid (Amount.toRational quantisationFactor)

    it "produces an invalid Rational with quantisation factor 0" $
      forAllValid $ \a@(Amount m) ->
        Amount.toRational (QuantisationFactor 0) a `shouldBe` (fromIntegral m :% 0)

  describe "DecimalLiteral" $ do
    decimalLiteralExampleSpec (QuantisationFactor 100) (DecimalLiteral (Just True) 100 2) (Amount 100)
    decimalLiteralExampleSpec (QuantisationFactor 100) (DecimalLiteral (Just True) 200 2) (Amount 200)
    decimalLiteralExampleSpec (QuantisationFactor 100) (DecimalLiteral (Just True) 3 2) (Amount 3)
    decimalLiteralExampleSpec (QuantisationFactor 100) (DecimalLiteral (Just True) 4 2) (Amount 4)
    decimalLiteralExampleSpec (QuantisationFactor 20) (DecimalLiteral (Just True) 500 2) (Amount 100)
    decimalLiteralExampleSpec (QuantisationFactor 20) (DecimalLiteral (Just True) 600 2) (Amount 120)
    decimalLiteralExampleSpec (QuantisationFactor 20) (DecimalLiteral (Just True) 10 2) (Amount 2)
    decimalLiteralExampleSpec (QuantisationFactor 20) (DecimalLiteral (Just True) 20 2) (Amount 4)
    decimalLiteralExampleSpec (QuantisationFactor 1) (DecimalLiteral (Just True) 1 0) (Amount 1)
    decimalLiteralExampleSpec (QuantisationFactor 1) (DecimalLiteral (Just True) 2 0) (Amount 2)
    decimalLiteralExampleSpec (QuantisationFactor 100_000_000) (DecimalLiteral (Just True) 500 8) (Amount 500)

    describe "Amount.fromDecimalLiteral" $ do
      it "produces valid factors" $
        producesValid2 Amount.fromDecimalLiteral

      it "fails on this amount that is too precise" $
        Amount.fromDecimalLiteral (QuantisationFactor 100) (DecimalLiteral (Just True) 1 4) `shouldBe` Nothing
      it "fails on this amount that is too precise" $
        Amount.fromDecimalLiteral (QuantisationFactor 20) (DecimalLiteral (Just True) 1 3) `shouldBe` Nothing

    describe "Amount.toDecimalLiteral" $ do
      it "produces valid decimal literals" $
        producesValid2 Amount.toDecimalLiteral

      it "roundtrips with Amount.fromDecimalLiteral" $
        forAllValid $ \acc ->
          forAllValid $ \qf -> do
            case Amount.toDecimalLiteral qf acc of
              Nothing -> pure () -- Fine
              Just dl ->
                context (show dl) $
                  case Amount.fromDecimalLiteral qf dl of
                    Nothing -> expectationFailure "Should have been able to parse as an account"
                    Just a -> a `shouldBe` acc

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
    it "succeeds for 3 * 6" $
      Amount.multiply 3 (Amount 6) `shouldBe` Just (Amount 18)

    it "fails for 2 * maxbound" $
      Amount.multiply 2 (Amount maxBound) `shouldBe` Nothing

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

  describe "distribute" $ do
    genValidSpec @Amount.AmountDistribution
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
                DistributedIntoZeroChunks -> f `shouldBe` 0
                DistributedZero -> a `shouldBe` Amount.zero
                DistributedIntoEqualChunks chunks chunkSize -> Amount.multiply chunks chunkSize `shouldBe` Just a
                DistributedIntoUnequalChunks
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
      Amount.fraction RoundNearest (Amount 100) (1 % 100)
        `shouldBe` (Just (Amount 1), 1 % 100)

    it "Correctly fractions 101 with 1 % 100" $
      Amount.fraction RoundNearest (Amount 101) (1 % 100)
        `shouldBe` (Just (Amount 1), 1 % 101)

    it "produces valid amounts" $
      producesValid3 Amount.fraction

    it "Produces a result that can be multiplied back" $
      forAllValid $ \rounding ->
        forAllValid $ \a ->
          forAllValid $ \requestedFraction ->
            let result = Amount.fraction rounding a requestedFraction
                (mFractionalAmount, actualFraction) = result
             in case mFractionalAmount of
                  Nothing -> pure () -- Fine.
                  Just (Amount fractionalAmount) ->
                    if actualFraction == 0
                      then pure () -- Fine.
                      else
                        context (show result) $
                          fromIntegral fractionalAmount / actualFraction
                            `shouldBe` fromIntegral (Amount.toMinimalQuantisations a)

    it "Produces a result that has been rounded in the right direction when using RoundDown" $
      forAllValid $ \a ->
        forAllValid $ \requestedFraction ->
          let (_, actualFraction) = Amount.fraction RoundDown a requestedFraction
           in actualFraction <= requestedFraction

    it "Produces a result that has been rounded in the right direction when using RoundUp" $
      forAllValid $ \a ->
        forAllValid $ \requestedFraction ->
          let (_, actualFraction) = Amount.fraction RoundUp a requestedFraction
           in actualFraction >= requestedFraction

  describe "rate" $ do
    it "computes this USD to CHF rate correctly" $
      Amount.rate (QuantisationFactor 100) (Amount 100) (QuantisationFactor 20) (Amount 22)
        `shouldBe` Just (ConversionRate (11 % 10))

    it "produces valid conversion rates" $
      forAllValid $ \qf1 ->
        forAllValid $ \a1 ->
          forAllValid $ \qf2 ->
            forAllValid $ \a2 ->
              shouldBeValid $ Amount.rate qf1 a1 qf2 a2

  describe "convert" $ do
    it "converts this USD to CHF correctly" $
      let cr = ConversionRate (110 % 100)
       in Amount.convert
            RoundNearest
            (QuantisationFactor 100)
            (Amount 100)
            cr
            (QuantisationFactor 20)
            `shouldBe` (Just (Amount 22), Just cr)

    -- TODO
    xit "succeeds in converting 1:1 without rounding" $
      forAllValid $ \r ->
        forAllValid $ \qf1 ->
          forAllValid $ \a ->
            forAllValid $ \qf2 ->
              let cr = ConversionRate 1
               in Amount.convert r qf1 a cr qf2 `shouldBe` (Just a, Just cr)

    it "produces valid amounts" $
      forAllValid $ \r ->
        forAllValid $ \qf1 ->
          forAllValid $ \a ->
            forAllValid $ \cr ->
              forAllValid $ \qf2 ->
                shouldBeValid $ Amount.convert r qf1 a cr qf2

  describe "format" $ do
    it "formats 1 correctly with quantisation factor 1" $
      Amount.format (QuantisationFactor 1) (Amount 1) `shouldBe` "1"

    it "formats 1 correctly with quantisation factor 10" $
      Amount.format (QuantisationFactor 10) (Amount 1) `shouldBe` "0.1"

    it "produces valid strings" $
      producesValid2 Amount.format

decimalLiteralExampleSpec :: HasCallStack => QuantisationFactor -> DecimalLiteral -> Amount -> Spec
decimalLiteralExampleSpec qf dl a =
  withFrozenCallStack $ do
    decimalLiteralParseExampleSpec qf dl a
    decimalLiteralRenderExampleSpec qf dl a

decimalLiteralRenderExampleSpec :: HasCallStack => QuantisationFactor -> DecimalLiteral -> Amount -> Spec
decimalLiteralRenderExampleSpec qf dl a =
  withFrozenCallStack $
    it (unwords ["can turn decimalLiteral", show qf, "into", show dl]) $
      Amount.toDecimalLiteral qf a `shouldBe` Just dl

decimalLiteralParseExampleSpec :: HasCallStack => QuantisationFactor -> DecimalLiteral -> Amount -> Spec
decimalLiteralParseExampleSpec qf dl a =
  withFrozenCallStack $
    it (unwords ["can turn", show dl, "into decimalLiteral", show qf]) $
      Amount.fromDecimalLiteral qf dl `shouldBe` Just a
