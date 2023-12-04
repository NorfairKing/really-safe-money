{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AmountOfSpec (spec) where

import Data.GenValidity.Vector ()
import Data.Maybe
import Data.Proxy
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Real
import Money.Amount (Amount (..))
import Money.AmountOf (AmountOf (..), Distribution (..))
import qualified Money.AmountOf as AmountOf
import Money.AmountOf.Gen ()
import Money.Currency (IsCurrencyType (..))
import Money.Currency.TestUtils
import Money.QuantisationFactor
import Numeric.Natural
import Test.Syd
import Test.Syd.Validity
import Prelude hiding (subtract, sum)
import qualified Prelude

spec :: Spec
spec = forallCurrencies $ \p@(Proxy :: Proxy currency) -> do
  genValidSpec @(AmountOf currency)
  eqSpec @(AmountOf currency)
  ordSpec @(AmountOf currency)
  showReadSpec @(AmountOf currency)

  describe "fromMinimalQuantisations" $ do
    let fromMinimalQuantisations = AmountOf.fromMinimalQuantisations @currency

    it "produces valid amounts" $
      producesValid fromMinimalQuantisations

  describe "toMinimalQuantisations" $ do
    let toMinimalQuantisations = AmountOf.toMinimalQuantisations @currency

    it "produces valid Int64s" $
      producesValid toMinimalQuantisations

    it "roundtrips with fromMinimalQuantisations" $
      forAllValid $ \amount ->
        AmountOf.fromMinimalQuantisations (toMinimalQuantisations amount) `shouldBe` amount

  describe "fromRatio" $ do
    let from = AmountOf.fromRatio :: Ratio Natural -> Maybe (AmountOf currency)
    let to = AmountOf.toRatio :: AmountOf currency -> Ratio Natural
    it "fails on NaN" $ do
      let nan = 0 :% 0 :: Ratio Natural
       in from nan `shouldBe` Nothing

    it "fails on +Infinity" $
      let pinf = 1 :% 0 :: Ratio Natural
       in from pinf `shouldBe` Nothing

    it "fails on 7.123" $
      from 7.123456789 `shouldSatisfy` isNothing

    it "succeeds on 77" $
      from 77 `shouldSatisfy` isJust

    it "succeeds on 0" $
      from 0.0 `shouldBe` Just AmountOf.zero

    it "succeeds on 1" $
      from 1
        `shouldBe` Just (AmountOf (Amount (fromIntegral (unQuantisationFactor (quantisationFactor (Proxy @currency))))))

    it "produces valid AmountOfs" $
      producesValid from

    it "roundtrips with toRatio" $
      forAllValid $ \amount ->
        let r = to amount
         in context (show r) $ case from r of
              Nothing -> pure () -- Fine
              Just amount' -> amount' `shouldBe` amount

  describe "toRatio" $ do
    let to = AmountOf.toRatio :: AmountOf currency -> Ratio Natural

    it "produces valid Rationals when the quantisation factor is nonzero" $
      producesValid to

  describe "fromDouble" $ do
    let from = AmountOf.fromDouble :: Double -> Maybe (AmountOf currency)

    it "produces valid amounts" $
      producesValid from

    it "succeeds on 0" $
      from 0.0 `shouldBe` Just (AmountOf (Amount 0))

    it "succeeds on 1" $
      from 1 `shouldBe` Just (AmountOf (Amount (fromIntegral (unQuantisationFactor (quantisationFactor p)))))

    it "fails on -1" $
      from (-1) `shouldBe` Nothing

    it "fails on NaN" $
      let nan = read "NaN" :: Double
       in from nan `shouldBe` Nothing

    it "fails on +Infinity" $
      let pinf = read "Infinity"
       in from pinf `shouldBe` Nothing

    it "fails on -Infinity" $
      let minf = read "-Infinity"
       in from minf `shouldBe` Nothing

  describe "toDouble" $ do
    let to = AmountOf.toDouble :: AmountOf currency -> Double
    it "produces valid Doubles" $
      producesValid to

  describe "fromRational" $ do
    let from = AmountOf.fromRational :: Rational -> Maybe (AmountOf currency)

    it "produces valid Amounts" $
      producesValid from

    it "succeeds on 0" $
      from 0.0 `shouldBe` Just (AmountOf (Amount 0))

    it "succeeds on 1" $
      from 1 `shouldBe` Just (AmountOf (Amount (fromIntegral (unQuantisationFactor (quantisationFactor p)))))

    it "fails on -1" $
      from (-1) `shouldBe` Nothing

    it "fails on NaN" $
      let nan = 0 :% 0 :: Rational
       in from nan `shouldBe` Nothing

    it "fails on +Infinity" $
      let pinf = 1 :% 0 :: Rational
       in from pinf `shouldBe` Nothing

    it "fails on -Infinity" $
      let minf = -1 :% 0 :: Rational
       in from minf `shouldBe` Nothing

    it "roundtrips with toRational" $
      forAllValid $ \amount ->
        from (AmountOf.toRational amount)
          `shouldBe` Just (amount :: AmountOf currency)

  describe "toRational" $ do
    let to = AmountOf.toRational :: AmountOf currency -> Rational
    it "produces valid Rationals" $
      producesValid to

  let zero = AmountOf.zero @currency
  describe "zero" $
    it "is valid" $
      shouldBeValid zero

  describe "add" $ do
    let add = AmountOf.add @currency
    it "produces valid amounts" $
      producesValid2 add

    it "has a left-identity: zero" $
      forAllValid $ \a ->
        add AmountOf.zero a `shouldBe` Just a

    it "has a right-identity: zero" $
      forAllValid $ \a ->
        add a AmountOf.zero `shouldBe` Just a

    it "is associative when both succeed" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          forAllValid $ \a3 -> do
            let errOrL = add <$> add a1 a2 <*> pure a3
            let errOrR = add <$> pure a1 <*> add a2 a3
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine
              Just (l, r) -> l `shouldBe` r

    it "is commutative" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          add a1 a2 `shouldBe` add a2 a1

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAmount = add a1 a2
          case errOrAmount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger (AmountOf.toMinimalQuantisations a1)
                      + toInteger (AmountOf.toMinimalQuantisations a2)
              toInteger (AmountOf.toMinimalQuantisations amountResult) `shouldBe` integerResult

  describe "sum" $ do
    let sum = AmountOf.sum :: Vector (AmountOf currency) -> Maybe (AmountOf currency)
    it "produces valid amounts" $
      producesValid sum

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \as -> do
        let errOrAmount = sum (as :: Vector (AmountOf currency))
        case errOrAmount of
          Nothing -> pure () -- Fine.
          Just amountResult -> do
            let integerResult :: Integer
                integerResult = Prelude.sum $ V.map (toInteger . AmountOf.toMinimalQuantisations) as
            toInteger (AmountOf.toMinimalQuantisations amountResult)
              `shouldBe` integerResult

  describe "subtract" $ do
    let subtract = AmountOf.subtract @currency
    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAmount = subtract a1 a2
          case errOrAmount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger (AmountOf.toMinimalQuantisations a1)
                      - toInteger (AmountOf.toMinimalQuantisations a2)
              toInteger (AmountOf.toMinimalQuantisations amountResult) `shouldBe` integerResult

  describe "multiply" $ do
    let multiply = AmountOf.multiply @currency

    it "produces valid amounts" $
      producesValid2 multiply

    it "has an identity: 1" $
      forAllValid $ \a ->
        multiply 1 a `shouldBe` Just a

    it "is absorbed by 0" $
      forAllValid $ \a ->
        multiply 0 a `shouldBe` Just zero

    -- A x (B + C) == A x B + A x C
    it "is distributive with add when both succeed" $
      forAllValid $ \a ->
        forAllValid $ \b ->
          forAllValid $ \c -> do
            let errOrL :: Maybe (AmountOf currency)
                errOrL = do
                  d <- AmountOf.add b c
                  multiply a d
            let errOrR :: Maybe (AmountOf currency)
                errOrR = do
                  d <- multiply a b
                  e <- multiply a c
                  AmountOf.add d e
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine
              Just (l, r) -> l `shouldBe` r

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \f ->
        forAllValid $ \a -> do
          let errOrAmountOf = multiply f a
          case errOrAmountOf of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger f
                      * toInteger (AmountOf.toMinimalQuantisations a)
              toInteger (AmountOf.toMinimalQuantisations amountResult) `shouldBe` integerResult

  describe "distribute" $ do
    eqSpec @(AmountOf.AmountDistributionOf currency)
    showReadSpec @(AmountOf.AmountDistributionOf currency)

    let distribute = AmountOf.distribute @currency

    it "produces valid amounts" $
      producesValid2 distribute

    it "produces results that sum up to the greater whole" $
      forAllValid $ \f ->
        forAllValid $ \a ->
          let distribution = distribute a f
           in context (show distribution) $ case distribution of
                DistributedIntoZeroChunks -> f `shouldBe` 0
                DistributedZero -> a `shouldBe` zero
                DistributedIntoEqualChunks chunks chunkSize -> AmountOf.multiply chunks chunkSize `shouldBe` Just a
                DistributedIntoUnequalChunks
                  numberOfLargerChunks
                  largerChunk
                  numberOfSmallerChunks
                  smallerChunk -> do
                    let errOrLargerChunksAmount = AmountOf.multiply numberOfLargerChunks largerChunk
                    let errOrSmallerChunksAmount = AmountOf.multiply numberOfSmallerChunks smallerChunk
                    let errOrTotal = do
                          largerChunksAmount <- errOrLargerChunksAmount
                          smallerChunksAmount <- errOrSmallerChunksAmount
                          AmountOf.add largerChunksAmount smallerChunksAmount
                    let ctx =
                          unlines
                            [ unwords ["errOrLargerChunksAmount  ", show errOrLargerChunksAmount],
                              unwords ["errOrSmallerChunksAmount ", show errOrSmallerChunksAmount],
                              unwords ["errOrTotal               ", show errOrTotal]
                            ]
                    context ctx $ errOrTotal `shouldBe` Just a

  describe "fraction" $ do
    let fraction = AmountOf.fraction @currency
    it "produces valid amounts" $
      producesValid3 fraction

  describe "format" $ do
    let format = AmountOf.format @currency
    it "produces valid strings" $
      producesValid format
