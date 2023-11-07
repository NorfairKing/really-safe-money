{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AccountSpec (spec) where

import Data.GenValidity.Vector ()
import Data.Ratio
import Data.Vector (Vector)
import qualified Data.Vector as V
import Money.Account (Account (..), Distribution (..), Rounding (..))
import qualified Money.Account as Account
import Money.Account.Gen ()
import Money.Amount (Amount (..))
import qualified Money.Amount as Amount
import Test.QuickCheck hiding (Negative (..), Positive (..))
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (* 100) . modifyMaxSize (* 3) $ do
  describe "Eq" $ do
    it "says negative 0 equals positive 0" $
      Positive Amount.zero == Negative Amount.zero
    eqSpec @Account

  describe "Ord" $ do
    it "says 4 is smaller than 5" $
      Positive (Amount 4) < Positive (Amount 5)
    it "says -3 is smaller than 2" $
      Negative (Amount 3) < Positive (Amount 2)
    it "says -6 is greater than -7" $
      Negative (Amount 6) > Negative (Amount 7)
    ordSpec @Account

  showReadSpec @Account

  describe "toMinimalQuantisations" $
    it "produces valid accounts" $
      producesValid Account.toMinimalQuantisations

  describe "fromMinimalQuantisations" $ do
    it "produces valid accounts" $
      producesValid Account.fromMinimalQuantisations

    it "roundtrips with toMinimalQuantisations" $
      forAllValid $ \account ->
        Account.fromMinimalQuantisations (Account.toMinimalQuantisations account) `shouldBe` Just account

  describe "fromAmount" $ do
    it "produces valid accounts" $
      producesValid Account.fromAmount

  describe "toRational" $ do
    it "produces valid Rationals when the quantisation factor is nonzero" $
      forAll (genValid `suchThat` (/= 0)) $ \quantisationFactor ->
        producesValid (Account.toRational quantisationFactor)

    it "produces an invalid Rational with quantisation factor 0" $
      forAllValid $ \a ->
        shouldBeInvalid $ Account.toRational 0 a

  describe "fromRational" $ do
    it "produces valid rational" $
      producesValid2 Account.fromRational

    it "roundtrips with toRational" $
      forAllValid $ \quantisationFactor ->
        forAllValid $ \account ->
          case Account.fromRational quantisationFactor (Account.toRational quantisationFactor account) of
            Nothing -> pure () -- Fine
            Just account' -> account' `shouldBe` account

  describe "toDouble" $ do
    it "produces valid Doubles when the quantisation factor is nonzero" $
      forAll (genValid `suchThat` (/= 0)) $ \quantisationFactor ->
        producesValid (Account.toDouble quantisationFactor)

    it "produces an infinite or NaN Double with quantisation factor 0" $
      forAllValid $ \a ->
        Account.toDouble 0 a `shouldSatisfy` (\d -> isInfinite d || isNaN d)

  describe "fromDouble" $ do
    it "produces valid rational" $
      producesValid2 Account.fromDouble

    xdescribe "does not hold" $
      it "roundtrips with toDouble" $
        forAllValid $ \quantisationFactor ->
          forAllValid $ \account ->
            case Account.fromDouble quantisationFactor (Account.toDouble quantisationFactor account) of
              Nothing -> pure () -- Fine
              Just account' -> account' `shouldBe` account

  describe "add" $ do
    it "fails for maxBound + 1" $
      Account.add (Positive (Amount maxBound)) (Positive (Amount 1))
        `shouldBe` Nothing

    it "fails for maxBound + maxBound" $
      Account.add (Positive (Amount maxBound)) (Positive (Amount maxBound))
        `shouldBe` Nothing

    it "fails for minBound + (-1)" $
      Account.add (Negative (Amount maxBound)) (Negative (Amount 1))
        `shouldBe` Nothing

    it "fails for minBound + minBound" $
      Account.add (Negative (Amount maxBound)) (Negative (Amount maxBound))
        `shouldBe` Nothing

    it "produces valid amounts" $
      producesValid2 Account.add

    it "has a left-identity: zero" $
      forAllValid $ \a ->
        Account.add Account.zero a `shouldBe` Just a

    it "has a right-identity: zero" $
      forAllValid $ \a ->
        Account.add a Account.zero `shouldBe` Just a

    it "is associative when both succeed" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          forAllValid $ \a3 -> do
            let errOrL = Account.add <$> Account.add a1 a2 <*> pure a3
            let errOrR = Account.add <$> pure a1 <*> Account.add a2 a3
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine.
              Just (l, r) -> l `shouldBe` r

    it "is commutative" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          Account.add a1 a2 `shouldBe` Account.add a2 a1

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAccount = Account.add a1 a2
          case errOrAccount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    Account.toMinimalQuantisations a1
                      + Account.toMinimalQuantisations a2
              Account.toMinimalQuantisations amountResult
                `shouldBe` integerResult

  describe "sum" $ do
    it "correctly sums [1,2,3] to 6" $
      Account.sum [Positive (Amount 1), Positive (Amount 2), Positive (Amount 3)] `shouldBe` Just (Positive (Amount 6))

    it "fails to sum above maxBound" $
      Account.sum [Positive (Amount maxBound), Positive (Amount 1), Positive (Amount 2)] `shouldBe` Nothing

    it "fails to sum above maxBound even if the result is back below maxBound" $
      Account.sum [Positive (Amount maxBound), Positive (Amount 1), Positive (Amount 2), Negative (Amount 3)] `shouldBe` Nothing

    it "produces valid amounts" $
      producesValid (Account.sum @Vector)

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \as -> do
        let errOrAccount = Account.sum as
        case errOrAccount of
          Nothing -> pure () -- Fine.
          Just amountResult -> do
            let integerResult = sum $ V.map Account.toMinimalQuantisations as
            Account.toMinimalQuantisations amountResult
              `shouldBe` integerResult

  describe "subtract" $ do
    it "fails for minBound - 1" $
      Account.subtract (Negative (Amount maxBound)) (Positive (Amount 1))
        `shouldBe` Nothing

    it "fails for maxBound - minBound" $
      Account.subtract (Positive (Amount maxBound)) (Negative (Amount maxBound))
        `shouldBe` Nothing

    it "fails for minBound - maxBound" $
      Account.subtract (Negative (Amount maxBound)) (Positive (Amount maxBound))
        `shouldBe` Nothing

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAccount = Account.subtract a1 a2
          case errOrAccount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    Account.toMinimalQuantisations a1
                      - Account.toMinimalQuantisations a2
              Account.toMinimalQuantisations amountResult
                `shouldBe` integerResult

  describe "abs" $ do
    it "produces valid amounts" $
      producesValid Account.abs

  describe "multiply" $ do
    it "succeeds for 3 * 6" $
      Account.multiply 3 (Positive (Amount 6)) `shouldBe` Just (Positive (Amount 18))

    it "fails for 2 * maxbound" $
      Account.multiply 2 (Positive (Amount maxBound)) `shouldBe` Nothing

    it "fails for 3 * minBound" $
      Account.multiply 3 (Negative (Amount maxBound)) `shouldBe` Nothing

    it "produces valid amounts" $
      producesValid2 Account.multiply

    it "has an identity: 1" $
      forAllValid $ \a ->
        Account.multiply 1 a `shouldBe` Just a

    it "is absorbed by 0" $
      forAllValid $ \a ->
        Account.multiply 0 a `shouldBe` Just Account.zero

    -- A x (B + C) == A x B + A x C
    it "is distributive with add when both succeed" $
      forAllValid $ \a ->
        forAllValid $ \b ->
          forAllValid $ \c -> do
            let errOrL :: Maybe Account
                errOrL = do
                  d <- Account.add b c
                  Account.multiply a d
            let errOrR :: Maybe Account
                errOrR = do
                  d <- Account.multiply a b
                  e <- Account.multiply a c
                  Account.add d e
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine
              Just (l, r) -> l `shouldBe` r

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \f ->
        forAllValid $ \a -> do
          let errOrAccount = Account.multiply f a
          case errOrAccount of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger f
                      * Account.toMinimalQuantisations a
              Account.toMinimalQuantisations amountResult
                `shouldBe` integerResult

  describe "distribute" $ do
    eqSpec @Account.AccountDistribution
    showReadSpec @Account.AccountDistribution

    it "correctly distributes 3 into 3" $
      Account.distribute (Positive (Amount 3)) 3 `shouldBe` Account.DistributedIntoEqualChunks 3 (Positive (Amount 1))

    it "correctly distributes 5 into 3" $
      Account.distribute (Positive (Amount 5)) 3 `shouldBe` Account.DistributedIntoUnequalChunks 2 (Positive (Amount 2)) 1 (Positive (Amount 1))

    it "correctly distributes 10 into 4" $
      Account.distribute (Positive (Amount 10)) 4 `shouldBe` Account.DistributedIntoUnequalChunks 2 (Positive (Amount 3)) 2 (Positive (Amount 2))

    it "produces valid amounts" $
      producesValid2 Account.distribute

    it "produces results that sum up to the greater whole" $
      forAllValid $ \a ->
        forAllValid $ \f ->
          let distribution = Account.distribute a f
           in case distribution of
                DistributedIntoZeroChunks -> f `shouldBe` 0
                DistributedZero -> a `shouldBe` Account.zero
                DistributedIntoEqualChunks chunks chunkSize -> Account.multiply (fromIntegral chunks) chunkSize `shouldBe` Just a
                DistributedIntoUnequalChunks
                  numberOfLargerChunks
                  largerChunk
                  numberOfSmallerChunks
                  smallerChunk -> do
                    let errOrLargerChunksAccount = Account.multiply (fromIntegral numberOfLargerChunks) largerChunk
                    let errOrSmallerChunksAccount = Account.multiply (fromIntegral numberOfSmallerChunks) smallerChunk
                    let errOrTotal = do
                          largerChunksAccount <- errOrLargerChunksAccount
                          smallerChunksAccount <- errOrSmallerChunksAccount
                          Account.add largerChunksAccount smallerChunksAccount
                    let ctx =
                          unlines
                            [ "distribution:",
                              ppShow distribution,
                              unwords ["errOrLargerChunksAccount  ", show errOrLargerChunksAccount],
                              unwords ["errOrSmallerChunksAccount ", show errOrSmallerChunksAccount],
                              unwords ["errOrTotal               ", show errOrTotal]
                            ]
                    context ctx $ errOrTotal `shouldBe` Just a

  describe "fraction" $ do
    it "Correctly fractions 100 with 1 % 100" $
      Account.fraction RoundNearest (Positive (Amount 100)) (1 % 100)
        `shouldBe` (Just (Positive (Amount 1)), 1 % 100)

    it "Correctly fractions 101 with 1 % 100" $
      Account.fraction RoundNearest (Positive (Amount 101)) (1 % 100)
        `shouldBe` (Just (Positive (Amount 1)), 1 % 101)

    it "produces valid amounts" $
      producesValid3 Account.fraction

    it "Produces a result that can be multiplied back" $
      forAllValid $ \rounding ->
        forAllValid $ \account ->
          forAllValid $ \requestedFraction ->
            let result = Account.fraction rounding account requestedFraction
                (mFractionalAmount, actualFraction) = result
             in case mFractionalAmount of
                  Nothing -> pure () -- Fine.
                  Just fractionalAccount ->
                    if actualFraction == 0
                      then pure () -- Fine.
                      else
                        context (show result) $
                          fromIntegral (Account.toMinimalQuantisations fractionalAccount) / actualFraction
                            `shouldBe` fromIntegral (Account.toMinimalQuantisations account)
