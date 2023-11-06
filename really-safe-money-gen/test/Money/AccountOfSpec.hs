{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.AccountOfSpec (spec) where

import Data.GenValidity.Vector ()
import Data.Proxy
import Data.Vector (Vector)
import qualified Data.Vector as V
import Money.Account.Gen ()
import Money.AccountOf (AccountOf (..))
import qualified Money.AccountOf as AccountOf
import Money.AccountOf.Gen ()
import Money.Currency.TestUtils
import Test.Syd
import Test.Syd.Validity
import Prelude hiding (abs, fromRational, subtract, sum, toRational)
import qualified Prelude

spec :: Spec
spec = forallCurrencies $ \(Proxy :: Proxy currency) -> do
  eqSpec @(AccountOf currency)
  ordSpec @(AccountOf currency)
  showReadSpec @(AccountOf currency)

  let toMinimalQuantisations = AccountOf.toMinimalQuantisations @currency
  describe "toMinimalQuantisations" $
    it "produces valid accounts" $
      producesValid toMinimalQuantisations

  let fromMinimalQuantisations = AccountOf.fromMinimalQuantisations @currency
  describe "fromMinimalQuantisations" $ do
    it "produces valid accounts" $
      producesValid fromMinimalQuantisations

    it "roundtrips with toMinimalQuantisations" $
      forAllValid $ \account ->
        fromMinimalQuantisations (toMinimalQuantisations account) `shouldBe` Just account

  let toRational = AccountOf.toRational @currency
  describe "toRational" $ do
    it "produces valid Rationals when the quantisation factor is nonzero" $
      producesValid toRational

  let fromRational = AccountOf.fromRational @currency
  describe "fromRational" $ do
    it "produces valid rational" $
      producesValid fromRational

    it "roundtrips with toRational" $
      forAllValid $ \account ->
        case fromRational (toRational account) of
          Nothing -> pure () -- Fine
          Just account' -> account' `shouldBe` account

  let toDouble = AccountOf.toDouble @currency
  describe "toDouble" $ do
    it "produces valid Doubles when the quantisation factor is nonzero" $
      producesValid toDouble

  let fromDouble = AccountOf.fromDouble @currency
  describe "fromDouble" $ do
    it "produces valid rational" $
      producesValid fromDouble

    xdescribe "does not hold" $
      it "roundtrips with toDouble" $
        forAllValid $ \account ->
          case fromDouble (toDouble account) of
            Nothing -> pure () -- Fine
            Just account' -> account' `shouldBe` account

  let zero = AccountOf.zero @currency
  let add = AccountOf.add @currency
  describe "add" $ do
    it "produces valid amounts" $
      producesValid2 add

    it "has a left-identity: zero" $
      forAllValid $ \a ->
        add zero a `shouldBe` Just a

    it "has a right-identity: zero" $
      forAllValid $ \a ->
        add a zero `shouldBe` Just a

    it "is associative when both succeed" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          forAllValid $ \a3 -> do
            let errOrL = add <$> add a1 a2 <*> pure a3
            let errOrR = add <$> pure a1 <*> add a2 a3
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine.
              Just (l, r) -> l `shouldBe` r

    it "is commutative" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 ->
          add a1 a2 `shouldBe` add a2 a1

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAccountOf = add a1 a2
          case errOrAccountOf of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toMinimalQuantisations a1
                      + toMinimalQuantisations a2
              toMinimalQuantisations amountResult
                `shouldBe` integerResult

  let sum = AccountOf.sum @Vector @currency
  describe "sum" $ do
    it "produces valid amounts" $
      producesValid sum

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \as -> do
        let errOrAccount = sum as
        case errOrAccount of
          Nothing -> pure () -- Fine.
          Just amountResult -> do
            let integerResult = Prelude.sum $ V.map toMinimalQuantisations as
            toMinimalQuantisations amountResult
              `shouldBe` integerResult

  let subtract = AccountOf.subtract @currency
  describe "subtract" $ do
    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \a1 ->
        forAllValid $ \a2 -> do
          let errOrAccountOf = subtract a1 a2
          case errOrAccountOf of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    AccountOf.toMinimalQuantisations a1
                      - AccountOf.toMinimalQuantisations a2
              AccountOf.toMinimalQuantisations amountResult
                `shouldBe` integerResult

  let abs = AccountOf.abs @currency
  describe "abs" $ do
    it "produces valid amounts" $
      producesValid abs

  let multiply = AccountOf.multiply @currency
  describe "multiply" $ do
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
            let errOrL :: Maybe (AccountOf currency)
                errOrL = do
                  d <- add b c
                  multiply a d
            let errOrR :: Maybe (AccountOf currency)
                errOrR = do
                  d <- multiply a b
                  e <- multiply a c
                  add d e
            case (,) <$> errOrL <*> errOrR of
              Nothing -> pure () -- Fine
              Just (l, r) -> l `shouldBe` r

    it "matches what you would get with Integer, if nothing fails" $
      forAllValid $ \f ->
        forAllValid $ \a -> do
          let errOrAccountOf = multiply f a
          case errOrAccountOf of
            Nothing -> pure () -- Fine.
            Just amountResult -> do
              let integerResult =
                    toInteger f
                      * AccountOf.toMinimalQuantisations a
              AccountOf.toMinimalQuantisations amountResult
                `shouldBe` integerResult

  let distribute = AccountOf.distribute @currency
  describe "distribute" $ do
    eqSpec @(AccountOf.AccountDistributionOf currency)
    showReadSpec @(AccountOf.AccountDistributionOf currency)

    it "produces valid amounts" $
      producesValid2 distribute

    it "produces results that sum up to the greater whole" $
      forAllValid $ \a ->
        forAllValid $ \f ->
          let distribution = distribute a f
           in case distribution of
                AccountOf.DistributedIntoZeroChunks -> f `shouldBe` 0
                AccountOf.DistributedZeroAccount -> a `shouldBe` zero
                AccountOf.DistributedIntoEqualChunks chunks chunkSize -> multiply (fromIntegral chunks) chunkSize `shouldBe` Just a
                AccountOf.DistributedIntoUnequalChunks
                  numberOfLargerChunks
                  largerChunk
                  numberOfSmallerChunks
                  smallerChunk -> do
                    let errOrLargerChunksAccountOf = AccountOf.multiply (fromIntegral numberOfLargerChunks) largerChunk
                    let errOrSmallerChunksAccountOf = AccountOf.multiply (fromIntegral numberOfSmallerChunks) smallerChunk
                    let errOrTotal = do
                          largerChunksAccountOf <- errOrLargerChunksAccountOf
                          smallerChunksAccountOf <- errOrSmallerChunksAccountOf
                          AccountOf.add largerChunksAccountOf smallerChunksAccountOf
                    let ctx =
                          unlines
                            [ "distribution:",
                              ppShow distribution,
                              unwords ["errOrLargerChunksAccountOf  ", show errOrLargerChunksAccountOf],
                              unwords ["errOrSmallerChunksAccountOf ", show errOrSmallerChunksAccountOf],
                              unwords ["errOrTotal               ", show errOrTotal]
                            ]
                    context ctx $ errOrTotal `shouldBe` Just a

  let fraction = AccountOf.fraction @currency
  describe "fraction" $ do
    it "produces valid amounts" $
      producesValid3 fraction

    it "Produces a result that can be multiplied back" $
      forAllValid $ \rounding ->
        forAllValid $ \account ->
          forAllValid $ \requestedFraction ->
            let result = fraction rounding account requestedFraction
                (mFractionalAccountOf, actualFraction) = result
             in case mFractionalAccountOf of
                  Nothing -> pure () -- Fine.
                  Just fractionalAccountOf ->
                    if actualFraction == 0
                      then pure () -- Fine.
                      else
                        context (show result) $
                          fromIntegral (toMinimalQuantisations fractionalAccountOf) / actualFraction
                            `shouldBe` fromIntegral (toMinimalQuantisations account)
