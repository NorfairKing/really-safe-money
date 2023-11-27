{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.CodecSpec (spec) where

import Autodocodec
import Autodocodec.Yaml
import Control.DeepSeq
import Control.Exception
import Data.Aeson.Types as JSON
import Data.Typeable
import GHC.Stack
import Money.Account (Account (..))
import Money.Account.Codec as Account
import Money.Account.Gen ()
import Money.AccountOf (AccountOf (..))
import Money.AccountOf.Codec as AccountOf
import Money.AccountOf.Gen ()
import Money.Amount (Amount (..))
import Money.Amount.Codec as Amount
import Money.Amount.Gen ()
import Money.AmountOf (AmountOf (..))
import Money.AmountOf.Codec as AmountOf
import Money.AmountOf.Gen ()
import Money.Currency (IsCurrencyType (..))
import Money.QuantisationFactor (QuantisationFactor (..))
import Money.QuantisationFactor.Codec as QuantisationFactor
import Money.QuantisationFactor.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "QuantisationFactor" $ do
    codecSpec @QuantisationFactor "quantisation-factor" "number" QuantisationFactor.codecViaNumber
    parseFailSpec QuantisationFactor.codecViaNumber (Number 0)
    parseSuccessSpec QuantisationFactor.codecViaNumber (Number 1) (QuantisationFactor 1)

  -- 2^64 is 18446744073709551616
  describe "Amount" $ do
    codecSpec @Amount "amount" "string" Amount.codecViaString
    parseFailSpec Amount.codecViaString (String "0.1")
    parseFailSpec Amount.codecViaString (String "-1")
    parseFailSpec Amount.codecViaString (String "18446744073709551617")
    parseSuccessSpec Amount.codecViaString (String "1") (Amount 1)
    parseSuccessSpec Amount.codecViaString (String "18446744073709551615") (Amount 18446744073709551615)

    codecSpec @Amount "amount" "number" Amount.codecViaNumber
    parseFailSpec Amount.codecViaNumber (Number 0.1)
    parseFailSpec Amount.codecViaNumber (Number (-1))
    parseFailSpec Amount.codecViaNumber (Number 18446744073709551617)
    parseSuccessSpec Amount.codecViaNumber (Number 1) (Amount 1)
    parseSuccessSpec Amount.codecViaNumber (Number 18446744073709551615) (Amount 18446744073709551615)

  describe "AmountOf" $ do
    codecSpec @(AmountOf USD) "amount-of" "string" AmountOf.codecViaString
    parseFailSpec AmountOf.codecViaString (String "0.1")
    parseFailSpec AmountOf.codecViaString (String "-1")
    parseFailSpec AmountOf.codecViaString (String "18446744073709551617")
    parseSuccessSpec AmountOf.codecViaString (String "1") (AmountOf (Amount 1))
    parseSuccessSpec AmountOf.codecViaString (String "18446744073709551615") (AmountOf (Amount 18446744073709551615))

    codecSpec @(AmountOf USD) "amount-of" "number" AmountOf.codecViaNumber
    parseFailSpec AmountOf.codecViaNumber (Number 0.1)
    parseFailSpec AmountOf.codecViaNumber (Number (-1))
    parseFailSpec AmountOf.codecViaNumber (Number 18446744073709551617)
    parseSuccessSpec AmountOf.codecViaNumber (Number 1) (AmountOf (Amount 1))
    parseSuccessSpec AmountOf.codecViaNumber (Number 18446744073709551615) (AmountOf (Amount 18446744073709551615))

  describe "Account" $ do
    codecSpec @Account "account" "string" Account.codecViaString
    parseFailSpec Account.codecViaString (String "0.1")
    parseFailSpec Account.codecViaString (String "18446744073709551617")
    parseFailSpec Account.codecViaString (String "-18446744073709551617")
    parseSuccessSpec Account.codecViaString (String "-18446744073709551615") (Negative (Amount 18446744073709551615))
    parseSuccessSpec Account.codecViaString (String "-1") (Negative (Amount 1))
    parseSuccessSpec Account.codecViaString (String "1") (Positive (Amount 1))
    parseSuccessSpec Account.codecViaString (String "18446744073709551615") (Positive (Amount 18446744073709551615))

    codecSpec @Account "account" "number" Account.codecViaNumber
    parseFailSpec Account.codecViaNumber (Number 0.1)
    parseFailSpec Account.codecViaNumber (Number 18446744073709551617)
    parseFailSpec Account.codecViaNumber (Number (-18446744073709551617))
    parseSuccessSpec Account.codecViaNumber (Number (-18446744073709551615)) (Negative (Amount 18446744073709551615))
    parseSuccessSpec Account.codecViaNumber (Number (-1)) (Negative (Amount 1))
    parseSuccessSpec Account.codecViaNumber (Number 1) (Positive (Amount 1))
    parseSuccessSpec Account.codecViaNumber (Number 18446744073709551615) (Positive (Amount 18446744073709551615))

  describe "AccountOf" $ do
    codecSpec @(AccountOf USD) "account-of" "string" AccountOf.codecViaString
    parseFailSpec AccountOf.codecViaString (String "0.1")
    parseFailSpec AccountOf.codecViaString (String "18446744073709551617")
    parseFailSpec AccountOf.codecViaString (String "-18446744073709551617")
    parseSuccessSpec AccountOf.codecViaString (String "-18446744073709551615") (AccountOf (Negative (Amount 18446744073709551615)))
    parseSuccessSpec AccountOf.codecViaString (String "-1") (AccountOf (Negative (Amount 1)))
    parseSuccessSpec AccountOf.codecViaString (String "1") (AccountOf (Positive (Amount 1)))
    parseSuccessSpec AccountOf.codecViaString (String "18446744073709551615") (AccountOf (Positive (Amount 18446744073709551615)))

    codecSpec @(AccountOf USD) "account-of" "number" AccountOf.codecViaNumber
    parseFailSpec AccountOf.codecViaNumber (Number 0.1)
    parseFailSpec AccountOf.codecViaNumber (Number 18446744073709551617)
    parseFailSpec AccountOf.codecViaNumber (Number (-18446744073709551617))
    parseSuccessSpec AccountOf.codecViaNumber (Number (-18446744073709551615)) (AccountOf (Negative (Amount 18446744073709551615)))
    parseSuccessSpec AccountOf.codecViaNumber (Number (-1)) (AccountOf (Negative (Amount 1)))
    parseSuccessSpec AccountOf.codecViaNumber (Number 1) (AccountOf (Positive (Amount 1)))
    parseSuccessSpec AccountOf.codecViaNumber (Number 18446744073709551615) (AccountOf (Positive (Amount 18446744073709551615)))

data USD
  deriving (Typeable)

instance IsCurrencyType USD where
  quantisationFactor Proxy = QuantisationFactor 100

codecSpec ::
  forall a.
  (Show a, Eq a, GenValid a) =>
  String ->
  String ->
  JSONCodec a ->
  Spec
codecSpec name dir c = describe name $ do
  it "has the same schema as before" $
    pureGoldenTextFile (concat ["test_resources/", name, "/", dir, ".txt"]) (renderColouredSchemaVia c)

  it "never fails to encode" $
    forAllValid $ \a -> do
      val <- evaluate (force (toJSONVia c a))
      shouldBeValid val

  it "roundtrips to json" $
    forAllValid $ \a ->
      let encoded = toJSONVia c a
          errOrDecoded = JSON.parseEither (parseJSONVia c) encoded
       in case errOrDecoded of
            Left err ->
              expectationFailure $
                unlines
                  [ "Decoding failed with error",
                    err,
                    "instead of decoding to",
                    show a,
                    "'encode' encoded it to the json",
                    show encoded
                  ]
            Right decoded ->
              let ctx =
                    unlines
                      [ "Decoding succeeded, but the decoded value",
                        show decoded,
                        "differs from expected decoded value",
                        show a,
                        "'encode' encoded it to the json",
                        show encoded
                      ]
               in context ctx $ decoded `shouldBe` a

parseSuccessSpec :: HasCallStack => (Show a, Eq a) => JSONCodec a -> JSON.Value -> a -> Spec
parseSuccessSpec c v expected = withFrozenCallStack $
  it (unwords ["fails to parse", show v]) $
    case JSON.parseEither (parseJSONVia c) v of
      Left err -> expectationFailure $ unlines ["Failed to parse:", err]
      Right actual -> actual `shouldBe` expected

parseFailSpec :: HasCallStack => Show a => JSONCodec a -> JSON.Value -> Spec
parseFailSpec c v = withFrozenCallStack $
  it (unwords ["fails to parse", show v]) $
    case JSON.parseEither (parseJSONVia c) v of
      Left _ -> pure ()
      Right a -> expectationFailure $ unlines ["Should have failed to decode, but got", ppShow a]
