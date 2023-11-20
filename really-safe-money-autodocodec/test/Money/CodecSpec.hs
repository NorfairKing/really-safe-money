{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.CodecSpec (spec) where

import Autodocodec
import Autodocodec.Yaml
import Control.DeepSeq
import Control.Exception
import Data.Aeson.Types as JSON
import Data.Typeable
import Money.Account (Account)
import Money.Account.Codec as Account
import Money.Account.Gen ()
import Money.AccountOf (AccountOf)
import Money.AccountOf.Codec as AccountOf
import Money.AccountOf.Gen ()
import Money.Amount (Amount)
import Money.Amount.Codec as Amount
import Money.Amount.Gen ()
import Money.AmountOf (AmountOf)
import Money.AmountOf.Codec as AmountOf
import Money.AmountOf.Gen ()
import Money.Currency (IsCurrencyType (..))
import Money.QuantisationFactor (QuantisationFactor (..))
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  codecSpec @Amount "amount" "string" Amount.codecViaString
  codecSpec @Amount "amount" "number" Amount.codecViaNumber
  codecSpec @(AmountOf USD) "amount-of" "string" AmountOf.codecViaString
  codecSpec @Account "account" "string" Account.codecViaString
  codecSpec @(AccountOf USD) "account-of" "string" AccountOf.codecViaString

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
