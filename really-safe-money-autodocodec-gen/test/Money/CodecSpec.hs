{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Money.CodecSpec (spec) where

import Data.Aeson.Types as JSON
import Data.Typeable
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
import Money.Autodocodec.Gen
import Money.Currency (IsCurrencyType (..))
import Money.QuantisationFactor (QuantisationFactor (..))
import Money.QuantisationFactor.Codec as QuantisationFactor
import Money.QuantisationFactor.Gen ()
import Test.Syd

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
    parseFailMessageSpec @Amount Amount.codecViaString (String "0.1") "Error in $: Could not read string as an Amount: 0.1"
    parseFailSpec Amount.codecViaString (String "-1")
    parseFailMessageSpec @Amount Amount.codecViaString (String "-1") "Error in $: Negative number of minimal quantisations: -1"
    parseFailSpec Amount.codecViaString (String "18446744073709551617")
    parseFailMessageSpec @Amount Amount.codecViaString (String "18446744073709551616") "Error in $: Number of minimal quantisations is too big: 18446744073709551616"
    parseSuccessSpec Amount.codecViaString (String "0") (Amount 0)
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
    parseFailMessageSpec @Account Account.codecViaString (String "0.1") "Error in $: Could not read string as an Account: 0.1"
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

instance IsCurrencyType USD where
  quantisationFactor Proxy = QuantisationFactor 100
