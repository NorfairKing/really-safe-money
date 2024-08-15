-- | Codecs for 'AmountOf'
--
-- This does not contain:
--
-- * `codecViaDecimalLiteral` because converting an 'AmountOf' to a 'DecimalLiteral' can fail.
-- * A codec that en/decodes a 'Scientific' because that can fail.
module Money.AccountOf.Codec
  ( codecViaString,
    codecViaNumber,
  )
where

import Autodocodec
import qualified Money.Account.Codec as Account
import Money.AccountOf (AccountOf)
import qualified Money.AccountOf as AccountOf

-- $setup
-- >>> import Autodocodec (toJSONVia, parseJSONVia)
-- >>> import Data.Aeson (Value(..))
-- >>> import Data.Aeson.Types as JSON (parseMaybe)
-- >>> import Money.Account (Account(..))
-- >>> import Money.AccountOf (AccountOf(..))
-- >>> import Money.AmountOf (AmountOf(..))
-- >>> import Money.Amount (Amount(..))
-- >>> :set -XOverloadedStrings

-- | A 'JSONCodec' for 'AccountOf' which encodes accounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'Account'
--
-- WARNING: This codec does not protect you against deserialising an account in
-- a different currency than the account that was serialised.
codecViaString :: JSONCodec (AccountOf currency)
codecViaString =
  dimapCodec
    AccountOf.fromAccount
    AccountOf.toAccount
    Account.codecViaString

-- | A 'JSONCodec' for 'AccountOf' which encodes amounts as a JSON Number of minimal quantisations.
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
--
-- >>> toJSONVia codecViaNumber (AccountOf (Negative (Amount 5)))
-- Number (-5.0)
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 1.0) :: Maybe (AccountOf currency)
-- Just (AccountOf {unAccountOf = Positive (Amount 1)})
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 0.1) :: Maybe (AccountOf currency)
-- Nothing
codecViaNumber :: JSONCodec (AccountOf currency)
codecViaNumber =
  dimapCodec
    AccountOf.fromAccount
    AccountOf.toAccount
    Account.codecViaNumber
