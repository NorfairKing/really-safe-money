-- | Codecs for 'AmountOf'
--
-- This does not contain:
--
-- * `codecViaDecimalLiteral` because converting an 'AmountOf' to a 'DecimalLiteral' can fail.
-- * A codec that en/decodes a 'Scientific' because that can fail.
module Money.AmountOf.Codec
  ( codecViaString,
    codecViaNumber,
  )
where

import Autodocodec
import qualified Money.Amount.Codec as Amount
import Money.AmountOf (AmountOf)
import qualified Money.AmountOf as AmountOf

-- $setup
-- >>> import Autodocodec (toJSONVia, parseJSONVia)
-- >>> import Data.Aeson (Value(..))
-- >>> import Data.Aeson.Types as JSON (parseMaybe)
-- >>> import Money.Amount (Amount(..))
-- >>> import Money.AmountOf (AmountOf(..))
-- >>> :set -XOverloadedStrings

-- | A 'JSONCodec' for 'AmountOf' which encodes amounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'Amount'
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
--
-- >>> toJSONVia codecViaString (AmountOf (Amount 5))
-- String "5"
--
-- -- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "1") :: Maybe (AmountOf currency)
-- Just (AmountOf (Amount 1))
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "1.0") :: Maybe (AmountOf currency)
-- Nothing
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "0.1") :: Maybe (AmountOf currency)
-- Nothing
codecViaString :: JSONCodec (AmountOf currency)
codecViaString =
  dimapCodec
    AmountOf.fromAmount
    AmountOf.toAmount
    Amount.codecViaString

-- | A 'JSONCodec' for 'AmountOf' which encodes amounts as a JSON Number of minimal quantisations.
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
--
-- >>> toJSONVia codecViaNumber (AmountOf (Amount 5))
-- Number 5.0
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 1.0) :: Maybe (AmountOf currency)
-- Just (AmountOf {unAmountOf = Amount 1})
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 0.1) :: Maybe (AmountOf currency)
-- Nothing
codecViaNumber :: JSONCodec (AmountOf currency)
codecViaNumber =
  dimapCodec
    AmountOf.fromAmount
    AmountOf.toAmount
    Amount.codecViaNumber
