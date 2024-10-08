{-# LANGUAGE OverloadedStrings #-}

-- | Codecs for 'Amount'
--
-- This does not contain:
--
-- * `codecViaDecimalLiteral` because converting an 'Amount' to a 'DecimalLiteral' can fail.
-- * A codec that en/decodes a 'Scientific' because that can fail.
module Money.Amount.Codec
  ( codecViaString,
    codecViaNumber,
  )
where

import Autodocodec
import Data.Word
import Money.Amount (Amount)
import qualified Money.Amount as Amount
import Text.Read (readMaybe)

-- $setup
-- >>> import Autodocodec (toJSONVia, parseJSONVia)
-- >>> import Data.Aeson (Value(..))
-- >>> import Data.Aeson.Types as JSON (parseMaybe)
-- >>> import Money.Amount (Amount(..))
-- >>> :set -XOverloadedStrings

-- | A 'JSONCodec' for 'Amount' which encodes amounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'Amount'
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
--
-- >>> toJSONVia codecViaString (Amount 5)
-- String "5"
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "1") :: Maybe Amount
-- Just (Amount 1)
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "1.0") :: Maybe Amount
-- Nothing
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "0.1") :: Maybe Amount
-- Nothing
codecViaString :: JSONCodec Amount
codecViaString = bimapCodec f g stringCodec <?> "Amount"
  where
    f :: String -> Either String Amount
    f s = case readMaybe s of
      Nothing -> Left $ unwords ["Could not read string as an Amount:", s]
      Just i ->
        if (i :: Integer) < toInteger (minBound :: Word64)
          then Left $ unwords ["Negative number of minimal quantisations:", show i]
          else
            if (i :: Integer) > toInteger (maxBound :: Word64)
              then Left $ unwords ["Number of minimal quantisations is too big:", show i]
              else Right $ Amount.fromMinimalQuantisations (fromIntegral i :: Word64)
    g :: Amount -> String
    g = show . Amount.toMinimalQuantisations

-- | A 'JSONCodec' for 'Amount' which encodes amounts as an integral JSON 'Number' of minimal quantisations.
--
-- Note that this codec will fail to parse non-integral numbers.
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
--
-- >>> toJSONVia codecViaNumber (Amount 5)
-- Number 5.0
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 1.0) :: Maybe Amount
-- Just (Amount 1)
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 0.1) :: Maybe Amount
-- Nothing
codecViaNumber :: JSONCodec Amount
codecViaNumber =
  dimapCodec
    Amount.fromMinimalQuantisations
    Amount.toMinimalQuantisations
    codec
    <?> "Amount"
