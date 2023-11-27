{-# LANGUAGE OverloadedStrings #-}

module Money.QuantisationFactor.Codec
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
-- >>> import Money.QuantisationFactor (QuantisationFactor(..))
-- >>> :set -XOverloadedStrings

-- | A 'JSONCodec' for 'QuantisationFactor' which encodes amounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'QuantisationFactor'
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
--
-- >>> toJSONVia codecViaString (QuantisationFactor 5)
-- String "5"
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "1") :: Maybe QuantisationFactor
-- Just (QuantisationFactor 1)
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "1.0") :: Maybe QuantisationFactor
-- Nothing
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "0.1") :: Maybe QuantisationFactor
-- Nothing
codecViaString :: JSONCodec QuantisationFactor
codecViaString = bimapCodec f g stringCodec <?> "QuantisationFactor"
  where
    f :: String -> Either String QuantisationFactor
    f s = case readMaybe s of
      Nothing -> Left $ unwords ["Could not read string as an QuantisationFactor:", s]
      Just i ->
        if (i :: Integer) < toInteger (minBound :: Word64)
          then Left $ unwords ["Negative number of minimal quantisations:", show i]
          else
            if (i :: Integer) > toInteger (maxBound :: Word64)
              then Left $ unwords ["Number of minimal quantisations is too big:", show i]
              else Right $ QuantisationFactor.fromMinimalQuantisations (fromIntegral i :: Word64)
    g :: QuantisationFactor -> String
    g = show . QuantisationFactor.toMinimalQuantisations

-- | A 'JSONCodec' for 'QuantisationFactor' which encodes amounts as an integral JSON 'Number' of minimal quantisations.
--
-- Note that this codec will fail to parse non-integral numbers.
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
--
-- >>> toJSONVia codecViaNumber (QuantisationFactor 5)
-- Number 5.0
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 1.0) :: Maybe QuantisationFactor
-- Just (QuantisationFactor 1)
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 0.1) :: Maybe QuantisationFactor
-- Nothing
codecViaNumber :: JSONCodec QuantisationFactor
codecViaNumber =
  dimapCodec
    QuantisationFactor.fromMinimalQuantisations
    QuantisationFactor.toMinimalQuantisations
    codec
    <?> "QuantisationFactor"
