{-# LANGUAGE OverloadedStrings #-}

module Money.QuantisationFactor.Codec
  ( codecViaNumber,
  )
where

import Autodocodec
import Money.QuantisationFactor (QuantisationFactor (..))
import qualified Money.QuantisationFactor as QuantisationFactor

-- $setup
-- >>> import Autodocodec (toJSONVia, parseJSONVia)
-- >>> import Data.Aeson (Value(..))
-- >>> import Data.Aeson.Types as JSON (parseMaybe)
-- >>> import Money.QuantisationFactor (QuantisationFactor(..))
-- >>> :set -XOverloadedStrings

-- | A 'JSONCodec' for 'QuantisationFactor' which encodes the factor as an integral JSON 'Number'
--
-- Note that this codec will fail to parse non-integral numbers.
--
--
-- >>> toJSONVia codecViaNumber (QuantisationFactor 5)
-- Number 5.0
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 1.0) :: Maybe QuantisationFactor
-- Just (QuantisationFactor {unQuantisationFactor = 1})
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 0) :: Maybe QuantisationFactor
-- Nothing
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 0.1) :: Maybe QuantisationFactor
-- Nothing
codecViaNumber :: JSONCodec QuantisationFactor
codecViaNumber =
  bimapCodec
    f
    unQuantisationFactor
    codec
    <?> "QuantisationFactor"
  where
    f w = case QuantisationFactor.fromWord32 w of
      Nothing -> Left $ "Unable to parse as a quantisation factor: " <> show w
      Just qf -> Right qf
