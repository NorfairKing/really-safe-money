{-# LANGUAGE OverloadedStrings #-}

module Numeric.DecimalLiteral.Codec
  ( codecViaString,
  )
where

import Autodocodec
import Numeric.DecimalLiteral (DecimalLiteral (..))
import qualified Numeric.DecimalLiteral as DecimalLiteral

-- $setup
-- >>> import Autodocodec (toJSONVia, parseJSONVia)
-- >>> import Data.Aeson (Value(..))
-- >>> import Data.Aeson.Types as JSON (parseMaybe)
-- >>> import Numeric.DecimalLiteral (DecimalLiteral(..))
-- >>> :set -XOverloadedStrings

-- | A 'JSONCodec' for 'DecimalLiteral' which encodes amounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'DecimalLiteral'
--
-- WARNING: This codec does not protect you against deserialising an account in
-- a different currency than the account that was serialised.
--
-- >>> toJSONVia codecViaString (DecimalLiteral Nothing 3 1)
-- String "0.3"
-- >>> toJSONVia codecViaString (DecimalLiteral (Just True) 5 0)
-- String "+5"
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "1") :: Maybe DecimalLiteral
-- Just (DecimalLiteral Nothing 1 0)
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "-1") :: Maybe DecimalLiteral
-- Just (DecimalLiteral (Just False) 1 0)
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "three") :: Maybe DecimalLiteral
-- Nothing
codecViaString :: JSONCodec DecimalLiteral
codecViaString = bimapCodec f g stringCodec <?> "DecimalLiteral"
  where
    f :: String -> Either String DecimalLiteral
    f s = case DecimalLiteral.fromString s of
      Nothing -> Left $ unwords ["Could not read string as a DecimalLiteral:", s]
      Just a -> Right a
    g :: DecimalLiteral -> String
    g = DecimalLiteral.format
