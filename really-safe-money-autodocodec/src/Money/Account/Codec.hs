{-# LANGUAGE OverloadedStrings #-}

-- | Codecs for 'Account'
--
-- This does not contain:
--
-- * `codecViaDecimalLiteral` because converting an 'Account' to a 'DecimalLiteral' can fail.
-- * A codec that en/decodes a 'Scientific' because that can fail.
module Money.Account.Codec
  ( codecViaString,
    codecViaNumber,
  )
where

import Autodocodec
import Money.Account (Account (..))
import qualified Money.Account as Account
import Text.Read (readMaybe)

-- $setup
-- >>> import Autodocodec (toJSONVia, parseJSONVia)
-- >>> import Data.Aeson (Value(..))
-- >>> import Data.Aeson.Types as JSON (parseMaybe)
-- >>> import Money.Account (Account(..))
-- >>> import Money.Amount (Amount(..))
-- >>> :set -XOverloadedStrings

-- | A 'JSONCodec' for 'Account' which encodes amounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'Account'
--
-- WARNING: This codec does not protect you against deserialising an account in
-- a different currency than the account that was serialised.
--
-- >>> toJSONVia codecViaString (Positive (Amount 5))
-- String "5"
-- >>> toJSONVia codecViaString (Negative (Amount 6))
-- String "-6"
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "1") :: Maybe Account
-- Just (Positive (Amount 1))
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "-1") :: Maybe Account
-- Just (Negative (Amount 1))
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "1.0") :: Maybe Account
-- Nothing
-- >>> JSON.parseMaybe (parseJSONVia codecViaString) (String "0.1") :: Maybe Account
-- Nothing
codecViaString :: JSONCodec Account
codecViaString = bimapCodec f g stringCodec <?> "Account"
  where
    f :: String -> Either String Account
    f s = case readMaybe s >>= Account.fromMinimalQuantisations of
      Nothing -> Left $ unwords ["Could not read string as an Account:", s]
      Just a -> Right a
    g :: Account -> String
    g = show . Account.toMinimalQuantisations

-- | A 'JSONCodec' for 'Account' which encodes amounts as an integral JSON 'Number' of minimal quantisations.
--
-- Note that this codec will fail to parse non-integral numbers.
--
-- WARNING: This codec does not protect you against deserialising an account in
-- a different currency than the amount that was serialised.
--
-- >>> toJSONVia codecViaNumber (Negative (Amount 5))
-- Number (-5.0)
--
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 1.0) :: Maybe Account
-- Just (Positive (Amount 1))
-- >>> JSON.parseMaybe (parseJSONVia codecViaNumber) (Number 0.1) :: Maybe Account
-- Nothing
codecViaNumber :: JSONCodec Account
codecViaNumber =
  bimapCodec
    f
    Account.toMinimalQuantisations
    codec
    <?> "Account"
  where
    f :: Integer -> Either String Account
    f i = case Account.fromMinimalQuantisations i of
      Nothing -> Left $ "Number did not fit into an account value: " <> show i
      Just a -> Right a
