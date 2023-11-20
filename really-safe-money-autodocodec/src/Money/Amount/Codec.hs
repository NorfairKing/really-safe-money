{-# LANGUAGE OverloadedStrings #-}

module Money.Amount.Codec (codecViaString) where

import Autodocodec
import Data.Word
import Money.Amount (Amount)
import qualified Money.Amount as Amount
import Text.Read (readMaybe)

-- | A 'JSONCodec' for 'Amount' which encodes amounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'Amount'
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
codecViaString :: JSONCodec Amount
codecViaString = bimapCodec f g stringCodec <?> "Amount"
  where
    f :: String -> Either String Amount
    f s = case readMaybe s of
      Nothing -> Left $ unwords ["Could not read string as an Amount:", s]
      Just i -> Right $ Amount.fromMinimalQuantisations (i :: Word64)
    g :: Amount -> String
    g = show . Amount.toMinimalQuantisations
