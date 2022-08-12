{-# LANGUAGE OverloadedStrings #-}

module Money.Account.Codec where

import Autodocodec
import Money.Account (Account)
import qualified Money.Account as Account
import Text.Read (readMaybe)

-- | A 'JSONCodec' for 'Account' which encodes amounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'Account'
--
-- WARNING: This codec does not protect you against deserialising an account in
-- a different currency than the account that was serialised.
accountCodecViaString :: JSONCodec Account
accountCodecViaString = bimapCodec f g stringCodec <?> "Account"
  where
    f :: String -> Either String Account
    f s = case readMaybe s >>= Account.fromMinimalQuantisations of
      Nothing -> Left $ unwords ["Could not read string as an Account:", s]
      Just a -> Right a
    g :: Account -> String
    g = show . Account.toMinimalQuantisations
