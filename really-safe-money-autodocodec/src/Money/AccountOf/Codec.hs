module Money.AccountOf.Codec (codecViaString) where

import Autodocodec
import qualified Money.Account.Codec as Account
import Money.AccountOf (AccountOf)
import qualified Money.AccountOf as AccountOf

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
