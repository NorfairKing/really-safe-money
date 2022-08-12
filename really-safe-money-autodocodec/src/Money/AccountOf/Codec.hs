module Money.AccountOf.Codec where

import Autodocodec
import Money.Account.Codec
import Money.AccountOf (AccountOf)
import qualified Money.AccountOf as AccountOf

-- | A 'JSONCodec' for 'AccountOf' which encodes accounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'Account'
--
-- WARNING: This codec does not protect you against deserialising an account in
-- a different currency than the account that was serialised.
accountOfCodecViaString :: JSONCodec (AccountOf currency)
accountOfCodecViaString = dimapCodec AccountOf.fromAccount AccountOf.toAccount accountCodecViaString
