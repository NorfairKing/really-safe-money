module Money.AmountOf.Codec where

import Autodocodec
import Money.Amount.Codec
import Money.AmountOf (AmountOf)
import qualified Money.AmountOf as AmountOf

-- | A 'JSONCodec' for 'AmountOf' which encodes amounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'Amount'
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
amountOfCodecViaString :: JSONCodec (AmountOf currency)
amountOfCodecViaString = dimapCodec AmountOf.fromAmount AmountOf.toAmount amountCodecViaString
