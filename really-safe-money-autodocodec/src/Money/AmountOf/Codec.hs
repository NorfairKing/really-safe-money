module Money.AmountOf.Codec (codecViaString) where

import Autodocodec
import qualified Money.Amount.Codec as Amount
import Money.AmountOf (AmountOf)
import qualified Money.AmountOf as AmountOf

-- | A 'JSONCodec' for 'AmountOf' which encodes amounts as a JSON String that contains a decimal number which represents the minimal quantisations of the 'Amount'
--
-- WARNING: This codec does not protect you against deserialising an amount in
-- a different currency than the amount that was serialised.
codecViaString :: JSONCodec (AmountOf currency)
codecViaString =
  dimapCodec
    AmountOf.fromAmount
    AmountOf.toAmount
    Amount.codecViaString
