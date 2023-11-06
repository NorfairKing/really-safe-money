{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Money.Format where

import Data.Proxy
import Data.Word
import Money.Account (Account)
import qualified Money.Account as Account
import Money.AccountOf (AccountOf)
import qualified Money.AccountOf as AccountOf
import Money.Amount (Amount)
import qualified Money.Amount as Amount
import Money.AmountOf (AmountOf)
import qualified Money.AmountOf as AmountOf
import Money.Currency
import Text.Printf

-- | Format an amount of money without a symbol.
formatAmount :: Word32 -> Amount -> String
formatAmount qf a =
  printf (quantisationFactorFormatString qf) (Amount.toDouble qf a)

-- | Format an account of money without a symbol.
formatAccount :: Word32 -> Account -> String
formatAccount qf a =
  printf (quantisationFactorFormatString qf) (Account.toDouble qf a)

-- | See 'formatAmount'
formatAmountOf :: forall currency. IsCurrencyType currency => AmountOf currency -> String
formatAmountOf ao = formatAmount (quantisationFactor (Proxy @currency)) (AmountOf.toAmount ao)

-- | See 'formatAccount'
formatAccountOf :: forall currency. IsCurrencyType currency => AccountOf currency -> String
formatAccountOf ao = formatAccount (quantisationFactor (Proxy @currency)) (AccountOf.toAccount ao)

-- | Produce a printf-style format string for a currency with a given quantisation factor.
--
-- >>> quantisationFactorFormatString 100000000
-- "%0.8f"
--
-- >>> quantisationFactorFormatString 100
-- "%0.2f"
--
-- >>> quantisationFactorFormatString 20
-- "%0.2f"
--
-- >>> quantisationFactorFormatString 1
-- "%0.0f"
quantisationFactorFormatString :: Word32 -> String
quantisationFactorFormatString qf =
  let decimals :: Int
      decimals = ceiling $ logBase 10 (fromIntegral qf :: Float)
   in printf "%%0.%df" decimals
