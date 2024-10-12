{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Money.Currency
  ( IsCurrencyType (..),
    Currency (..),
  )
where

import Control.DeepSeq
import Data.Data
import Data.Validity
import GHC.Generics (Generic)
import Money.QuantisationFactor

-- | Class of type-level currencies
--
-- For example:
--
-- > data USD
-- >  deriving (Generic, Typeable)
-- > instance Currency USD where
-- >   quantisationFactor Proxy = 100
--
-- > data BTC
-- >  deriving (Generic, Typeable)
-- > instance Currency BTC where
-- >   quantisationFactor Proxy = 100_000_000
class IsCurrencyType (currency :: k) where
  quantisationFactor :: Proxy currency -> QuantisationFactor

-- | Term-level currency
data Currency = Currency
  { currencySymbol :: !String,
    currencyQuantisationFactor :: !QuantisationFactor
  }
  deriving (Show, Read, Eq, Ord, Data, Generic)

instance Validity Currency

instance NFData Currency
