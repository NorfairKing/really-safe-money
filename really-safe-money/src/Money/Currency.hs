{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}

module Money.Currency
  ( IsCurrencyType (..),
    CHF,
    USD,
    BTC,
    ADA,
  )
where

import Data.Proxy
import Data.Typeable
import Data.Word

-- | Class of type-level currencies
--
-- For example:
-- @
-- instance Currency USD where
--   quantisationFactor Proxy = 100
--
-- instance Currency BTC where
--   quantisationFactor Proxy = 100_000_000
-- @
class IsCurrencyType (currency :: k) where
  quantisationFactor :: Proxy currency -> Word32

data CHF
  deriving (Typeable)

instance IsCurrencyType CHF where
  quantisationFactor Proxy = 20

data USD
  deriving (Typeable)

instance IsCurrencyType USD where
  quantisationFactor Proxy = 100

data BTC
  deriving (Typeable)

instance IsCurrencyType BTC where
  quantisationFactor Proxy = 100_000_000

data ADA
  deriving (Typeable)

instance IsCurrencyType ADA where
  quantisationFactor Proxy = 1_000_000
