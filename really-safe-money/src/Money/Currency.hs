{-# LANGUAGE PolyKinds #-}

module Money.Currency
  ( IsCurrencyType (..),
  )
where

import Data.Proxy
import Data.Word

-- | Class of type-level currencies
--
-- For example:
-- @
-- data USD
--  deriving (Generic, Typeable)
-- instance Currency USD where
--   quantisationFactor Proxy = 100
--
-- data BTC
--  deriving (Generic, Typeable)
-- instance Currency BTC where
--   quantisationFactor Proxy = 100_000_000
-- @
class IsCurrencyType (currency :: k) where
  quantisationFactor :: Proxy currency -> Word32
