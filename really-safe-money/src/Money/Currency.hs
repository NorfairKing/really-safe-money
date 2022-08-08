{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}

module Money.Currency
  ( Currency (..),
    CHF,
    USD,
    BTC,
    ADA,
  )
where

import Data.Proxy
import Data.Typeable
import Data.Word

class Currency (currency :: k) where
  quantisationFactor :: Proxy currency -> Word32

data CHF
  deriving (Typeable)

instance Currency CHF where
  quantisationFactor Proxy = 20

data USD
  deriving (Typeable)

instance Currency USD where
  quantisationFactor Proxy = 100

data BTC
  deriving (Typeable)

instance Currency BTC where
  quantisationFactor Proxy = 100_000_000

data ADA
  deriving (Typeable)

instance Currency ADA where
  quantisationFactor Proxy = 1_000_000
