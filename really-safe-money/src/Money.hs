{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}

module Money
  ( Amount (..),
    Currency (..),
    addAmount,
    AdditionFailure (..),
    subtractAmount,
    SubtractionFailure (..),
    multiplyAmount,
    MultiplicationFailure (..),
    divideAmount,
    DivisionFailure (..),
    fractionAmount,
    FractionFailure (..),
    CHF,
    USD,
    BTC,
  )
where

import Data.Int
import Data.Proxy
import Data.Word

-- | An amount of money. May be negative.
--
-- The underlying representation is 'Int64'.
-- This supports 1E18 minimal quantisations.
-- For example:
--
-- * 10 quadrillion USD ((much) more than the M1 money supply as of 2022)
-- * 50 quadrillion CHF ((much) more than the M1 money supply as of 2022)
-- * 10 billion BTC (more than the 21 million that can exist)
newtype Amount currency = Amount
  { unAmount :: Int64
  }

class Currency currency where
  quantisationFactor :: Proxy currency -> Word32

data CHF

instance Currency CHF where
  quantisationFactor Proxy = 20

data USD

instance Currency USD where
  quantisationFactor Proxy = 100

data BTC

instance Currency BTC where
  quantisationFactor Proxy = 100_000_000

data AdditionFailure = AdditionFailure

addAmount :: Amount currency -> Amount currency -> Either AdditionFailure (Amount currency)
addAmount = undefined

data SubtractionFailure = SubtractionFailure

subtractAmount :: Amount currency -> Amount currency -> Either SubtractionFailure (Amount currency)
subtractAmount = undefined

data MultiplicationFailure = MultiplicationFailure

multiplyAmount :: Int32 -> Amount currency -> Either MultiplicationFailure (Amount currency)
multiplyAmount = undefined

data DivisionFailure = DivisionFailure

divideAmount :: Amount currency -> Int32 -> Either DivisionFailure (Amount currency, Amount currency)
divideAmount = undefined

data FractionFailure = FractionFailure

fractionAmount :: Amount currency -> Rational -> Either FractionFailure (Amount currency)
fractionAmount = undefined
