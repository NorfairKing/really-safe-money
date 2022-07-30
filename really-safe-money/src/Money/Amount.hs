{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Money.Amount
  ( Amount (..),
    toMinimalQuantisations,
    fromMinimalQuantisations,
    fromDouble,
    toDouble,
    fromRational,
    toRational,
    Currency (..),
    add,
    AdditionFailure (..),
    subtract,
    SubtractionFailure (..),
    multiply,
    MultiplicationFailure (..),
    divide,
    DivisionFailure (..),
    fraction,
    FractionFailure (..),
  )
where

import Data.Int
import Data.Proxy
import Data.Validity
import GHC.Generics (Generic)
import GHC.TypeLits
import Money.Currency as Currency
import Prelude hiding (fromRational, subtract, toRational)

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
  { toMinimalQuantisations :: Int64
  }
  deriving (Show, Eq, Generic)

instance Validity (Amount currency)

instance
  TypeError ('Text "Amounts of money cannot be an instance of Num, don't do it.") =>
  Num (Amount currency)
  where
  (+) = error "unreachable"
  (*) = error "unreachable"
  abs = error "unreachable"
  signum = error "unreachable"
  fromInteger = error "unreachable"
  negate = error "unreachable"
  (-) = error "unreachable"

fromMinimalQuantisations :: Int64 -> Amount currency
fromMinimalQuantisations = Amount

fromDouble :: forall currency. Currency currency => Double -> Maybe (Amount currency)
fromDouble d = Just $ Amount $ round d * fromIntegral (quantisationFactor (Proxy @currency))

toDouble :: forall currency. Currency currency => Amount currency -> Double
toDouble a = fromIntegral (toMinimalQuantisations a) / fromIntegral (quantisationFactor (Proxy @currency))

fromRational :: forall currency. Currency currency => Rational -> Maybe (Amount currency)
fromRational r = Just $ Amount $ round r * fromIntegral (quantisationFactor (Proxy @currency))

toRational :: forall currency. Currency currency => Amount currency -> Rational
toRational a = fromIntegral (toMinimalQuantisations a) / fromIntegral (quantisationFactor (Proxy @currency))

data AdditionFailure = AdditionFailure

add :: Amount currency -> Amount currency -> Either AdditionFailure (Amount currency)
add = undefined

data SubtractionFailure = SubtractionFailure

subtract :: Amount currency -> Amount currency -> Either SubtractionFailure (Amount currency)
subtract = undefined

data MultiplicationFailure = MultiplicationFailure

multiply ::
  Int32 ->
  Amount currency ->
  Either MultiplicationFailure (Amount currency)
multiply = undefined

data DivisionFailure = DivisionFailure

divide ::
  Amount currency ->
  Int32 ->
  Either DivisionFailure (Amount currency, Amount currency)
divide = undefined

data FractionFailure = FractionFailure

fraction ::
  Amount currency ->
  Rational ->
  Either FractionFailure (Amount currency, Rational)
fraction = undefined
