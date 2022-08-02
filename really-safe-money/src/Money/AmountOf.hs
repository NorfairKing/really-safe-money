{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports -Wno-dodgy-exports #-}

module Money.AmountOf
  ( AmountOf (..),
    zero,
    toMinimalQuantisations,
    fromMinimalQuantisations,
    fromDouble,
    toDouble,
    fromRational,
    toRational,
    Currency (..),
    add,
    Amount.AdditionFailure (..),
    subtract,
    Amount.SubtractionFailure (..),
    multiply,
    Amount.MultiplicationFailure (..),
    divide,
    Amount.DivisionFailure (..),
    fraction,
    Amount.FractionFailure (..),
  )
where

import Data.Int
import Data.Proxy
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits
import Money.Amount (Amount)
import qualified Money.Amount as Amount
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
newtype AmountOf currency = AmountOf
  { unAmountOf :: Amount
  }
  deriving (Show, Eq, Generic)

instance Validity (AmountOf currency)

instance
  TypeError
    ( 'Text "This would require that Amounts of money are an instance of Num"
        ':$$: 'Text "Amounts of money must not be an instance of Num. Don't do this."
    ) =>
  Num (AmountOf currency)
  where
  (+) = error "unreachable"
  (*) = error "unreachable"
  abs = error "unreachable"
  signum = error "unreachable"
  fromInteger = error "unreachable"
  negate = error "unreachable"
  (-) = error "unreachable"

zero :: AmountOf currency
zero = AmountOf Amount.zero

toMinimalQuantisations :: AmountOf currency -> Int64
toMinimalQuantisations = Amount.toMinimalQuantisations . unAmountOf

fromMinimalQuantisations :: Int64 -> AmountOf currency
fromMinimalQuantisations = AmountOf . Amount.fromMinimalQuantisations

fromDouble :: forall currency. Currency currency => Double -> Maybe (AmountOf currency)
fromDouble = fmap AmountOf . Amount.fromDouble (quantisationFactor (Proxy @currency))

toDouble :: forall currency. Currency currency => AmountOf currency -> Double
toDouble = Amount.toDouble (quantisationFactor (Proxy @currency)) . unAmountOf

fromRational :: forall currency. Currency currency => Rational -> Maybe (AmountOf currency)
fromRational = fmap AmountOf . Amount.fromRational (quantisationFactor (Proxy @currency))

toRational :: forall currency. Currency currency => AmountOf currency -> Rational
toRational = Amount.toRational (quantisationFactor (Proxy @currency)) . unAmountOf

-- | Add two amounts of money.
--
-- This operation may fail with an 'AdditionFailure' for the following reasons:
--
-- TODO
add :: AmountOf currency -> AmountOf currency -> Either Amount.AdditionFailure (AmountOf currency)
add (AmountOf a1) (AmountOf a2) = AmountOf <$> Amount.add a1 a2

subtract :: AmountOf currency -> AmountOf currency -> Either Amount.SubtractionFailure (AmountOf currency)
subtract (AmountOf a1) (AmountOf a2) = AmountOf <$> Amount.subtract a1 a2

-- API Note: The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
multiply ::
  Int32 ->
  AmountOf currency ->
  Either Amount.MultiplicationFailure (AmountOf currency)
multiply f (AmountOf a) = AmountOf <$> Amount.multiply f a

-- API Note: The order of arguments in 'multiply' and 'divide' is reversed to increase the likelyhood of a compile-error when refactoring.
divide ::
  AmountOf currency ->
  Word32 ->
  Either Amount.DivisionFailure (AmountOf currency)
divide (AmountOf a) i = AmountOf <$> Amount.divide a i

fraction ::
  AmountOf currency ->
  Rational ->
  (AmountOf currency, Rational)
fraction (AmountOf a) f =
  let (a', r) = Amount.fraction a f
   in (AmountOf a', r)
