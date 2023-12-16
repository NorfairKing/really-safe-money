{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-duplicate-exports -Wno-dodgy-exports #-}

-- === Importing this module
--
-- This module is designed to be imported as follows:
--
-- @
-- import Money.MultiAmount (MultiAmount)
-- import qualified Money.MultiAmount as MultiAmount
-- @
module Money.MultiAmount
  ( MultiAmount (..),
    fromAmount,
    zero,
    add,
    sum,
    addAmount,
    subtractAmount,
    convertAll,
    convertAllA,
    Rounded (..),
  )
where

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ratio
import Data.Validity
import Data.Validity.Map
import Data.Word
import GHC.Generics (Generic)
import Money.Amount (Amount, Rounding (..))
import qualified Money.Amount as Amount
import Money.ConversionRate (ConversionRate)
import qualified Money.ConversionRate as ConversionRate
import Money.QuantisationFactor (QuantisationFactor)
import Numeric.Natural
import Prelude hiding (sum)
import qualified Prelude

-- | A type for a combination of amounts of different currencies
--
-- This uses a 'currency' type parameter so that you can choose how to
-- represent the currencies that are being accounted for.
newtype MultiAmount currency = MultiAmount
  { unMultiAmount :: Map currency Amount
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance (Validity currency, Show currency, Ord currency) => Validity (MultiAmount currency) where
  validate ma@(MultiAmount m) =
    mconcat
      [ genericValidate ma,
        decorateMap m $ \_ a ->
          declare "The amount is not zero" $
            a /= Amount.zero
      ]

instance NFData currency => NFData (MultiAmount currency)

fromAmount :: currency -> Amount -> MultiAmount currency
fromAmount currency amount =
  if amount == Amount.zero
    then zero
    else MultiAmount $ M.singleton currency amount

-- | No money of any currency
zero :: MultiAmount currency
zero = MultiAmount M.empty

-- | Add two 'MultiAmount's
add :: forall currency. Ord currency => MultiAmount currency -> MultiAmount currency -> Maybe (MultiAmount currency)
add m1 = foldM (\m (c, a) -> addAmount m c a) m1 . M.toList . unMultiAmount

-- | Add multiple 'MultiAmount's
sum :: (Foldable f, Ord currency) => f (MultiAmount currency) -> Maybe (MultiAmount currency)
sum = foldM add zero

-- | Add an 'Amount' to a 'MultiAmount'
addAmount :: Ord currency => MultiAmount currency -> currency -> Amount -> Maybe (MultiAmount currency)
addAmount m _ (Amount 0) = Just m
addAmount (MultiAmount m) currency amount =
  fmap MultiAmount $ case M.lookup currency m of
    Nothing -> Just $ M.insert currency amount m
    Just a -> do
      r <- Amount.add a amount
      Just $
        if r == Amount.zero
          then M.delete currency m
          else M.insert currency r m

-- | Subtract an 'Amount' from a 'MultiAmount'
subtractAmount :: Ord currency => MultiAmount currency -> currency -> Amount -> Maybe (MultiAmount currency)
subtractAmount m _ (Amount 0) = Just m
subtractAmount (MultiAmount m) currency amount =
  fmap MultiAmount $ case M.lookup currency m of
    Nothing -> Nothing -- Can't go below zero
    Just a -> do
      r <- Amount.subtract a amount
      Just $
        if r == Amount.zero
          then M.delete currency m
          else M.insert currency r m

-- | Try to convert every amount to one currency.
--
-- This function can be more precise than calling 'Amount.convert' on each
-- amount and then adding the results together because rounding only happens once.
--
-- Note that this fails when the result becomes too big.
convertAll ::
  -- Where to round the result
  Rounding ->
  -- The quantisation factor of the currency to convert to
  QuantisationFactor ->
  -- The conversion rate and quantisation factor of each currency to convert from
  (currency -> (ConversionRate, QuantisationFactor)) ->
  -- The 'MultiAmount' to convert
  MultiAmount currency ->
  -- | The actual result and which rounding happened
  (Maybe Amount, Rounded)
convertAll r qf1 func = runIdentity . convertAllA r qf1 (Identity . func)

-- | Like 'convertAll', but you can decide to convert in your own Applicative.
convertAllA ::
  Applicative f =>
  -- Where to round the result
  Rounding ->
  -- The quantisation factor of the currency to convert to
  QuantisationFactor ->
  -- The conversion rate and quantisation factor of each currency to convert from
  (currency -> f (ConversionRate, QuantisationFactor)) ->
  -- The 'MultiAmount' to convert
  MultiAmount currency ->
  -- | The actual result and which rounding happened
  f (Maybe Amount, Rounded)
convertAllA r qf1 func =
  fmap
    ( ( \theoreticalResult ->
          let rounder :: Ratio Natural -> Natural
              rounder = case r of
                RoundUp -> ceiling
                RoundDown -> floor
                RoundNearest -> round
              roundedResult :: Natural
              roundedResult = rounder theoreticalResult
              maxBoundN :: Natural
              maxBoundN = fromIntegral (maxBound :: Word64)
              actualResult =
                if roundedResult > maxBoundN
                  then Nothing
                  else Just (fromIntegral roundedResult)
              rounded = case compare (fromIntegral roundedResult) theoreticalResult of
                LT -> RoundedDown
                EQ -> DidNotRound
                GT -> RoundedUp
           in (Amount.fromMinimalQuantisations <$> actualResult, rounded)
      )
        . Prelude.sum
    )
    . traverse
      ( \(currency, a) ->
          ( \(cr, qf2) ->
              fromIntegral (Amount.toMinimalQuantisations a) * ConversionRate.conversionFactor qf2 cr qf1
          )
            <$> func currency
      )
    . M.toList
    . unMultiAmount

data Rounded
  = RoundedDown
  | DidNotRound
  | RoundedUp
  deriving (Show, Eq, Generic)
