{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-duplicate-exports -Wno-dodgy-exports #-}

-- === Importing this module
--
-- This module is designed to be imported as follows:
--
-- @
-- import Money.MultiAccount (MultiAccount)
-- import qualified Money.MultiAccount as MultiAccount
-- @
module Money.MultiAccount
  ( MultiAccount (..),
    fromAccount,
    zero,
    add,
    subtract,
    sum,
    lookupAccount,
    addAmount,
    subtractAmount,
    addAccount,
    subtractAccount,
    Rounding (..),
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
import Data.Maybe
import Data.Ratio
import Data.Validity
import Data.Validity.Map
import GHC.Generics (Generic)
import Money.Account (Account (..), Rounding (..))
import qualified Money.Account as Account
import Money.Amount (Amount (..))
import Money.ConversionRate (ConversionRate)
import qualified Money.ConversionRate as ConversionRate
import Money.MultiAmount (Rounded (..))
import Money.QuantisationFactor (QuantisationFactor)
import Prelude hiding (subtract, sum)
import qualified Prelude

-- | A type for a combination of amounts of different currencies
--
-- This uses a 'currency' type parameter so that you can choose how to
-- represent the currencies that are being accounted for.
newtype MultiAccount currency = MultiAccount
  { unMultiAccount :: Map currency Account
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance (Validity currency, Show currency, Ord currency) => Validity (MultiAccount currency) where
  validate ma@(MultiAccount m) =
    mconcat
      [ genericValidate ma,
        decorateMap m $ \_ a ->
          declare "The account is not zero" $
            a /= Account.zero
      ]

-- TODO no empty currencies

instance (NFData currency) => NFData (MultiAccount currency)

fromAccount :: currency -> Account -> MultiAccount currency
fromAccount currency amount =
  if amount == Account.zero
    then zero
    else MultiAccount $ M.singleton currency amount

-- | No money of any currency
zero :: MultiAccount currency
zero = MultiAccount M.empty

-- | Add two 'MultiAccount's
add :: forall currency. (Ord currency) => MultiAccount currency -> MultiAccount currency -> Maybe (MultiAccount currency)
add m1 = foldM (\m (c, a) -> addAccount m c a) m1 . M.toList . unMultiAccount

-- | Add multiple 'MultiAccount's
sum :: (Foldable f, Ord currency) => f (MultiAccount currency) -> Maybe (MultiAccount currency)
sum = foldM add zero

-- | Subtract a 'MultiAccount' from a 'MultiAccount'
subtract :: forall currency. (Ord currency) => MultiAccount currency -> MultiAccount currency -> Maybe (MultiAccount currency)
subtract m1 = foldM (\m (c, a) -> subtractAccount m c a) m1 . M.toList . unMultiAccount

-- | Lookup up the amount of one currency in a 'MultiAccount'
lookupAccount :: (Ord currency) => currency -> MultiAccount currency -> Account
lookupAccount currency (MultiAccount m) = fromMaybe Account.zero $ M.lookup currency m

-- | Add an 'Amount' to a 'MultiAccount'
addAmount :: (Ord currency) => MultiAccount currency -> currency -> Amount -> Maybe (MultiAccount currency)
addAmount ma cur a = addAccount ma cur (Positive a)

-- | Subtract an 'Amount' from a 'MultiAccount'
subtractAmount :: (Ord currency) => MultiAccount currency -> currency -> Amount -> Maybe (MultiAccount currency)
subtractAmount ma cur a = subtractAccount ma cur (Positive a)

-- | Add an 'Account' to a 'MultiAccount'
addAccount :: (Ord currency) => MultiAccount currency -> currency -> Account -> Maybe (MultiAccount currency)
addAccount m _ (Positive (Amount 0)) = Just m
addAccount m _ (Negative (Amount 0)) = Just m
addAccount (MultiAccount m) currency account =
  fmap MultiAccount $ case M.lookup currency m of
    Nothing -> Just $ M.insert currency account m
    Just a -> do
      r <- Account.add a account
      Just $
        if r == Account.zero
          then M.delete currency m
          else M.insert currency r m

-- | Add an 'Account' to a 'MultiAccount'
subtractAccount :: (Ord currency) => MultiAccount currency -> currency -> Account -> Maybe (MultiAccount currency)
subtractAccount m _ (Positive (Amount 0)) = Just m
subtractAccount m _ (Negative (Amount 0)) = Just m
subtractAccount (MultiAccount m) currency account =
  fmap MultiAccount $ case M.lookup currency m of
    Nothing -> Just $ M.insert currency (Account.negate account) m
    Just a -> do
      r <- Account.add a account
      Just $
        if r == Account.zero
          then M.delete currency m
          else M.insert currency r m

-- | Try to convert every account to one currency.
--
-- This function can be more precise than calling 'Amount.convert' on each
-- account and then adding the results together because rounding only happens once.
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
  MultiAccount currency ->
  -- | The actual result and which rounding happened
  (Maybe Account, Rounded)
convertAll r qf1 func = runIdentity . convertAllA r qf1 (Identity . func)

-- | Like 'convertAll', but you can decide to convert in your own Applicative.
convertAllA ::
  (Applicative f) =>
  -- Where to round the *result*
  Rounding ->
  -- The quantisation factor of the currency to convert to
  QuantisationFactor ->
  -- The conversion rate and quantisation factor of each currency to convert from
  (currency -> f (ConversionRate, QuantisationFactor)) ->
  -- The 'MultiAccount' to convert
  MultiAccount currency ->
  -- | The actual result and which rounding happened
  f (Maybe Account, Rounded)
convertAllA r qf1 func =
  fmap
    ( ( \theoreticalResult ->
          let rounder :: Rational -> Integer
              rounder = case r of
                RoundUp -> ceiling
                RoundDown -> floor
                RoundNearest -> round
              roundedResult :: Integer
              roundedResult = rounder theoreticalResult
              rounded = case compare (fromIntegral roundedResult) theoreticalResult of
                LT -> RoundedDown
                EQ -> DidNotRound
                GT -> RoundedUp
           in (Account.fromMinimalQuantisations roundedResult, rounded)
      )
        . Prelude.sum
    )
    . traverse
      ( \(currency, a) ->
          ( \(cr, qf2) ->
              let factor = ConversionRate.conversionFactor qf2 cr qf1
               in Account.toMinimalQuantisations a
                    * toInteger (numerator factor)
                    % toInteger (denominator factor)
          )
            <$> func currency
      )
    . M.toList
    . unMultiAccount
