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
  )
where

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Map
import GHC.Generics (Generic)
import Money.Amount (Amount)
import qualified Money.Amount as Amount
import Prelude hiding (sum)

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
add (MultiAmount m1) (MultiAmount m2) =
  fmap MultiAmount $ foldM go m1 $ M.toList m2
  where
    go ::
      Map currency Amount ->
      (currency, Amount) ->
      Maybe (Map currency Amount)
    go m (currency, amount) = case M.lookup currency m of
      Nothing -> Just $ M.insert currency amount m
      Just a -> do
        r <- Amount.add amount a
        Just $
          if r == Amount.zero
            then M.delete currency m
            else M.insert currency r m

-- | Add multiple 'MultiAmount's
sum :: (Foldable f, Ord currency) => f (MultiAmount currency) -> Maybe (MultiAmount currency)
sum = foldM add zero
