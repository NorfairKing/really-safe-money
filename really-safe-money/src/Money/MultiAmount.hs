{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
  )
where

import Control.DeepSeq
import Data.Data
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Map ()
import GHC.Generics (Generic)
import Money.Amount (Amount)
import qualified Money.Amount as Amount

-- | A type for a combination of amounts of different currencies
--
-- This uses a 'currency' type parameter so that you can choose how to
-- represent the currencies that are being accounted for.
newtype MultiAmount currency = MultiAmount
  { unMultiAmount :: Map currency Amount
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance (Validity currency, Show currency, Ord currency) => Validity (MultiAmount currency)

instance NFData currency => NFData (MultiAmount currency)

fromAmount :: currency -> Amount -> MultiAmount currency
fromAmount currency amount = MultiAmount $ M.singleton currency amount

-- | No money of any currency
zero :: MultiAmount currency
zero = MultiAmount M.empty

-- | Add two 'MultiAmount's
add :: Ord currency => MultiAmount currency -> MultiAmount currency -> Maybe (MultiAmount currency)
add (MultiAmount m1) (MultiAmount m2) =
  MultiAmount
    <$> sequenceA
      ( M.unionWith
          ( \ma1 ma2 -> do
              a1 <- ma1
              a2 <- ma2
              Amount.add a1 a2
          )
          (M.map Just m1)
          (M.map Just m2)
      )
