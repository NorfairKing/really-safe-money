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
import Money.Currency (Currency (..))

-- | A type for a combination of amounts of different currencies
newtype MultiAmount = MultiAmount
  { unMultiAmount :: Map Currency Amount
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Validity MultiAmount

instance NFData MultiAmount

fromAmount :: Currency -> Amount -> MultiAmount
fromAmount currency amount = MultiAmount $ M.singleton currency amount

-- | No money of any currency
zero :: MultiAmount
zero = MultiAmount M.empty

-- | Add two 'MultiAmount's
add :: MultiAmount -> MultiAmount -> Maybe MultiAmount
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
