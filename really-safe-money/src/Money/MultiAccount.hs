{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
  )
where

import Control.DeepSeq
import Data.Data
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Map ()
import GHC.Generics (Generic)
import Money.Account (Account)
import qualified Money.Account as Account

-- | A type for a combination of amounts of different currencies
--
-- This uses a 'currency' type parameter so that you can choose how to
-- represent the currencies that are being accounted for.
newtype MultiAccount currency = MultiAccount
  { unMultiAccount :: Map currency Account
  }
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance (Validity currency, Show currency, Ord currency) => Validity (MultiAccount currency)

instance NFData currency => NFData (MultiAccount currency)

fromAccount :: currency -> Account -> MultiAccount currency
fromAccount currency amount = MultiAccount $ M.singleton currency amount

-- | No money of any currency
zero :: MultiAccount currency
zero = MultiAccount M.empty

-- | Add two 'MultiAccount's
add :: Ord currency => MultiAccount currency -> MultiAccount currency -> Maybe (MultiAccount currency)
add (MultiAccount m1) (MultiAccount m2) =
  MultiAccount
    <$> sequenceA
      ( M.unionWith
          ( \ma1 ma2 -> do
              a1 <- ma1
              a2 <- ma2
              Account.add a1 a2
          )
          (M.map Just m1)
          (M.map Just m2)
      )
