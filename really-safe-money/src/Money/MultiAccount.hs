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
import Money.Account (Account)
import qualified Money.Account as Account
import Prelude hiding (sum)

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

instance NFData currency => NFData (MultiAccount currency)

fromAccount :: currency -> Account -> MultiAccount currency
fromAccount currency amount =
  if amount == Account.zero
    then zero
    else MultiAccount $ M.singleton currency amount

-- | No money of any currency
zero :: MultiAccount currency
zero = MultiAccount M.empty

-- | Add two 'MultiAccount's
add :: forall currency. Ord currency => MultiAccount currency -> MultiAccount currency -> Maybe (MultiAccount currency)
add (MultiAccount m1) (MultiAccount m2) =
  fmap MultiAccount $ foldM go m1 $ M.toList m2
  where
    go ::
      Map currency Account ->
      (currency, Account) ->
      Maybe (Map currency Account)
    go m (currency, amount) = case M.lookup currency m of
      Nothing -> Just $ M.insert currency amount m
      Just a -> do
        r <- Account.add amount a
        Just $
          if r == Account.zero
            then M.delete currency m
            else M.insert currency r m

-- | Add multiple 'MultiAccount's
sum :: (Foldable f, Ord currency) => f (MultiAccount currency) -> Maybe (MultiAccount currency)
sum = foldM add zero