{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- === Importing this module
--
-- This module is designed to be imported as follows:
--
-- @
-- import Money.QuantisationFactor (QuantisationFactor)
-- import qualified Money.QuantisationFactor as QuantisationFactor
-- @
module Money.QuantisationFactor
  ( QuantisationFactor (..),
    fromWord32,
  )
where

import Control.DeepSeq
import Data.Data
import Data.Validity
import Data.Word
import GHC.Generics (Generic)

-- | A quantisation factor.
--
-- This is a newtype because it must not be 0.
newtype QuantisationFactor = QuantisationFactor {unQuantisationFactor :: Word32}
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Validity QuantisationFactor where
  validate qf@(QuantisationFactor w) =
    mconcat
      [ genericValidate qf,
        declare "The quantisation factor is not zero" $ w /= 0
      ]

instance NFData QuantisationFactor

fromWord32 :: Word32 -> Maybe QuantisationFactor
fromWord32 = constructValid . QuantisationFactor
