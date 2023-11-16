{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Money.QuantisationFactor where

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
