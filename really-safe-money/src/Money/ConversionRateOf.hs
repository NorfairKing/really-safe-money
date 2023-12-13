{-# LANGUAGE DeriveGeneric #-}

module Money.ConversionRateOf
  ( ConversionRateOf (..),
    toRatio,
    fromRatio,
    Money.ConversionRateOf.fromRational,
    Money.ConversionRateOf.toRational,
    toDecimalLiteral,
    fromDecimalLiteral,
  )
where

import Control.DeepSeq
import Data.Ratio
import Data.Validity
import GHC.Generics (Generic)
import Money.ConversionRate (ConversionRate (..))
import qualified Money.ConversionRate as ConversionRate
import Numeric.DecimalLiteral (DecimalLiteral (..))
import Numeric.Natural
import Prelude hiding (fromRational, toRational)

-- | A conversion rate to go from one currency to another.
--
-- This denotes the number of tos per from.
--
-- This value must not be zero (or negative).
newtype ConversionRateOf from to = ConversionRateOf {unConversionRateOf :: ConversionRate}
  deriving (Show, Eq, Generic)

instance Validity (ConversionRateOf from to)

instance NFData (ConversionRateOf from to)

-- | See 'ConversionRate.fromRatio'
fromRatio :: Ratio Natural -> Maybe (ConversionRateOf from to)
fromRatio = fmap ConversionRateOf . ConversionRate.fromRatio

-- | See 'ConversionRate.toRatio'
toRatio :: ConversionRateOf from to -> Ratio Natural
toRatio = ConversionRate.toRatio . unConversionRateOf

-- | See 'ConversionRate.fromRational'
fromRational :: Rational -> Maybe (ConversionRateOf from to)
fromRational = fmap ConversionRateOf . ConversionRate.fromRational

-- | See 'ConversionRate.toRational'
toRational :: ConversionRateOf from to -> Rational
toRational = ConversionRate.toRational . unConversionRateOf

-- | See 'ConversionRate.fromDecimalLiteral'
fromDecimalLiteral :: DecimalLiteral -> Maybe (ConversionRateOf from to)
fromDecimalLiteral = fmap ConversionRateOf . ConversionRate.fromDecimalLiteral

-- | See 'ConversionRate.toDecimalLiteral'
toDecimalLiteral :: ConversionRateOf from to -> Maybe DecimalLiteral
toDecimalLiteral = ConversionRate.toDecimalLiteral . unConversionRateOf
