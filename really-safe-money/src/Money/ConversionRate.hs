{-# LANGUAGE DeriveGeneric #-}

module Money.ConversionRate
  ( ConversionRate (..),
    toRatio,
    fromRatio,
    Money.ConversionRate.fromRational,
    Money.ConversionRate.toRational,
    toDecimalLiteral,
    fromDecimalLiteral,
  )
where

import Control.DeepSeq
import Data.Ratio
import Data.Validity
import GHC.Generics (Generic)
import Numeric.DecimalLiteral (DecimalLiteral (..))
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Numeric.Natural
import Prelude hiding (fromRational, toRational)

-- | A conversion rate between two currencies.
--
-- This value must not be zero (or negative).
newtype ConversionRate = ConversionRate {unConversionRate :: Ratio Natural}
  deriving (Show, Eq, Generic)

instance Validity ConversionRate where
  validate cr@(ConversionRate r) =
    mconcat
      [ genericValidate cr,
        declare "The rate is nonzero" $ numerator r /= 0
      ]

instance NFData ConversionRate

-- | Turn a 'Ratio Natural' into a 'ConversionRate'
--
-- Fails if the ratio is zero
--
-- >>> fromRatio (1 % 1)
-- Just (ConversionRate {unConversionRate = 1 % 1})
-- >>> fromRatio (0 % 1)
-- Nothing
fromRatio :: Ratio Natural -> Maybe ConversionRate
fromRatio = constructValid . ConversionRate

-- | Turn a 'ConversionRate' back into a 'Ratio'
--
-- >>> toRatio (ConversionRate {unConversionRate = 1 % 2})
-- 1 % 2
toRatio :: ConversionRate -> Ratio Natural
toRatio = unConversionRate

-- | Turn a 'Rational' into a 'ConversionRate'
--
-- Fails if the ratio is zero or negative
--
-- >>> fromRatio (2 % 1)
-- Just (ConversionRate {unConversionRate = 2 % 1})
-- >>> fromRational ((-1) % 1)
-- Nothing
-- >>> fromRational (0 % 1)
-- Nothing
fromRational :: Rational -> Maybe ConversionRate
fromRational r = if r < 0 then Nothing else fromRatio (realToFrac r)

-- | Turn a 'ConversionRate' back into a 'Rational'
--
-- >>> toRational (ConversionRate {unConversionRate = 1 % 3})
-- 1 % 3
toRational :: ConversionRate -> Rational
toRational = realToFrac . toRatio

-- | Parse a 'DecimalLiteral' into a 'ConversionRate'.
--
-- This will fail if:
--
-- * The literal is zero
-- * The literal is negative
--
-- >>> fromDecimalLiteral (DecimalLiteral Nothing 5 1)
-- Just (ConversionRate {unConversionRate = 1 % 2})
-- >>> fromDecimalLiteral (DecimalLiteral (Just False) 1 2)
-- Nothing
-- >>> fromDecimalLiteral (DecimalLiteral Nothing 0 1)
-- Nothing
fromDecimalLiteral :: DecimalLiteral -> Maybe ConversionRate
fromDecimalLiteral = fromRational . DecimalLiteral.toRational

-- | Turn a 'ConversionRate' into a 'DecimalLiteral'
--
-- This will fail whenever 'DecimalLiteral.fromRational' would fail and will
-- never have a sign.
toDecimalLiteral :: ConversionRate -> Maybe DecimalLiteral
toDecimalLiteral =
  fmap DecimalLiteral.setSignOptional
    . DecimalLiteral.fromRational
    . toRational
