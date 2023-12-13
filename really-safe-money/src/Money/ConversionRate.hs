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
import Numeric.DecimalLiteral (DecimalLiteral)
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Numeric.Natural
import Prelude hiding (fromRational, toRational)

newtype ConversionRate = ConversionRate {unConversionRate :: Ratio Natural}
  deriving (Show, Eq, Generic)

instance Validity ConversionRate where
  validate cr@(ConversionRate r) =
    mconcat
      [ genericValidate cr,
        declare "The rate is nonzero" $ numerator r /= 0
      ]

instance NFData ConversionRate

fromRatio :: Ratio Natural -> Maybe ConversionRate
fromRatio = constructValid . ConversionRate

toRatio :: ConversionRate -> Ratio Natural
toRatio = unConversionRate

fromRational :: Rational -> Maybe ConversionRate
fromRational r = if r < 0 then Nothing else fromRatio (realToFrac r)

toRational :: ConversionRate -> Rational
toRational = realToFrac . toRatio

fromDecimalLiteral :: DecimalLiteral -> Maybe ConversionRate
fromDecimalLiteral = fromRational . DecimalLiteral.toRational

toDecimalLiteral :: ConversionRate -> Maybe DecimalLiteral
toDecimalLiteral =
  fmap DecimalLiteral.setSignOptional
    . DecimalLiteral.fromRational
    . toRational
