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
    digits,
    fromDecimalLiteral,
    toDecimalLiteral,
  )
where

import Control.DeepSeq
import Data.Data
import Data.Ratio
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Numeric.DecimalLiteral (DecimalLiteral (..))
import qualified Numeric.DecimalLiteral as DecimalLiteral

-- | A quantisation factor.
--
-- This is a newtype because it must not be 0.
newtype QuantisationFactor = QuantisationFactor {unQuantisationFactor :: Word32}
  deriving (Show, Read, Eq, Ord, Data, Generic)

instance Validity QuantisationFactor where
  validate (QuantisationFactor w) =
    declare "The quantisation factor is not zero" $ w /= 0

instance NFData QuantisationFactor

fromWord32 :: Word32 -> Maybe QuantisationFactor
fromWord32 = constructValid . QuantisationFactor

digits :: QuantisationFactor -> Word8
digits qf = ceiling (logBase 10 $ fromIntegral $ unQuantisationFactor qf :: Float)

-- | Render a 'DecimalLiteral' that represents the smallest unit from a 'QuantisationFactor'
--
-- Note that this fails on quantisation factors that cannot be represented
-- using a literal, for example because they would correspond to a number with
-- an infinite decimal representation.
--
-- this will always have a 'Nothing' sign.
--
-- >>> toDecimalLiteral (QuantisationFactor 100)
-- Just (DecimalLiteral Nothing 1 2)
-- >>> toDecimalLiteral (QuantisationFactor 20)
-- Just (DecimalLiteral Nothing 5 2)
-- >>> toDecimalLiteral (QuantisationFactor 1)
-- Just (DecimalLiteral Nothing 1 0)
toDecimalLiteral :: QuantisationFactor -> Maybe DecimalLiteral
toDecimalLiteral (QuantisationFactor qfw) =
  DecimalLiteral.setSignOptional <$> DecimalLiteral.fromRational (1 % fromIntegral qfw)

-- | Parse a 'QuantisationFactor' from a 'DecimalLiteral' that represents the smallest unit
-- TODO explain that it's the inverse.
--
-- Note that this fails on:
--
-- * Negative literals
-- * Integrals greater than 1
--
-- >>> fromDecimalLiteral (DecimalLiteral Nothing 2 0)
-- Nothing
-- >>> fromDecimalLiteral (DecimalLiteral (Just False) 2 2)
-- Nothing
-- >>> fromDecimalLiteral (DecimalLiteral (Just True) 2 2)
-- Just (QuantisationFactor {unQuantisationFactor = 50})
--
-- [check] If you change this function, re-verify that the @EQ@ branch of the
-- @case compare fac maxBound@ below is still unreachable, so the @DisableMutations@
-- annotation is still justified.
--
-- The @EQ@ branch is unreachable: a decimal literal's @toRational@ has the
-- form @m / 10^e@, so @fac = 10^e / m@ has only 2 and 5 as prime factors,
-- but @maxBound :: Word32 = 4294967295 = 3 * 5 * 17 * 257 * 65537@. No input
-- can hit the boundary, so we disable the @RemoveCase@ and @MaybeOp@
-- mutations on that branch.
{-# ANN fromDecimalLiteral ("DisableMutations: RemoveCase, MaybeOp" :: String) #-}
fromDecimalLiteral :: DecimalLiteral -> Maybe QuantisationFactor
fromDecimalLiteral dl = do
  irat <-
    let r = DecimalLiteral.toRational dl
     in if r <= 0
          then Nothing
          else pure r

  let rat = 1 / irat

  fac <-
    if denominator rat == 1
      then Just (numerator rat)
      else Nothing

  case compare fac (fromIntegral (maxBound :: Word32)) of
    GT -> Nothing
    EQ -> Just (QuantisationFactor (fromIntegral fac))
    LT -> Just (QuantisationFactor (fromIntegral fac))
