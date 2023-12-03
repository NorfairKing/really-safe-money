{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Numeric.DecimalLiteral
  ( DecimalLiteral (..),
    parseDecimalLiteralM,
    parseDecimalLiteral,
    renderDecimalLiteral,
    toRational,
    fromRational,
    toQuantisationFactor,
    fromQuantisationFactor,
    toAccount,
    fromAccount,
    quantisationFactorDigits,
  )
where

import Control.DeepSeq
import qualified Data.Char as Char
import Data.List (find)
import Data.Ratio
import Data.Validity
import Data.Validity.Scientific ()
import Data.Word
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.QuantisationFactor
import Numeric.Natural
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import qualified Text.ParserCombinators.ReadP as ReadP
import Prelude hiding (fromRational, toRational)

-- | Decimal literal
--
--
-- Note on representation
--
-- * Why not some combination of @Scientific@ and representation fields?
--   Because then there are multiple representations of negative numbers.
-- * Why not only a @DecimalLiteralFractional (Maybe Bool) Natural Int8@?
--   Because then there are multiple representations of "10": (Nothing, 10, 0) and (Nothing, 1, 1)
data DecimalLiteral
  = -- | Integral decimal literal
    DecimalLiteralInteger
      !(Maybe Bool)
      -- ^ Sign
      !Natural
      -- ^ Absolute value
  | -- | Fractional decimal literal
    --
    -- Using a and e to represent
    -- m * 10 ^(-(e + 1))
    DecimalLiteralFractional
      !(Maybe Bool)
      -- ^ Sign
      !Natural
      -- ^ m
      !Word8
      -- ^ e
  deriving (Show, Eq, Generic)

instance Validity DecimalLiteral

instance NFData DecimalLiteral

parseDecimalLiteralM :: MonadFail m => String -> m DecimalLiteral
parseDecimalLiteralM s = case parseDecimalLiteral s of
  Nothing -> fail $ "Failed to parse decimal literal from:" <> show s
  Just dl -> pure dl

parseDecimalLiteral :: String -> Maybe DecimalLiteral
parseDecimalLiteral = fmap fst . find (null . snd) . readP_to_S decimalLiteralP

decimalLiteralP :: ReadP DecimalLiteral
decimalLiteralP = do
  let isSignChar :: Char -> Bool
      isSignChar c = c == '-' || c == '+'

  mSign <- ReadP.option Nothing $ do
    signChar <- ReadP.satisfy isSignChar
    pure $ Just $ signChar == '+'

  units <- parseDigits step 0

  ReadP.option (DecimalLiteralInteger mSign units) $ do
    _ <- ReadP.satisfy (== '.')

    (m, e) <- parseDigits stepFraction (units, 0)

    pure $ DecimalLiteralFractional mSign m (pred e)

stepFraction :: (Natural, Word8) -> Int -> (Natural, Word8)
stepFraction (m, e) digit = (m * 10 + fromIntegral digit, succ e)

step :: Natural -> Int -> Natural
step a digit = a * 10 + fromIntegral digit
{-# INLINE step #-}

parseDigits :: (a -> Int -> a) -> a -> ReadP a
parseDigits f z = do
  c <- ReadP.satisfy Char.isDigit
  let digit = Char.ord c - 48
      a = f z digit

  ReadP.look >>= go a
  where
    go !a [] = return a
    go !a (c : cs)
      | Char.isDigit c = do
          _ <- ReadP.get
          let digit = Char.ord c - 48
          go (f a digit) cs
      | otherwise = return a

renderDecimalLiteral :: DecimalLiteral -> String
renderDecimalLiteral = \case
  DecimalLiteralInteger s a -> sign s ++ show a
  DecimalLiteralFractional s m e -> sign s ++ go m e
  where
    sign = \case
      Nothing -> ""
      Just True -> "+"
      Just False -> "-"
    go m _ = show m -- TODO

fromRational :: Rational -> Maybe DecimalLiteral
fromRational = undefined

toRational :: DecimalLiteral -> Rational
toRational = undefined

toQuantisationFactor :: DecimalLiteral -> Maybe QuantisationFactor
toQuantisationFactor dl = do
  irat <-
    let r = toRational dl
     in if numerator r == 0
          then Nothing
          else pure r

  rat <-
    let r = 1 / irat
     in if r < 0
          then Nothing
          else Just r

  fac <-
    if denominator rat == 1
      then Just (numerator rat)
      else Nothing

  if fac <= fromIntegral (maxBound :: Word32)
    then Just (QuantisationFactor (fromIntegral fac))
    else Nothing

fromQuantisationFactor :: QuantisationFactor -> Maybe DecimalLiteral
fromQuantisationFactor (QuantisationFactor qfw) =
  let r = 1 % fromIntegral qfw
   in do
        _ <- fromRational r
        -- TODO set quantisationFactorDigits
        -- DecimalLiteral False (quantisationFactorDigits qf) s
        undefined

toAccount :: QuantisationFactor -> DecimalLiteral -> Maybe Money.Account
toAccount qf = Account.fromRational qf . toRational

fromAccount :: QuantisationFactor -> Money.Account -> Maybe DecimalLiteral
fromAccount qf acc =
  let r = Account.toRational qf acc
   in do
        _ <- fromRational r
        -- TODO set quantisationFactorDigits
        -- DecimalLiteral False (quantisationFactorDigits qf) s
        undefined

quantisationFactorDigits :: QuantisationFactor -> Word8
quantisationFactorDigits qf = ceiling (logBase 10 $ fromIntegral $ unQuantisationFactor qf :: Float)
