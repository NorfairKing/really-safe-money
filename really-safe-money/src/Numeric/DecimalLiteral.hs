{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Numeric.DecimalLiteral
  ( DecimalLiteral (..),
    renderDecimalLiteral,
    parseDecimalLiteralM,
    parseDecimalLiteral,
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

data DecimalLiteral = DecimalLiteral
  { -- Whether a sign should be present when rendering a positive number.
    decimalLiteralSign :: !(Maybe Bool),
    decimalLiteralUnits :: !Natural,
    decimalLiteralLeadingZeroes :: !Word8,
    decimalLiteralDecimals :: !(Maybe Natural)
  }
  deriving (Show, Eq, Generic)

instance Validity DecimalLiteral

instance NFData DecimalLiteral

renderDecimalLiteral :: DecimalLiteral -> String
renderDecimalLiteral DecimalLiteral {..} =
  concat
    [ case decimalLiteralSign of
        Nothing -> ""
        Just True -> "+"
        Just False -> "-",
      show decimalLiteralUnits,
      replicate (fromIntegral decimalLiteralLeadingZeroes) '0',
      maybe "" show decimalLiteralDecimals
    ]

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

  decimalLiteralSign <- ReadP.option Nothing $ do
    signChar <- ReadP.satisfy isSignChar
    pure $ Just $ signChar == '+'

  let step :: Natural -> Int -> Natural
      step a digit = a * 10 + fromIntegral digit
      {-# INLINE step #-}

  decimalLiteralUnits <- foldDigits step 0

  (decimalLiteralLeadingZeroes, decimalLiteralDecimals) <- ReadP.option (0, Nothing) $ do
    _ <- ReadP.satisfy (== '.')
    leadingZeroes <- fromIntegral . length <$> ReadP.many (ReadP.satisfy (== '0'))
    decimals <- foldDigits step 0
    pure (leadingZeroes, Just decimals)

  pure DecimalLiteral {..}

foldDigits :: (a -> Int -> a) -> a -> ReadP a
foldDigits f z = do
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
