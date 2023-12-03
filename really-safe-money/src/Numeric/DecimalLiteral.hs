{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Numeric.DecimalLiteral
  ( DecimalLiteral (..),
    renderDecimalLiteral,
    parseDecimalLiteralM,
    parseDecimalLiteral,
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
import Data.Scientific
import Data.Validity
import Data.Validity.Scientific ()
import Data.Word
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.QuantisationFactor
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import qualified Text.ParserCombinators.ReadP as ReadP

data DecimalLiteral = DecimalLiteral
  { -- Whether a sign should be present when rendering a positive number.
    decimalLiteralSign :: !Bool,
    decimalLiteralDigits :: !Word8, -- (Minimum) number of digits after the dot.
    decimalLiteralScientific :: !Scientific
  }
  deriving (Show, Eq, Generic)

instance Validity DecimalLiteral where
  validate dl@(DecimalLiteral _ _ s) =
    mconcat
      [ genericValidate dl,
        declare "The scientific is small in absolute value" $ base10Exponent s < 128
      ]

instance NFData DecimalLiteral

renderDecimalLiteral :: DecimalLiteral -> String
renderDecimalLiteral (DecimalLiteral useSign digits s) =
  (if s >= 0 && useSign then ('+' :) else id) $
    let d = max (fromIntegral digits) (-(base10Exponent s))
     in formatScientific Fixed (Just d) s

parseDecimalLiteralM :: MonadFail m => String -> m DecimalLiteral
parseDecimalLiteralM s = case parseDecimalLiteral s of
  Nothing -> fail $ "Failed to parse decimal literal from:" <> show s
  Just dl -> pure dl

parseDecimalLiteral :: String -> Maybe DecimalLiteral
parseDecimalLiteral = fmap fst . find (null . snd) . readP_to_S decimalLiteralP

decimalLiteralP :: ReadP DecimalLiteral
decimalLiteralP = do
  (useSign, pos) <- ReadP.option (False, True) $ do
    signChar <- ReadP.satisfy isSign
    pure (True, signChar == '+')

  let step :: Integer -> Int -> Integer
      step a digit = a * 10 + fromIntegral digit
      {-# INLINE step #-}

  n <- foldDigits step 0

  let s = SP n 0 0
      fractional =
        foldDigits
          ( \(SP a e w) digit ->
              SP (step a digit) (pred e) (succ w)
          )
          s

  SP coeff expnt digits <-
    (ReadP.satisfy (== '.') >> fractional)
      ReadP.<++ return s

  let signedCoeff
        | pos = coeff
        | otherwise = (-coeff)

  return $ DecimalLiteral useSign digits $ scientific signedCoeff expnt

-- A strict pair
data SP = SP !Integer {-# UNPACK #-} !Int !Word8

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

isSign :: Char -> Bool
isSign c = c == '-' || c == '+'
{-# INLINE isSign #-}

decimalLiteralToRational :: DecimalLiteral -> Rational
decimalLiteralToRational = toRational . decimalLiteralScientific -- This is safe because we use small decimal literals

toQuantisationFactor :: DecimalLiteral -> Maybe QuantisationFactor
toQuantisationFactor dl = do
  irat <-
    let r = decimalLiteralToRational dl
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
fromQuantisationFactor qf@(QuantisationFactor qfw) =
  let r = 1 % fromIntegral qfw
   in -- We set a limit for safety reasons.
      case fromRationalRepetend (Just 128) r of
        Left _ -> Nothing
        Right (s, Nothing) -> Just $ DecimalLiteral False (quantisationFactorDigits qf) s
        Right (_, Just _) -> Nothing

toAccount :: QuantisationFactor -> DecimalLiteral -> Maybe Money.Account
toAccount qf dl = Account.fromRational qf $ decimalLiteralToRational dl

fromAccount :: QuantisationFactor -> Money.Account -> Maybe DecimalLiteral
fromAccount qf acc =
  let r = Account.toRational qf acc
   in -- We set a limit for safety reasons.
      case fromRationalRepetend (Just 128) r of
        Left _ -> Nothing
        Right (s, Nothing) -> Just $ DecimalLiteral True (quantisationFactorDigits qf) s
        Right (_, Just _) -> Nothing

quantisationFactorDigits :: QuantisationFactor -> Word8
quantisationFactorDigits qf = ceiling (logBase 10 $ fromIntegral $ unQuantisationFactor qf :: Float)
