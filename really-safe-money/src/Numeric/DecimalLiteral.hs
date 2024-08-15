{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Decimal Literal
--
-- === Importing this module
--
-- This module is designed to be imported as follows:
--
-- @
-- import Numeric.DecimalLiteral (DecimalLiteral)
-- import qualified Numeric.DecimalLiteral as DecimalLiteral
-- @
module Numeric.DecimalLiteral
  ( DecimalLiteral (..),
    Numeric.DecimalLiteral.fromString,
    fromStringM,
    format,
    fromRational,
    toRational,
    fromRatio,
    toRatio,
    fromWord,
    toWord,
    fromNatural,
    toNatural,
    fromInt,
    toInt,
    fromInteger,
    toInteger,
    setSignRequired,
    setSignOptional,
    digits,
    setMinimumDigits,
  )
where

import Control.DeepSeq
import Control.Monad
import qualified Data.Char as Char
import Data.List (find)
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString (..))
import Data.Validity
import Data.Validity.Scientific ()
import Data.Word
import GHC.Generics (Generic)
import GHC.Real (Ratio (..))
import Numeric.Natural
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import qualified Text.ParserCombinators.ReadP as ReadP
import Prelude hiding (fromInteger, fromRational, toInteger, toRational)
import qualified Prelude (toInteger)

-- | Decimal literal
--
--
-- Note on representation
--
-- * Why not some combination of @Scientific@ and representation fields?
--   Because then there are multiple representations of negative numbers.
-- * Why not only a @DecimalLiteral (Maybe Bool) Natural Int8@?
--   Because then there are multiple representations of "10": (Nothing, 10, 0) and (Nothing, 1, 1)
--
-- Fractional decimal literal
--
-- Using a and e to represent
-- sign * m * 10 ^(-e)
data DecimalLiteral
  = DecimalLiteral
      !(Maybe Bool)
      !Natural
      !Word8
  deriving (Show, Eq, Generic)

instance Validity DecimalLiteral

instance NFData DecimalLiteral

instance IsString DecimalLiteral where
  fromString s = case Numeric.DecimalLiteral.fromString s of
    Nothing -> error $ "Invalid DecimalLiteral: " <> show s
    Just dl -> dl

-- | Parse a decimal literal from a string
--
-- This only accepts non-scientific notation.
-- It also fails if the exponent would get too big.
--
-- >>> Numeric.DecimalLiteral.fromString "1"
-- Just (DecimalLiteral Nothing 1 0)
-- >>> Numeric.DecimalLiteral.fromString "0.02"
-- Just (DecimalLiteral Nothing 2 2)
-- >>> Numeric.DecimalLiteral.fromString "+0.00003"
-- Just (DecimalLiteral (Just True) 3 5)
-- >>> Numeric.DecimalLiteral.fromString "-0.00000004"
-- Just (DecimalLiteral (Just False) 4 8)
-- >>> Numeric.DecimalLiteral.fromString ("0." ++ replicate 100 '0')
-- Just (DecimalLiteral Nothing 0 100)
-- >>> Numeric.DecimalLiteral.fromString ("0." ++ replicate 300 '0')
-- Nothing
fromString :: String -> Maybe DecimalLiteral
fromString = fmap fst . find (null . snd) . readP_to_S decimalLiteralP

-- | Like 'fromString' but in a 'MonadFail'
fromStringM :: (MonadFail m) => String -> m DecimalLiteral
fromStringM s = case Numeric.DecimalLiteral.fromString s of
  Nothing -> fail $ "Failed to parse decimal literal from: " <> show s
  Just dl -> pure dl

decimalLiteralP :: ReadP DecimalLiteral
decimalLiteralP = do
  let isSignChar :: Char -> Bool
      isSignChar c = c == '-' || c == '+'

  mSign <- ReadP.option Nothing $ do
    signChar <- ReadP.satisfy isSignChar
    pure $ Just $ signChar == '+'

  units <- parseDigits step 0

  ReadP.option (DecimalLiteral mSign units 0) $ do
    _ <- ReadP.satisfy (== '.')

    (m, e) <- parseDigits stepFraction (units, 0)

    pure $ DecimalLiteral mSign m e

stepFraction :: (Natural, Word8) -> Int -> Maybe (Natural, Word8)
stepFraction (_, 255) _ = Nothing
stepFraction (m, e) digit = Just (m * 10 + fromIntegral digit, succ e)

step :: Natural -> Int -> Maybe Natural
step a digit = Just $ a * 10 + fromIntegral digit
{-# INLINE step #-}

parseDigits :: (a -> Int -> Maybe a) -> a -> ReadP a
parseDigits f z = do
  c <- ReadP.satisfy Char.isDigit
  let digit = Char.ord c - 48
  case f z digit of
    Nothing -> fail "Failed to step the first digit"
    Just a -> ReadP.look >>= go a
  where
    go !a [] = return a
    go !a (c : cs)
      | Char.isDigit c = do
          _ <- ReadP.get
          let digit = Char.ord c - 48
          case f a digit of
            Nothing -> fail "Failed to step the digit"
            Just a' -> go a' cs
      | otherwise = return a

-- | Render a decimal literal to a string
--
-- >>> format (DecimalLiteral Nothing 5 0)
-- "5"
-- >>> format (DecimalLiteral (Just True) 60 0)
-- "+60"
-- >>> format (DecimalLiteral (Just False) 700 0)
-- "-700"
-- >>> format (DecimalLiteral Nothing 8 3)
-- "0.008"
-- >>> format (DecimalLiteral (Just True) 90 5)
-- "+0.00090"
-- >>> format (DecimalLiteral (Just False) 100 7)
-- "-0.0000100"
format :: DecimalLiteral -> String
format = \case
  DecimalLiteral s m 0 -> sign s ++ show m
  DecimalLiteral s m e -> sign s ++ goFrac m e
  where
    sign = \case
      Nothing -> ""
      Just True -> "+"
      Just False -> "-"

    goFrac m e = reverse (go e (reverse (show m)))
    go :: Word8 -> String -> String
    go 0 [] = ['.', '0']
    go 0 s = '.' : s
    go e [] = '0' : go (pred e) []
    go e (c : cs) = c : go (pred e) cs

-- | Parse a 'DecimalLiteral' from a 'Rational'
--
-- Because a 'Rational' contains no rendering information, we fill in these details:
--
-- * Use "No sign" for positive numbers
-- * Use the minimum number of required digits.
--
-- This fails when the rational cannot be represented finitely.
--
-- >>> fromRational (1 % 1)
-- Just (DecimalLiteral Nothing 1 0)
-- >>> fromRational ((-1) % 2)
-- Just (DecimalLiteral (Just False) 5 1)
-- >>> fromRational (1 % 3)
-- Nothing
fromRational :: Rational -> Maybe DecimalLiteral
fromRational (n :% d)
  | n < 0 = (\(DecimalLiteral _ m e) -> DecimalLiteral (Just False) m e) <$> fromRatio (fromIntegral (abs n) % fromIntegral d)
  | otherwise = fromRatio (fromIntegral n % fromIntegral d)

-- | Turn a 'DecimalLiteral' into a 'Rational'
--
-- This throws away all rendering information.
--
-- >>> toRational (DecimalLiteral Nothing 1 0)
-- 1 % 1
-- >>> toRational (DecimalLiteral (Just True) 2 1)
-- 1 % 5
-- >>> toRational (DecimalLiteral (Just False) 3 1)
-- (-3) % 10
toRational :: DecimalLiteral -> Rational
toRational (DecimalLiteral mSign m e) =
  signSignum mSign (fromIntegral m / (10 ^ e))

-- | Parse a 'DecimalLiteral' from a 'Ratio Natural'
--
-- Because a 'Ratio Natural' contains no rendering information, we fill in the number of required digits.
--
-- This fails when the rational cannot be represented finitely.
--
-- >>> fromRatio (1 % 1)
-- Just (DecimalLiteral Nothing 1 0)
-- >>> fromRatio (1 % 3)
-- Nothing
fromRatio :: Ratio Natural -> Maybe DecimalLiteral
fromRatio = fromRationalRepetendLimited 256
  where
    fromRationalRepetendLimited ::
      -- limit
      Int ->
      Ratio Natural ->
      Maybe DecimalLiteral
    fromRationalRepetendLimited l rational
      | d == 0 = Nothing
      | otherwise = toLiteral Nothing <$> longDiv num
      where
        toLiteral mSign (m, e) = DecimalLiteral mSign m (fromIntegral e)
        d = denominator rational
        num = numerator rational

        longDiv :: Natural -> Maybe (Natural, Int)
        longDiv = longDivWithLimit 0 0 S.empty

        longDivWithLimit ::
          Natural ->
          Int ->
          Set Natural ->
          Natural ->
          Maybe (Natural, Int)
        longDivWithLimit !c !e _ns 0 =
          Just (c, e)
        longDivWithLimit !c !e ns !n
          -- If there's a repetend, we can't turn it into a decimal literal
          | S.member n ns = Nothing
          -- Over the limit, stop trying
          | e >= l = Nothing
          | n < d =
              let !ns' = S.insert n ns
               in longDivWithLimit (c * 10) (succ e) ns' (n * 10)
          | otherwise =
              let (q, r') = n `quotRem` d
               in longDivWithLimit (c + q) e ns r'

-- | Turn a 'DecimalLiteral' into a 'Ratio Natural'
--
-- This throws away all rendering information.
--
-- Note that this will fail if the decimal literal is negative.
--
-- >>> toRatio (DecimalLiteral Nothing 1 0)
-- Just (1 % 1)
-- >>> toRatio (DecimalLiteral (Just True) 2 1)
-- Just (1 % 5)
-- >>> toRatio (DecimalLiteral (Just False) 3 1)
-- Nothing
toRatio :: DecimalLiteral -> Maybe (Ratio Natural)
toRatio (DecimalLiteral mSign m e) = case mSign of
  Just False -> Nothing
  _ -> Just $ fromIntegral m / (10 ^ e)

-- | Construct a 'DecimalLiteral' from a 'Word'
fromWord :: Word -> DecimalLiteral
fromWord = fromNatural . fromIntegral

-- | Turn a 'DecimalLiteral' into a 'Word'
toWord :: DecimalLiteral -> Maybe Word
toWord dl = do
  n <- toNatural dl
  guard $ n <= fromIntegral (maxBound :: Word)
  pure $ fromIntegral n

-- | Construct a 'DecimalLiteral' from a 'Natural'
fromNatural :: Natural -> DecimalLiteral
fromNatural n = DecimalLiteral Nothing n 0

-- | Turn a 'DecimalLiteral' into a 'Natural'
toNatural :: DecimalLiteral -> Maybe Natural
toNatural = \case
  DecimalLiteral (Just False) _ _ -> Nothing
  DecimalLiteral _ n 0 -> Just n
  _ -> Nothing

-- | Construct a 'DecimalLiteral' from an 'Integer'
fromInteger :: Integer -> DecimalLiteral
fromInteger n = DecimalLiteral (numSign n) (fromIntegral (abs n)) 0

-- | Turn a 'DecimalLiteral' into an 'Integer'
toInteger :: DecimalLiteral -> Maybe Integer
toInteger = \case
  DecimalLiteral mSign n 0 ->
    Just $ signSignum mSign (Prelude.toInteger n)
  _ -> Nothing

-- | Construct a 'DecimalLiteral' from an 'Int'
fromInt :: Int -> DecimalLiteral
fromInt = fromInteger . Prelude.toInteger

-- | Turn a 'DecimalLiteral' into an 'Int'
toInt :: DecimalLiteral -> Maybe Int
toInt dl = do
  n <- toInteger dl
  guard $ n <= fromIntegral (maxBound :: Int)
  guard $ n >= fromIntegral (minBound :: Int)
  pure $ fromIntegral n

numSign :: (Ord a, Num a) => a -> Maybe Bool
numSign a = if a >= 0 then Nothing else Just False

signSignum :: (Num a) => Maybe Bool -> (a -> a)
signSignum = \case
  Nothing -> id
  Just True -> id
  Just False -> negate

-- | Count how many digits the literal has after the decimal point
--
-- >>> digits (DecimalLiteral Nothing 1 0)
-- 0
-- >>> digits (DecimalLiteral Nothing 100 2)
-- 2
-- >>> digits (DecimalLiteral Nothing 1 2)
-- 2
digits :: DecimalLiteral -> Word8
digits (DecimalLiteral _ _ w) = w

-- | Set the minimum number of digits the literal has after the decimal point
--
-- Note that this function never decreases the number of digits after the decimal point.
--
-- >>> setMinimumDigits 2 (DecimalLiteral Nothing 1 0)
-- DecimalLiteral Nothing 100 2
-- >>> setMinimumDigits 0 (DecimalLiteral Nothing 100 1)
-- DecimalLiteral Nothing 100 1
setMinimumDigits :: Word8 -> DecimalLiteral -> DecimalLiteral
setMinimumDigits wantedDigits dl =
  let currentDigits :: Word8
      currentDigits = digits dl
   in if wantedDigits <= currentDigits
        then dl
        else increaseDigits (wantedDigits - currentDigits) dl
  where
    increaseDigits :: Word8 -> DecimalLiteral -> DecimalLiteral
    increaseDigits 0 = id
    increaseDigits w = \case
      DecimalLiteral mS m e -> increaseDigits (pred w) (DecimalLiteral mS (m * 10) (succ e))

-- | Ensures that a positive literal has no sign
--
-- Turns a @Just True@ sign into a @Nothing@ sign.
setSignOptional :: DecimalLiteral -> DecimalLiteral
setSignOptional = \case
  DecimalLiteral mS m e -> DecimalLiteral (go mS) m e
  where
    go = \case
      Just True -> Nothing
      s -> s

-- | Ensures that a positive literal has a sign
--
-- Turns a @Nothing@ sign into a @Just True@ sign.
setSignRequired :: DecimalLiteral -> DecimalLiteral
setSignRequired = \case
  DecimalLiteral mS m e -> DecimalLiteral (go mS) m e
  where
    go = \case
      Nothing -> Just True
      s -> s
