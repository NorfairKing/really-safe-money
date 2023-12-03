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
    parseDecimalLiteral,
    parseDecimalLiteralM,
    renderDecimalLiteral,
    fromRational,
    toRational,
    fromQuantisationFactor,
    toQuantisationFactor,
    fromAmount,
    toAmount,
    fromAmountOf,
    toAmountOf,
    fromAccount,
    toAccount,
    fromAccountOf,
    toAccountOf,
    setSignRequired,
    setSignOptional,
    digits,
    setMinimumDigits,
  )
where

import Control.DeepSeq
import qualified Data.Char as Char
import Data.List (find)
import Data.Proxy
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Validity
import Data.Validity.Scientific ()
import Data.Word
import GHC.Generics (Generic)
import Money.Account as Money (Account (..))
import qualified Money.Account as Account
import Money.AccountOf as Money (AccountOf (..))
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import Money.AmountOf as Money (AmountOf (..))
import Money.Currency
import Money.QuantisationFactor (QuantisationFactor (..))
import qualified Money.QuantisationFactor as QuantisationFactor
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

instance IsString DecimalLiteral where
  fromString s = case parseDecimalLiteral s of
    Nothing -> error $ "Invalid decimal literal string: " <> show s
    Just dl -> dl

-- | Parse a decimal literal from a string
--
-- This only accepts non-scientific notation
--
-- >>> parseDecimalLiteral "1"
-- Just (DecimalLiteralInteger Nothing 1)
-- >>> parseDecimalLiteral "0.02"
-- Just (DecimalLiteralFractional Nothing 2 1)
-- >>> parseDecimalLiteral "+0.00003"
-- Just (DecimalLiteralFractional (Just True) 3 4)
-- >>> parseDecimalLiteral "-0.00000004"
-- Just (DecimalLiteralFractional (Just False) 4 7)
parseDecimalLiteral :: String -> Maybe DecimalLiteral
parseDecimalLiteral = fmap fst . find (null . snd) . readP_to_S decimalLiteralP

-- | Like 'parseDecimalLiteral' but in a 'MonadFail'
parseDecimalLiteralM :: MonadFail m => String -> m DecimalLiteral
parseDecimalLiteralM s = case parseDecimalLiteral s of
  Nothing -> fail $ "Failed to parse decimal literal from:" <> show s
  Just dl -> pure dl

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

-- | Render a decimal literal to a string
--
-- >>> renderDecimalLiteral (DecimalLiteralInteger Nothing 5)
-- "5"
-- >>> renderDecimalLiteral (DecimalLiteralInteger (Just True) 60)
-- "+60"
-- >>> renderDecimalLiteral (DecimalLiteralInteger (Just False) 700)
-- "-700"
-- >>> renderDecimalLiteral (DecimalLiteralFractional Nothing 8 2)
-- "0.008"
-- >>> renderDecimalLiteral (DecimalLiteralFractional (Just True) 90 4)
-- "+0.00090"
-- >>> renderDecimalLiteral (DecimalLiteralFractional (Just False) 100 6)
-- "-0.0000100"
renderDecimalLiteral :: DecimalLiteral -> String
renderDecimalLiteral = \case
  DecimalLiteralInteger s a -> sign s ++ show a
  DecimalLiteralFractional s m e -> sign s ++ goFrac m e
  where
    sign = \case
      Nothing -> ""
      Just True -> "+"
      Just False -> "-"

    goFrac m e = reverse (go (succ e) (reverse (show m)))
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
-- Just (DecimalLiteralInteger Nothing 1)
-- >>> fromRational ((-1) % 2)
-- Just (DecimalLiteralFractional (Just False) 5 0)
-- >>> fromRational (1 % 3)
-- Nothing
fromRational :: Rational -> Maybe DecimalLiteral
fromRational r =
  let d = denominator r
   in if d == 1
        then Just (DecimalLiteralInteger (if r >= 0 then Nothing else Just False) (fromIntegral (abs (numerator r))))
        else fromRationalRepetendLimited 256 r
  where
    fromRationalRepetendLimited ::
      -- limit
      Int ->
      Rational ->
      Maybe DecimalLiteral
    fromRationalRepetendLimited l rational
      | d == 0 = Nothing
      | num < 0 = toLiteral (Just False) <$> longDiv (-num)
      | otherwise = toLiteral Nothing <$> longDiv num
      where
        toLiteral mSign (m, e) = DecimalLiteralFractional mSign m (fromIntegral (pred e))
        d = denominator rational
        num = numerator rational

        longDiv :: Integer -> Maybe (Natural, Int)
        longDiv = longDivWithLimit 0 0 S.empty

        longDivWithLimit ::
          Integer ->
          Int ->
          Set Integer ->
          Integer ->
          Maybe (Natural, Int)
        longDivWithLimit !c !e _ns 0 =
          Just (fromIntegral (abs c), e)
        longDivWithLimit !c !e ns !n
          -- If there's a repetend, we can't turn it into a decimal literal
          | S.member n ns = Nothing
          -- Over the limit, stop trying
          | e >= l = Nothing
          | n < d =
              let !ns' = S.insert n ns
               in longDivWithLimit (c * 10) (succ e) ns' (n * 10)
          | otherwise = case n `quotRem` d of
              (q, r') -> longDivWithLimit (c + q) e ns r'

-- | Turn a 'DecimalLiteral' into a 'Rational'
--
-- This throws away all rendering information
--
-- >>> toRational (DecimalLiteralInteger Nothing 1)
-- 1 % 1
-- >>> toRational (DecimalLiteralFractional (Just True) 2 0)
-- 1 % 5
-- >>> toRational (DecimalLiteralFractional (Just False) 3 0)
-- (-3) % 10
toRational :: DecimalLiteral -> Rational
toRational = \case
  DecimalLiteralInteger mSign i ->
    signRational mSign * fromIntegral i
  DecimalLiteralFractional mSign m e ->
    (signRational mSign * fromIntegral m) / (10 ^ (e + 1))
  where
    signRational :: Maybe Bool -> Rational
    signRational = \case
      Nothing -> 1
      Just True -> 1
      Just False -> -1

-- | Render a 'DecimalLiteral' that represents the smallest unit from a 'QuantisationFactor'
--
-- Note that this fails on quantisation factors that cannot be represented
-- using a literal, for example because they would correspond to a number with
-- an infinite decimal representation.
--
-- this will always have a 'Nothing' sign.
--
-- >>> fromQuantisationFactor (QuantisationFactor 100)
-- Just (DecimalLiteralFractional Nothing 1 1)
-- >>> fromQuantisationFactor (QuantisationFactor 20)
-- Just (DecimalLiteralFractional Nothing 5 1)
-- >>> fromQuantisationFactor (QuantisationFactor 1)
-- Just (DecimalLiteralInteger Nothing 1)
fromQuantisationFactor :: QuantisationFactor -> Maybe DecimalLiteral
fromQuantisationFactor (QuantisationFactor qfw) =
  setSignOptional <$> fromRational (1 % fromIntegral qfw)

-- | Parse a 'QuantisationFactor' from a 'DecimalLiteral' that represents the smallest unit
-- TODO explain that it's the inverse.
--
-- Note that this fails on:
--
-- * Negative literals
-- * Integrals greater than 1
--
-- >>> toQuantisationFactor (DecimalLiteralInteger Nothing 2)
-- Nothing
-- >>> toQuantisationFactor (DecimalLiteralFractional (Just False) 2 1)
-- Nothing
-- >>> toQuantisationFactor (DecimalLiteralFractional (Just True) 2 1)
-- Just (QuantisationFactor {unQuantisationFactor = 50})
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

-- | Parse a 'DecimalLiteral' from an 'Amount' of a currency with a given quantisation factor.
--
-- This fails when the 'QuantisationFactor' would prevent the account to be
-- represented as a finite decimal literal.
--
-- Note that:
--
-- * The resulting literals always have a (positive) sign.
-- * The resulting literals always have digits corresponding to the precision
--   that the quantisation factor prescribes.
--
-- >>> fromAmount (QuantisationFactor 100) (Amount 1)
-- Just (DecimalLiteralFractional (Just True) 1 1)
-- >>> fromAmount (QuantisationFactor 100) (Amount 100)
-- Just (DecimalLiteralFractional (Just True) 100 1)
-- >>> fromAmount (QuantisationFactor 20) (Amount 100)
-- Just (DecimalLiteralFractional (Just True) 500 1)
-- >>> fromAmount (QuantisationFactor 1) (Amount 100)
-- Just (DecimalLiteralInteger (Just True) 100)
-- >>> fromAmount (QuantisationFactor 17) (Amount 100)
-- Nothing
fromAmount :: QuantisationFactor -> Money.Amount -> Maybe DecimalLiteral
fromAmount qf acc =
  let r = Amount.toRational qf acc
   in setSignRequired . setMinimumDigits (QuantisationFactor.digits qf) <$> fromRational r

-- | Convert a 'DecimalLiteral' to an 'Amount' of a currency with a given quantisation factor.
--
-- This fails when:
--
-- * the result would be too big to fit into an 'Amount'.
-- * the decimal literal is too precise.
--
-- >>> toAmount (QuantisationFactor 100) (DecimalLiteralInteger Nothing 100)
-- Just (Amount 10000)
-- >>> toAmount (QuantisationFactor 100) (DecimalLiteralFractional Nothing 1 3)
-- Nothing
-- >>> toAmount (QuantisationFactor 1000000000) (DecimalLiteralInteger Nothing 1000000000000)
-- Nothing
toAmount :: QuantisationFactor -> DecimalLiteral -> Maybe Money.Amount
toAmount qf = Amount.fromRational qf . toRational

-- | See 'fromAmount'
fromAmountOf :: forall currency. IsCurrencyType currency => Money.AmountOf currency -> Maybe DecimalLiteral
fromAmountOf (AmountOf a) = fromAmount (quantisationFactor (Proxy :: Proxy currency)) a

-- | See 'toAmount'
toAmountOf :: forall currency. IsCurrencyType currency => DecimalLiteral -> Maybe (Money.AmountOf currency)
toAmountOf = fmap AmountOf . Amount.fromRational (quantisationFactor (Proxy :: Proxy currency)) . toRational

-- | Parse a 'DecimalLiteral' from an 'Account' of a currency with a given quantisation factor.
--
-- This fails when the 'QuantisationFactor' would prevent the account to be
-- represented as a finite decimal literal.
--
-- Note that:
--
-- * The resulting literals always have a sign.
-- * The resulting literals always have digits corresponding to the precision
--   that the quantisation factor prescribes.
--
-- >>> fromAccount (QuantisationFactor 100) (Positive (Amount 1))
-- Just (DecimalLiteralFractional (Just True) 1 1)
-- >>> fromAccount (QuantisationFactor 100) (Negative (Amount 100))
-- Just (DecimalLiteralFractional (Just False) 100 1)
-- >>> fromAccount (QuantisationFactor 20) (Negative (Amount 100))
-- Just (DecimalLiteralFractional (Just False) 500 1)
-- >>> fromAccount (QuantisationFactor 1) (Positive (Amount 100))
-- Just (DecimalLiteralInteger (Just True) 100)
-- >>> fromAccount (QuantisationFactor 17) (Positive (Amount 100))
-- Nothing
fromAccount :: QuantisationFactor -> Money.Account -> Maybe DecimalLiteral
fromAccount qf acc =
  let r = Account.toRational qf acc
   in setSignRequired . setMinimumDigits (QuantisationFactor.digits qf) <$> fromRational r

-- | Convert a 'DecimalLiteral' to an 'Account' of a currency with a given quantisation factor.
--
-- This fails when:
--
-- * the result would be too big to fit into an 'Account'.
-- * the decimal literal is too precise.
--
-- >>> toAccount (QuantisationFactor 100) (DecimalLiteralInteger Nothing 100)
-- Just (Positive (Amount 10000))
-- >>> toAccount (QuantisationFactor 100) (DecimalLiteralFractional Nothing 1 3)
-- Nothing
-- >>> toAccount (QuantisationFactor 1000000000) (DecimalLiteralInteger Nothing 1000000000000)
-- Nothing
toAccount :: QuantisationFactor -> DecimalLiteral -> Maybe Money.Account
toAccount qf = Account.fromRational qf . toRational

-- | See 'fromAccount'
fromAccountOf :: forall currency. IsCurrencyType currency => Money.AccountOf currency -> Maybe DecimalLiteral
fromAccountOf = fromAccount (quantisationFactor (Proxy :: Proxy currency)) . unAccountOf

-- | See 'toAccount'
toAccountOf :: forall currency. IsCurrencyType currency => DecimalLiteral -> Maybe (Money.AccountOf currency)
toAccountOf = fmap AccountOf . toAccount (quantisationFactor (Proxy :: Proxy currency))

-- | Count how many digits the literal has after the decimal point
--
-- >>> digits (DecimalLiteralInteger Nothing 1)
-- 0
-- >>> digits (DecimalLiteralFractional Nothing 100 1)
-- 2
-- >>> digits (DecimalLiteralFractional Nothing 1 1)
-- 2
digits :: DecimalLiteral -> Word8
digits = \case
  DecimalLiteralInteger _ _ -> 0
  DecimalLiteralFractional _ _ w -> succ w

-- | Set the minimum number of digits the literal has after the decimal point
--
-- Note that this function never decreases the number of digits after the decimal point.
--
-- >>> setMinimumDigits 2 (DecimalLiteralInteger Nothing 1)
-- DecimalLiteralFractional Nothing 100 1
-- >>> setMinimumDigits 0 (DecimalLiteralFractional Nothing 100 1)
-- DecimalLiteralFractional Nothing 100 1
setMinimumDigits :: Word8 -> DecimalLiteral -> DecimalLiteral
setMinimumDigits wantedDigits dl =
  let currentDigits = digits dl
   in if wantedDigits <= currentDigits
        then dl
        else increaseDigits (wantedDigits - currentDigits) dl
  where
    increaseDigits :: Word8 -> DecimalLiteral -> DecimalLiteral
    increaseDigits 0 = id
    increaseDigits w = \case
      DecimalLiteralInteger mS a -> increaseDigits (pred w) (DecimalLiteralFractional mS (a * 10) 0)
      DecimalLiteralFractional mS m e -> increaseDigits (pred w) (DecimalLiteralFractional mS (m * 10) (succ e))

-- | Ensures that a positive literal has no sign
--
-- Turns a @Just True@ sign into a @Nothing@ sign.
setSignOptional :: DecimalLiteral -> DecimalLiteral
setSignOptional = \case
  DecimalLiteralInteger mS a -> DecimalLiteralInteger (go mS) a
  DecimalLiteralFractional mS m e -> DecimalLiteralFractional (go mS) m e
  where
    go = \case
      Just True -> Nothing
      s -> s

-- | Ensures that a positive literal has a sign
--
-- Turns a @Nothing@ sign into a @Just True@ sign.
setSignRequired :: DecimalLiteral -> DecimalLiteral
setSignRequired = \case
  DecimalLiteralInteger mS a -> DecimalLiteralInteger (go mS) a
  DecimalLiteralFractional mS m e -> DecimalLiteralFractional (go mS) m e
  where
    go = \case
      Nothing -> Just True
      s -> s
