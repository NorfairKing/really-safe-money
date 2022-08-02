{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity
import Data.GenValidity.Criterion
import Data.GenValidity.Vector ()
import Data.Proxy
import Data.Typeable
import qualified Data.Vector as V
import Money.Amount (Amount)
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Money.AmountOf (AmountOf)
import qualified Money.AmountOf as AmountOf
import Money.AmountOf.Gen ()
import Money.Currency as Currency
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (mkQCGen)

main :: IO ()
main = do
  Criterion.defaultMain
    [ bgroup
        "generators"
        [ genValidBench @Amount,
          genValidBench @(AmountOf USD),
          genValidBench @(AmountOf BTC)
        ],
      bgroup
        "operations"
        [ env (pure (generateDeterministically genValid)) $ \args ->
            bgroup
              "add"
              $ concat
                [ [ bench "add" $ nf (V.map (uncurry Amount.add)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        let makeTyped = V.map (\(l, r) -> (AmountOf.fromAmount l, AmountOf.fromAmount r))
                         in env (pure $ makeTyped args) $ \args' ->
                              bench ("add " <> nameOf @currency) $
                                nf (V.map (uncurry AmountOf.add)) args'
                    )
                ]
        ]
    ]

forAllCurrencies :: (forall currency. (Typeable currency, Currency currency) => Proxy currency -> Benchmark) -> [Benchmark]
forAllCurrencies func =
  [ func (Proxy @Currency.USD),
    func (Proxy @Currency.CHF),
    func (Proxy @Currency.BTC),
    func (Proxy @Currency.ADA)
  ]

nameOf ::
  forall a.
  Typeable a =>
  String
nameOf =
  let s = show $ typeRep (Proxy @a)
   in if ' ' `elem` s
        then "(" ++ s ++ ")"
        else s

generateDeterministically :: Gen a -> a
generateDeterministically (MkGen f) = f seed size
  where
    seed = mkQCGen 42
    size = 30
