{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.DeepSeq
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
        $ concat
          [ [genValidBench @Amount],
            forAllCurrencies $ \(Proxy :: Proxy currency) ->
              genValidBench @(AmountOf currency)
          ],
      bgroup
        "conversions"
        [ withArgs $ \args ->
            bgroup "fromMinimalQuantisations" $
              concat
                [ [ bench "fromMinimalQuantisations" $ nf (V.map Amount.fromMinimalQuantisations) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        bench (nameOf @currency) $
                          nf (V.map (AmountOf.fromMinimalQuantisations @currency)) args
                    )
                ],
          withArgs $ \args ->
            bgroup "toMinimalQuantisations" $
              concat
                [ [ bench "toMinimalQuantisations" $ nf (V.map Amount.toMinimalQuantisations) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        env (pure $ V.map AmountOf.fromAmount args) $ \args' ->
                          bench (nameOf @currency) $
                            nf (V.map (AmountOf.toMinimalQuantisations @currency)) args'
                    )
                ],
          withArgs $ \args ->
            bgroup "fromDouble" $
              concat
                [ [ bench "fromDouble" $ nf (V.map (uncurry Amount.fromDouble)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        env (pure $ V.map snd args) $ \args' ->
                          bench (nameOf @currency) $
                            nf (V.map (AmountOf.fromDouble @currency)) args'
                    )
                ],
          withArgs $ \args ->
            bgroup "toDouble" $
              concat
                [ [ bench "toDouble" $ nf (V.map (uncurry Amount.toDouble)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        env (pure $ V.map (AmountOf.fromAmount . snd) args) $ \args' ->
                          bench (nameOf @currency) $
                            nf (V.map (AmountOf.toDouble @currency)) args'
                    )
                ],
          withArgs $ \args ->
            bgroup "fromRational" $
              concat
                [ [ bench "fromRational" $ nf (V.map (uncurry Amount.fromRational)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        env (pure $ V.map snd args) $ \args' ->
                          bench (nameOf @currency) $
                            nf (V.map (AmountOf.fromRational @currency)) args'
                    )
                ],
          withArgs $ \args ->
            bgroup "toRational" $
              concat
                [ [ bench "toRational" $ nf (V.map (uncurry Amount.toRational)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        env (pure $ V.map (AmountOf.fromAmount . snd) args) $ \args' ->
                          bench (nameOf @currency) $
                            nf (V.map (AmountOf.toRational @currency)) args'
                    )
                ]
        ],
      bgroup
        "operations"
        [ withArgs $ \args ->
            bgroup
              "add"
              $ concat
                [ [ bench "add" $ nf (V.map (uncurry Amount.add)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        let makeTyped = V.map (\(l, r) -> (AmountOf.fromAmount l, AmountOf.fromAmount r))
                         in env (pure $ makeTyped args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (uncurry AmountOf.add)) args'
                    )
                ],
          withArgs $ \args ->
            bgroup
              "subtract"
              $ concat
                [ [ bench "subtract" $ nf (V.map (uncurry Amount.subtract)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        let makeTyped = V.map (\(l, r) -> (AmountOf.fromAmount l, AmountOf.fromAmount r))
                         in env (pure $ makeTyped args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (uncurry AmountOf.subtract)) args'
                    )
                ],
          withArgs $ \args ->
            bgroup
              "multiply"
              $ concat
                [ [ bench "multiply" $ nf (V.map (uncurry Amount.multiply)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        let makeTyped = V.map (\(l, r) -> (l, AmountOf.fromAmount r))
                         in env (pure $ makeTyped args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (uncurry AmountOf.multiply)) args'
                    )
                ],
          withArgs $ \args ->
            bgroup
              "divide"
              $ concat
                [ [ bench "divide" $ nf (V.map (uncurry Amount.divide)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        let makeTyped = V.map (\(l, r) -> (AmountOf.fromAmount l, r))
                         in env (pure $ makeTyped args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (uncurry AmountOf.divide)) args'
                    )
                ],
          withArgs $ \args ->
            bgroup
              "fraction"
              $ concat
                [ [ bench "fraction" $ nf (V.map (uncurry Amount.fraction)) args
                  ],
                  forAllCurrencies
                    ( \(Proxy :: Proxy currency) ->
                        let makeTyped = V.map (\(l, r) -> (AmountOf.fromAmount l, r))
                         in env (pure $ makeTyped args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (uncurry AmountOf.fraction)) args'
                    )
                ]
        ]
    ]

forAllCurrencies :: (forall currency. (Typeable currency, Currency currency) => Proxy currency -> Benchmark) -> [Benchmark]
forAllCurrencies func =
  [ func (Proxy @Currency.USD)
  -- The others don't actually matter for benchmarking purposes, it turns out.
  -- ,
  -- func (Proxy @Currency.CHF),
  -- func (Proxy @Currency.BTC),
  -- func (Proxy @Currency.ADA)
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

withArgs :: (NFData env, GenValid env) => (env -> Benchmark) -> Benchmark
withArgs = env (pure (generateDeterministically genValid))

generateDeterministically :: Gen a -> a
generateDeterministically (MkGen f) = f seed size
  where
    seed = mkQCGen 42
    size = 30