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
import Data.Vector (Vector)
import qualified Data.Vector as V
import Money.Account (Account)
import qualified Money.Account as Account
import Money.Account.Gen ()
import qualified Money.AccountOf as AccountOf
import Money.Amount (Amount)
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Money.AmountOf (AmountOf)
import qualified Money.AmountOf as AmountOf
import Money.AmountOf.Gen ()
import Money.Currency as Currency
import Money.QuantisationFactor
import Money.QuantisationFactor.Gen ()
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (mkQCGen)

main :: IO ()
main = do
  Criterion.defaultMain
    [ bgroup "generators" $
        concat
          [ [genValidBench @Amount],
            forAllCurrencies $ \(Proxy :: Proxy currency) ->
              genValidBench @(AmountOf currency),
            [genValidBench @Account]
          ],
      bgroup
        "Account"
        [ bgroup
            "conversions"
            [ withArgs $ \args ->
                bgroup "fromMinimalQuantisations" $
                  concat
                    [ [ bench "fromMinimalQuantisations" $ nf (V.map Account.fromMinimalQuantisations) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            bench (nameOf @currency) $
                              nf (V.map (AccountOf.fromMinimalQuantisations @currency)) args
                        )
                    ],
              withArgs $ \args ->
                bgroup "toMinimalQuantisations" $
                  concat
                    [ [ bench "toMinimalQuantisations" $ nf (V.map Account.toMinimalQuantisations) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            env (pure $ V.map AccountOf.fromAccount args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (AccountOf.toMinimalQuantisations @currency)) args'
                        )
                    ],
              withArgs $ \args ->
                bgroup "fromDouble" $
                  concat
                    [ [ bench "fromDouble" $ nf (V.map (uncurry Account.fromDouble)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            env (pure $ V.map snd args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (AccountOf.fromDouble @currency)) args'
                        )
                    ],
              withArgs $ \args ->
                bgroup "toDouble" $
                  concat
                    [ [ bench "toDouble" $ nf (V.map (uncurry Account.toDouble)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            env (pure $ V.map (AccountOf.fromAccount . snd) args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (AccountOf.toDouble @currency)) args'
                        )
                    ],
              withArgs $ \args ->
                bgroup "fromRational" $
                  concat
                    [ [ bench "fromRational" $ nf (V.map (uncurry Account.fromRational)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            env (pure $ V.map snd args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (AccountOf.fromRational @currency)) args'
                        )
                    ],
              withArgs $ \args ->
                bgroup "toRational" $
                  concat
                    [ [ bench "toRational" $ nf (V.map (uncurry Account.toRational)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            env (pure $ V.map (AccountOf.fromAccount . snd) args) $ \args' ->
                              bench (nameOf @currency) $
                                nf (V.map (AccountOf.toRational @currency)) args'
                        )
                    ]
            ],
          bgroup
            "operations"
            [ withArgs $ \args ->
                bgroup
                  "add"
                  $ concat
                    [ [ bench "add" $ nf (V.map (uncurry Account.add)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            let makeTyped = V.map (\(l, r) -> (AccountOf.fromAccount l, AccountOf.fromAccount r))
                             in env (pure $ makeTyped args) $ \args' ->
                                  bench (nameOf @currency) $
                                    nf (V.map (uncurry AccountOf.add)) args'
                        )
                    ],
              withArgs $
                \args ->
                  bgroup
                    "sum"
                    $ concat
                      [ [ bench "sum" $ nf (V.map (Account.sum @Vector)) args
                        ],
                        forAllCurrencies
                          ( \(Proxy :: Proxy currency) ->
                              let makeTyped = V.map (V.map AccountOf.fromAccount)
                               in env (pure $ makeTyped args) $ \args' ->
                                    bench (nameOf @currency) $
                                      nf (V.map AccountOf.sum) args'
                          )
                      ],
              withArgs $ \args ->
                bgroup
                  "subtract"
                  $ concat
                    [ [ bench "subtract" $ nf (V.map (uncurry Account.subtract)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            let makeTyped = V.map (\(l, r) -> (AccountOf.fromAccount l, AccountOf.fromAccount r))
                             in env (pure $ makeTyped args) $ \args' ->
                                  bench (nameOf @currency) $
                                    nf (V.map (uncurry AccountOf.subtract)) args'
                        )
                    ],
              withArgs $ \args ->
                bgroup
                  "multiply"
                  $ concat
                    [ [ bench "multiply" $ nf (V.map (uncurry Account.multiply)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            let makeTyped = V.map (\(l, r) -> (l, AccountOf.fromAccount r))
                             in env (pure $ makeTyped args) $ \args' ->
                                  bench (nameOf @currency) $
                                    nf (V.map (uncurry AccountOf.multiply)) args'
                        )
                    ],
              withArgs $ \args ->
                bgroup
                  "distribute"
                  $ concat
                    [ [ bench "distribute" $ nf (V.map (uncurry Account.distribute)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            let makeTyped = V.map (\(l, r) -> (AccountOf.fromAccount l, r))
                             in env (pure $ makeTyped args) $ \args' ->
                                  bench (nameOf @currency) $
                                    nf (V.map (uncurry AccountOf.distribute)) args'
                        )
                    ],
              withArgs $ \args ->
                bgroup
                  "fraction"
                  $ concat
                    [ [ bench "fraction" $ nf (V.map (uncurry3 Account.fraction)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            let makeTyped = V.map (\(ro, l, r) -> (ro, AccountOf.fromAccount l, r))
                             in env (pure $ makeTyped args) $ \args' ->
                                  bench (nameOf @currency) $
                                    nf (V.map (uncurry3 AccountOf.fraction)) args'
                        )
                    ]
            ]
        ],
      bgroup
        "Amount"
        [ bgroup
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
                bgroup "add" $
                  concat
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
                bgroup "sum" $
                  concat
                    [ [ bench "sum" $ nf (V.map Amount.sum) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            let makeTyped = V.map (V.map AmountOf.fromAmount)
                             in env (pure $ makeTyped args) $ \args' ->
                                  bench (nameOf @currency) $
                                    nf (V.map AmountOf.sum) args'
                        )
                    ],
              withArgs $ \args ->
                bgroup "subtract" $
                  concat
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
                bgroup "multiply" $
                  concat
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
                bgroup "distribute" $
                  concat
                    [ [ bench "distribute" $ nf (V.map (uncurry Amount.distribute)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            let makeTyped = V.map (\(l, r) -> (AmountOf.fromAmount l, r))
                             in env (pure $ makeTyped args) $ \args' ->
                                  bench (nameOf @currency) $
                                    nf (V.map (uncurry AmountOf.distribute)) args'
                        )
                    ],
              withArgs $ \args ->
                bgroup "fraction" $
                  concat
                    [ [ bench "fraction" $ nf (V.map (uncurry3 Amount.fraction)) args
                      ],
                      forAllCurrencies
                        ( \(Proxy :: Proxy currency) ->
                            let makeTyped = V.map (\(ro, l, r) -> (ro, AmountOf.fromAmount l, r))
                             in env (pure $ makeTyped args) $ \args' ->
                                  bench (nameOf @currency) $
                                    nf (V.map (uncurry3 AmountOf.fraction)) args'
                        )
                    ]
            ]
        ]
    ]

forAllCurrencies :: (forall currency. (Typeable currency, IsCurrencyType currency) => Proxy currency -> Benchmark) -> [Benchmark]
forAllCurrencies func =
  [ func (Proxy @Hypothetical)
  -- The others don't actually matter for benchmarking purposes, it turns out.
  ]

data Hypothetical

instance IsCurrencyType Hypothetical where
  quantisationFactor Proxy = QuantisationFactor 47

nameOf ::
  forall a.
  (Typeable a) =>
  String
nameOf =
  let s = show $ typeRep (Proxy @a)
   in if ' ' `elem` s
        then "(" ++ s ++ ")"
        else s

withArgs :: (NFData arg, GenValid arg) => (Vector arg -> Benchmark) -> Benchmark
withArgs = env (pure (generateDeterministically $ V.replicateM 100 genValid))

generateDeterministically :: Gen a -> a
generateDeterministically (MkGen f) = f seed size
  where
    seed = mkQCGen 42
    size = 30

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 func (a, b, c) = func a b c
