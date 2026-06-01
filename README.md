# Really safe money

[![NixCI](https://staging.nix-ci.com/badge/gh:NorfairKing:really-safe-money)](https://staging.nix-ci.com/gh:NorfairKing:really-safe-money)

## Status

Ready to use.
This is used in production at [NixCI](https://nix-ci.com/) and [Centjes](https://centjes.cs-syd.eu/).

## Features

* No partial functions.
* No silently incorrect functions.
* Compile errors for instances that must not exist
* Batteries included, otherwise users will write their own batteries, incorrectly

## Comparison

|  | Really Safe Money | [Safe Money](https://github.com/k0001/safe-money) | [Dollaridoos](https://github.com/qfpl/dollaridoos) | [Plutus values](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#t:Value) | [Simple Amount](https://hackage.haskell.org/package/simple-amount-0.2.0) | [Safe Decimal](https://hackage.haskell.org/package/safe-decimal-0.2.1.0) | [Money](https://github.com/jpvillaisaza/money) | [Simple Money](https://hackage.haskell.org/package/simple-money) |
|--|--|--|--|--|--|--|--|--|
| Explicitly meant for money                        | ✔️  | ✔️  | ✔️     | ✔️ | ✖️ | ✖️ | ✔️    | ✔️ |
| Cannot create or destroy money through errors     | ✔️  | ✔️  | C     | ✔️ | ✔️ | ✔️ | ✔️    | ✖️ |
| No silent overflow or underflow                   | ✔️  | ✔️  | C     | ✔️ | ✔️ | ✔️ | ✔️    | ✖️ |
| Cannot represent nonsense values of money         | ✔️  | ✔️  | C     | ✔️ | ✖️ | C | ✔️    | ✖️ |
| Computations use constant time and space          | ✔️  | ✖️  | C     | ✖️ | ✖️ | C | ✖️    | ✔️ |
| Cannot represent amounts that are too granular    | ✔️  | ✖️  | C     | ✔️ | ✖️ | C | ✖️    | ✖️ |
| No-floating-point representation                  | ✔️  | ✔️  | C     | ✔️ | ✔️ | ✔️ | ✔️    | ✖️ |
| Fixed-sized representation                        | ✔️  | ✖️  | C     | ✖️ | ✖️ | C | ✖️    | ✔️ |
| Amounts with    type-level currency               | ✔️  | ✔️  | C [1] | ✖️ | ✔️ | ✖️ | ✔️[2] | ✔️ |
| Amounts without type-level currency               | ✔️  | ✖️  | C     | ✔️ | ✖️ | ✔️ | ✖️    | ✖️ |
| A type for positive-only amounts                  | ✔️  | ✖️  | C     | ✖️ | ✖️ | ✔️ | ✔️    | ✖️ |
| A type for positive or negative amounts           | ✔️  | ✔️  | C     | ✔️ | ✔️ | ✔️ | ✖️    | ✔️ |
| No 'Num' instance for amounts                     | ✔️  | ✖️  | ✖️     | ✔️ | ✖️ | ✖️ | ✖️    | ✔️ |
| Type-errors for instances that must not exist     | ✔️  | ✔️  | ✖️     | ✖️ | ✖️ | ✖️ | ✖️    | ✖️ |
| Multi-amounts                                     | ✔️  | ✖️  | ✖️     | ? | ✖️ | ✖️ | ✖️    | ✖️ |
| Multi-accounts                                    | ✔️  | ✖️  | ✖️     | ? | ✖️ | ✖️ | ✖️    | ✖️ |
| Addition                                          | ✔️  | ✔️  | ✔️     | ✖️ | ✔️ | ✔️ | ✔️    | ✔️ |
| Subtraction                                       | ✔️  | ✔️  | ✔️     | ✖️ | ✔️ | ✔️ | ✔️    | ✔️ |
| Negation                                          | ✔️  | ✔️  | ✔️     | ✖️ | ✔️ | ✔️ | ✔️    | ✖️ |
| Integer scalar multiplication                     | ✔️  | ✔️  | ✔️     | ✖️ | ✖️ | ✖️ | ✔️    | ✔️ |
| Distribution                                      | ✔️  | ✖️  | ✖️     | ✖️ | ✔️ | ✖️ | ✖️    | ✖️ |
| Fractional multiplication                         | ✔️  | ✔️  | ✔️     | ✖️ | ✔️ | ✔️ | ✔️    | ✔️ |
| Fractional multiplication with correct accounting | ✔️  | ✖️  | ✖️     | ✖️ | ✖️ | ✖️ | ✖️    | ✖️ |

[1]: Dollaridoos technically supports multiple currencies, but [its `Show` instance](https://github.com/qfpl/dollaridoos/blob/fd0686edad9fee855f4651cb9494a9214f570e6a/src/Data/Money.hs) always uses `$`.

[2]: [Money technically supports multiple currencies, but only the three that it defines](https://github.com/jpvillaisaza/money/blob/fbfac3dbc585749035d46e31ca6c9b4b53c978ef/src/Data/Money.hs#L48-L51).

(I will happily correct any errors in this table if you see I have made any.)


* ✔️: Supported
* C: Possible but you have to write some code yourself. This is not good enough for a money library.
* 🚧: Under development
* ✖️: Not supported
* ?: I don't know.

## License

All rights reserved.
Contact me if you would like to use this library.
