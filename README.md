# Really safe money

## Status

Ready to try out!
This is not used in production anywhere yet, and the docs are incomplete, so try it out with the appropriate amount of caution.

## Features

* No partial functions.
* No silently incorrect functions.
* Compile errors for instances that must not exist
* Batteries included, otherwise users will write their own batteries, incorrectly

## Comparison

|  | Really Safe Money | [Safe Money](https://github.com/k0001/safe-money) | [Dollaridoos](https://github.com/qfpl/dollaridoos) | [Plutus values](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#t:Value) | [Simple Amount](https://hackage.haskell.org/package/simple-amount-0.2.0) | [Money](https://github.com/jpvillaisaza/money) | [Simple Money](https://hackage.haskell.org/package/simple-money) |
|--|--|--|--|--|--|--|--|
| Cannot create or destroy money through errors     | ✔️  | ✔️  | C     | ✔️ | ✔️ | ✔️    | ✖️ |
| No silent overflow or underflow                   | ✔️  | ✔️  | C     | ✔️ | ✔️ | ✔️    | ✖️ |
| Cannot represent nonsense values of money         | ✔️  | ✔️  | C     | ✔️ | ✖️ | ✔️    | ✖️ |
| Computations use constant time and space          | ✔️  | ✖️  | C     | ✖️ | ✖️ | ✖️    | ✔️ |
| Cannot represent amounts that are too granular    | ✔️  | ✖️  | C     | ✔️ | ✖️ | ✖️    | ✖️ |
| No-floating-point representation                  | ✔️  | ✔️  | ✖️     | ✔️ | ✔️ | ✔️    | ✖️ |
| Fixed-sized representation                        | ✔️  | ✖️  | C     | ✖️ | ✖️ | ✖️    | ✔️ |
| Amounts with    type-level currency               | ✔️  | ✔️  | C [1] | ✖️ | ✔️ | ✔️[2] | ✔️ |
| Amounts without type-level currency               | ✔️  | ✖️  | C     | ✔️ | ✖️ | ✖️    | ✖️ |
| A type for positive-only amounts                  | ✔️  | ✖️  | C     | ✖️ | ✖️ | ✔️    | ✖️ |
| A type for positive or negative amounts           | ✔️  | ✔️  | C     | ✔️ | ✔️ | ✖️    | ✔️ |
| No 'Num' instance for amounts                     | ✔️  | ✖️  | ✖️     | ✔️ | ✖️ | ✖️    | ✔️ |
| Type-errors for instances that must not exist     | ✔️  | ✔️  | ✖️     | ✖️ | ✖️ | ✖️    | ✖️ |
| Multi-amounts                                     | 🚧 | ✖️  | ✖️     | ✔️ | ✖️ | ✖️    | ✖️ |
| Addition                                          | ✔️  | ✔️  | ✔️     | ✖️ | ✔️ | ✔️    | ✔️ |
| Subtraction                                       | ✔️  | ✔️  | ✔️     | ✖️ | ✔️ | ✔️    | ✔️ |
| Integer scalar multiplication                     | ✔️  | ✔️  | ✔️     | ✖️ | ✔️ | ✔️    | ✔️ |
| Distribution                                      | ✔️  | ✖️  | ✖️     | ✖️ | ✔️ | ✖️    | ✖️ |
| Fractional multiplication                         | ✔️  | ✔️  | ✔️     | ✖️ | ✔️ | ✔️    | ✔️ |
| Fractional multiplication with correct accounting | ✔️  | ✖️  | ✖️     | ✖️ | ✖️ | ✖️    | ✖️ |

[1]: Dollaridoos technically supports multiple currencies, but [its `Show` instance](https://github.com/qfpl/dollaridoos/blob/fd0686edad9fee855f4651cb9494a9214f570e6a/src/Data/Money.hs) always uses `$`.

[2]: [Money technically supports multiple currencies, but only the three that it defines](https://github.com/jpvillaisaza/money/blob/fbfac3dbc585749035d46e31ca6c9b4b53c978ef/src/Data/Money.hs#L48-L51).


* ✔️: Supported
* C: Possible but you have to write some code yourself. This is not good enough for a money library.
* 🚧: Under development
* ✖️: Not supported
* ?: I don't know.

## License

All rights reserved.
Contact me if you would like to use this library.
