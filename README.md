# Really safe money

## Status

Not ready for anything.
Come back no sooner than when this has a passing CI.

## Features

* No partial functions.
* No silently incorrect functions.
* Compile errors for instances that must not exist

## Comparison

|  | Really Safe Money | [Safe Money](https://github.com/k0001/safe-money) | [Dollaridoos](https://github.com/qfpl/dollaridoos) | [Plutus values](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#t:Value) | [Money](https://github.com/jpvillaisaza/money) | [Simple Money](https://hackage.haskell.org/package/simple-money) |
|--|--|--|--|--|--|--|
| Amounts with    type-level currency              | ✔️  | ✔️  | C | ✖ | ✔️ | ✔️ |
| Amounts without type-level currency              | ✔️  | ✖️  | C | ✔ | ✖ | ✖ |
| No 'Dense'                                       | ✔️  | ✖️  | ✔️ | ✔️ | ✔️ | ✔️ |
| No-floating-point representation                 | ✔️  | ✔️  | ✖️ | ✔️ | ✔️ | ✖ |
| Fixed-sized representation                       | ✔️  | ✖  | C | ✖️ | ✖ | ✔️ |
| No 'Num' instance for amounts                    | ✔️  | ✔️  | ✖ | ✔️ | ✖ | ✔️ |
| A type for positive amounts                      | ✔️  | ✖  | C | ✖️ | ✖ | ✖ |
| Multi-amounts                                    | 🚧 | ✖️  | ✖️ | ✔️ | ✖ | ✖ |


* ✔️: Supported
* C: Possible but you have to write some code yourself
* 🚧: Under development
* ✖️: Not supported
* ?: I don't know.

## License

All rights reserved.
Contact me if you would like to use this library.
