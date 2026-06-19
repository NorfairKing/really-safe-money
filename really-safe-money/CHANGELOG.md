# Changelog

## [0.0.0.1] - 2026-06-19

### Changed

- Adapted to the upgraded `sydtest` mutation-testing engine, which adds the
  `SwitchFunctionArguments`, `RemoveClause`, `ElideCall` and `TupleSwap`
  operators. The killable new mutants are now covered by tests; the equivalent
  ones are marked with `DisableMutation` annotations.
- Simplified some internals that were flagged as redundant or dead code by the
  new operators (all behaviour-preserving, no API changes):
  - `Money.Account.distribute`: dropped a no-op `abs` on the `Word16` chunk
    count and removed the unreachable zero-account branches.
  - `Money.Amount.fraction`: removed the redundant zero-fraction clause.
  - `Numeric.DecimalLiteral.toRational`/`fromRatio`: dropped redundant
    always-`Nothing`/sign plumbing.
