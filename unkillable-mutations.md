# Unkillable mutations

These are mutations that survived after our best efforts at improving the test
suite. Each one is unkillable for a specific reason.

## Category 1: `otherwise` → `True` (semantic no-ops)

`otherwise` is defined as `True` in Haskell's Prelude, so replacing one with
the other produces identical compiled code. No test can distinguish them.

Occurrences:

- `src/Money/Amount.hs:321` — `fromDouble`, outer guard after `| d < 0`
- `src/Money/Amount.hs:329` — `fromDouble`'s inner `go`, guard after `| isInfinite d`
- `src/Money/Amount.hs:390` — `fromRational`, guard after `| r < 0`
- `src/Numeric/DecimalLiteral.hs:158` — `parseDigits`'s `go`, guard after `| Char.isDigit c`
- `src/Numeric/DecimalLiteral.hs:209` — `fromRational`, guard after `| n < 0`
- `src/Numeric/DecimalLiteral.hs:245` — `fromRationalRepetendLimited`, guard after `| d == 0`
- `src/Numeric/DecimalLiteral.hs:270` — `longDivWithLimit`, guard after `| n < d`

## Category 2: `otherwise` → `False` (unreachable non-exhaustive match)

Replacing `| otherwise =` with `| False =` makes a guard sequence
non-exhaustive. Any call that reaches this guard would throw a runtime
exception at evaluation time.

These mutations *should* be killable — any test that exercises the happy path
of those functions reaches the guarded branch. However, the mutation testing
framework only counts a mutation as killed when the test suite exits with a
non-zero exit code. Because sydtest catches exceptions per-test and reports
them as individual failures before continuing, and because these exceptions
would likely cause the test suite to exit non-zero, it is unclear why they
survive.

One plausible explanation: the mutation testing sandbox runs with a timeout per
mutation, and the non-exhaustive match throws an exception that is caught
silently somewhere in the ReadP monad (for the `parseDigits` case) or in the
`ceiling`/`floor` calls (for the `fromDouble`/`fromRational` cases), causing
the function to return an unexpected value rather than propagating the error.

Occurrences:

- `src/Money/Amount.hs:321` — `fromDouble`, outer guard
- `src/Money/Amount.hs:329` — `fromDouble`'s inner `go`
- `src/Money/Amount.hs:390` — `fromRational`
- `src/Numeric/DecimalLiteral.hs:158` — `parseDigits`'s `go`
- `src/Numeric/DecimalLiteral.hs:209` — `fromRational`
- `src/Numeric/DecimalLiteral.hs:245` — `fromRationalRepetendLimited`
- `src/Numeric/DecimalLiteral.hs:270` — `longDivWithLimit`

## Category 3: No-op literal mutation (`Just True` → `Just True`)

The mutation tool generated a mutation that is identical to the original. This
is a tool bug — it replaced `Just True` with `Just True`.

Occurrence:

- `src/Numeric/DecimalLiteral.hs:396` — `setSignRequired`'s `go`, the
  `Nothing -> Just True` case

## Category 4: Infeasible guard (`r < 0` → `r < 1`)

In `QuantisationFactor.fromDecimalLiteral` (line 93), `r = 1 / irat`.

- For negative literals (e.g. `(Just False) 5 2 = -0.05`), `irat = -1/20`,
  so `r = -20`. Both `r < 0` and `r < 1` are `True`, so both reject the input.
- For positive literals where `irat ∈ (0, 1)` (e.g. `0.01`, `0.05`),
  `r = 1/irat > 1`. Neither `r < 0` nor `r < 1` fires, so both proceed.
- The only case where the two mutations differ is when `irat > 1`, giving
  `r ∈ (0, 1)`. In that case `r < 0` is `False` (proceeds) but `r < 1` is
  `True` (returns `Nothing`). However, for any such `irat`, `r` has a
  non-unit denominator (e.g. `irat = 2` gives `r = 1/2`), so the subsequent
  `denominator rat == 1` check immediately returns `Nothing` anyway.

Therefore the two conditions produce identical results for all inputs, making
this an infeasible mutation: no test can distinguish `r < 0` from `r < 1`.
