---
sidebar_position: 10
---

# Profiling Hydra scripts

Start with whole-transaction costs, then use function-level traces to locate
expensive parts of a validator. Record the repository revision, operation,
party count, and UTxO shape so results can be reproduced.

## Measure transaction costs

From the repository root, generate the current report:

```shell
nix build .#tx-cost
./result/bin/tx-cost --seed 42 --output-directory /tmp/hydra-tx-cost
```

The [published transaction-cost report](./transaction-cost.md) measures size,
CPU, memory, and fees for protocol transactions. Its generators and evaluation
code are in
[TxCost.hs](https://github.com/cardano-scaling/hydra/blob/master/hydra-node/bench/tx-cost/TxCost.hs).
For example, isolate a five-party increment by restricting
`computeIncrementCost` to `[5]` and removing its limit-search loop during the
experiment. Restore the full benchmark afterward.

Use the evaluation report to distinguish the cost of the head validator from
the deposit validator. The current evaluation interface is
[`evaluateTxWith`](pathname:///haddocks/hydra-tx/Hydra-Ledger-Cardano-Evaluate.html#v:evaluateTxWith),
which accepts the protocol parameters, time configuration, transaction, and
input UTxO context.

## Instrument a validator

For the PlutusTx head validator, enable tracing in `Hydra.Contract.Head`:

```haskell
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:conservative-optimisation #-}
```

Rebuild the scripts and the fixture that uses them. Instrumentation can change
optimization and script size, so compare the result with uninstrumented costs.

The deposit validator is Aiken code in `hydra-plutus/validators/deposit.ak`.
Its Haskell module defines datum and redeemer types; Plinth flags there do not
instrument the validator. Use the pinned Aiken toolchain's tracing facilities
for that script, and the transaction-cost benchmark for its total budget.

## Produce a function-level profile

This step needs a fixture harness that exports the validator fully applied to
its arguments, including the correct transaction context. Use the same ledger
and Plutus versions as the checkout. A serialized validator alone does not
include the context needed to profile a particular transaction.

Follow the [Plutus profiling guide](https://plutus.cardano.intersectmbo.org/docs/working-with-scripts/profiling-budget-usage)
to install matching `uplc` and `traceToStacks` executables. With a fully applied
Flat-encoded script and `flamegraph.pl` on your `PATH`:

```shell
uplc evaluate -i fully-applied-script.flat --if flat --trace-mode LogsWithBudgets -o logs
traceToStacks < logs | flamegraph.pl > cpu.svg
traceToStacks --column 2 < logs | flamegraph.pl > mem.svg
```

Select the input format matching the harness's export; named De Bruijn exports
use `--if flat-namedDeBruijn`. Open the generated SVGs in a browser to inspect
individual functions. Regenerate graphs when the transaction fixture,
contracts, or compiler changes, and remove profiling flags before comparing
release artifacts.
