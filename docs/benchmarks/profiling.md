---
sidebar_position: 10
---

# Profiling Hydra scripts

This is a quick tutorial how to profile Hydra scripts and is intended for
contributors to the `hydra-node`.

On every PR and also for the latest `master`, we do compute typical transaction
costs in size, memory and cpu usage of the Hydra protocol transactions on
Cardano. The latest results can be seen
[here](https://hydra.family/head-protocol/benchmarks/transaction-cost/).

Such benchmarks provide a great overview of what "fits" into a given transaction
in terms of maximum transaction size, percent of maximum memory and cpu budget.
For the latter, we evaluates _all_ the scripts which will run in a given
transaction.

To get more detailed insights of _what exactly_ is resulting in excessive memory
or cpu usage, we need to profile the scripts as they are validating a
transaction.

This guide follows the instructions provided upstream by the
[`plutus`](https://github.com/input-output-hk/plutus) project
[here](https://plutus.readthedocs.io/en/latest/howtos/profiling-scripts.html),
but points out how this can be done in the `hydra` code base.

## Isolate a transaction to profile

To do any measurements, we need to isolate the actual cardano `Tx` which is to
be profiled. For example, let's investigate what the `collectCom` transaction
for `5` parties in the `tx-cost` benchmark is spending most time and memory on.

The benchmark computes many transactions with growing number of participants in `computeCollectComCost`:
```haskell
computeCollectComCost =
  catMaybes <$> mapM compute [1 .. 100]
 where
  compute numParties = do
    (st, tx) <- generate $ genCollectComTx numParties
    let utxo = getKnownUTxO st
    case checkSizeAndEvaluate tx utxo of
      -- [...]
```

The `tx` here would be the transaction we want to profile, so we can "isolate"
the transaction for `5` parties by changing the body of this function `maybe []
pure <$> compute 5`.

## Compiling a script for profiling

The `collectCom` transaction runs `vCommit` and `vHead` validator scripts, so we
need to add

```
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
```

to the corresponding modules [`Hydra.Contract.Commit`](https://hydra.family/head-protocol/haddock/hydra-plutus/Hydra-Contract-Commit.html) and
[`Hydra.Contract.Head`](https://hydra.family/head-protocol/haddock/hydra-plutus/Hydra-Contract-Head.html).

## Acquiring an executable script

This can be now achieved using
[`prepareTxScripts`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Ledger-Cardano-Evaluate.html#v:prepareTxScripts).
We can use this function to acquire and dump the fully applied scripts from the
transaction onto disk.

```haskell
-- [...]
(st, tx) <- generate $ genCollectComTx numParties
let utxo = getKnownUTxO st
scripts <- either die pure $ prepareTxScripts tx utxo
forM_ (zip [1 ..] scripts) $ \(i, s) -> writeFileBS ("scripts-" <> show i <> ".flat") s
-- [...]
```

After running the corresponding code (`tx-cost` in our example), we will be left
with `scripts-{1,2,3,4,5}.flat` files in the current directory.

Unfortunately it's quite hard to tell them apart, but script sizes should help
in telling the big `vHead` script apart from the smaller `vCommit` script. In
the profile, names of original `plutus-tx` functions will be retained so that
should make it clear at the latest.

## Running the script & analysing the results

The tools for this step can be acquired using nix (or alternatively compile the
upstream projects and use your distribution's package manager):

```
nix shell nixpkgs#flamegraph github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-924.hsPkgs.plutus-core.components.exes.traceToStacks github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-924.hsPkgs.plutus-core.components.exes.uplc
```

To produce the profile log as explained upstream we need to use a different
input format as `prepareTxScripts` retains the original name annotations.

```
uplc evaluate -t -i scripts-1.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs
```

This should have produced `logs` file. If not, double check that you have
compiled the script with profiling options via the language pragma above.

At this stage, we can inspect the logs or produce the flamegraph SVGs exactly as
described in the original tutorial:

```
cat logs | traceToStacks | flamegraph.pl > cpu.svg
cat logs | traceToStacks --column 2 | flamegraph.pl > mem.svg
```

Here, for example the memory profile of a `5` party `collectCom` at the time of
writing:

![](profile-mem.svg)

:::tip
Open the SVG in a browser to search and drill-down through the profile
interactively.
:::
