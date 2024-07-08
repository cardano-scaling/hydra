---
sidebar_position: 10
---

# Profiling Hydra scripts

This tutorial explains how to profile Hydra scripts and is intended for contributors to the `hydra-node`.

## Overview

For every PR and the latest `master` branch, we compute typical transaction costs in terms of size, memory, and CPU usage of the Hydra protocol transactions on Cardano. You can view the latest results [here](https://hydra.family/head-protocol/benchmarks/transaction-cost/).

Such benchmarks provide a comprehensive overview of the constraints for a given transaction, including maximum transaction size, and percent of maximum memory and CPU budget. For a detailed assessment, we analyze _all_ scripts that run within a given transaction.

To gain detailed insights into _what exactly_ results in excessive memory or CPU usage, we need to profile the scripts as they validate a transaction.

Follow the instructions provided by the [`plutus`](https://github.com/input-output-hk/plutus) project [here](https://plutus.readthedocs.io/en/latest/howtos/profiling-scripts.html), adapted for the `hydra` codebase.


## Isolate a transaction to profile

First, isolate the specific Cardano `TX` you want to profile. For example, let's investigate what the `collectCom` transaction
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

Here, isolate the transaction for `5` parties by altering the function to `maybe [] pure <$> compute 5`.

## Compiling a script for profiling

The `collectCom` transaction utilizes the `vCommit` and `vHead` validator scripts. To enable profiling, add the following directive to the modules [`Hydra.Contract.Commit`](/haddock/hydra-plutus/Hydra-Contract-Commit.html) and [`Hydra.Contract.Head`](/haddock/hydra-plutus/Hydra-Contract-Head.html):

```
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
```


## Acquiring an executable script

This can be achieved using
[`prepareTxScripts`](/haddock/hydra-node/Hydra-Ledger-Cardano-Evaluate.html#v:prepareTxScripts).
to acquire and save the fully applied scripts from the transaction onto disk:

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

To perform this step, use the following tools available through Nix:

```
nix shell nixpkgs#flamegraph github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-924.hsPkgs.plutus-core.components.exes.traceToStacks github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-924.hsPkgs.plutus-core.components.exes.uplc
```

To produce the profile log as explained upstream we need to use a different
input format as `prepareTxScripts` retains the original name annotations.

```
uplc evaluate -t -i scripts-1.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs
```

Check for a `logs` file output. If not present, ensure the script was compiled with profiling enabled as specified.

Finally, you can inspect the logs or generate flamegraph SVGs as outlined in the original tutorial:

```
cat logs | traceToStacks | flamegraph.pl > cpu.svg
cat logs | traceToStacks --column 2 | flamegraph.pl > mem.svg
```

Here's an example of a memory profile for a `5` party `collectCom`:

![](profile-mem.svg)

:::tip
Open the SVG in a browser to interactively search and drill down through the profile.
:::