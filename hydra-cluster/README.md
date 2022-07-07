Integration test suite spinning up a local cluster of Hydra nodes on a "devnet".

For simplicity, we usually would spin up a trivial Cardano network comprised by
only a single block producing node, having all the stake. This is of course not
exactly how a proper Cardano network would work, but the scope of this package
and it's tests are to assert correct hydra-node behavior in a cluster of Hydra
nodes.

## Benchmarks

The benchmark can be run using `cabal bench` and produces a `results.csv` file. To plot the transaction confirmation times you can use the `bench/plot.sh` script, passing it the directory containing the benchmark's results.

To run and plot results of the benchmark:
``` sh
$ cabal bench
Running 1 benchmarks...
Benchmark bench-e2e: RUNNING...
Writing transactions to: /run/user/1000/bench-83d18973f95a554d/txs.json
[...]
Writing results to: /run/user/1000/bench-6b772589d08f82a5/results.csv
Benchmark bench-e2e: FINISH
$ bench/plot.sh /run/user/1000/bench-6b772589d08f82a5/results.csv
Created plot: /run/user/1000/bench-6b772589d08f82a5/results.png
```

The benchmark provides several options for running, check `cabal bench --benchmark-options --help` for details.

See [this README](./bench/README.md) for more details on benchmark run results interpretation.
