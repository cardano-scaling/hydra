# local-cluster

Command line executable and integration test suite spinning up a local cluster of Cardano and Hydra nodes.

This is used to experiment and explore the design space for the Hydra node, how it interacts with Cardano nodes and have an always available, ad-hoc environment for demos.

## End-to-end Scenario

For now, we use a static scenario in which we first develop the Hydra node:

- 3 Cardano BFT nodes
- using sped up network parameters
- directly forking into Allegra and Mary

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
