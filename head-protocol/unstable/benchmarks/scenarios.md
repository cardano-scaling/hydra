--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-16 18:17:27.944567133 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.1 | 226.01 | n/a | 132.2 | 132.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 232.55 | 222.08 | 4.3 | 6.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.2 | 141.23 | n/a | 212.0 | 212.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.4 | 82.39 | 94.08 | 12.1 | 53.0 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 738.97 | n/a | 39.9 | 40.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.3 | 107.10 | 154.23 | 9.3 | 27.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 654.24 | n/a | 90.3 | 91.3 |
| Nodes=2, Constant, wait for tx valid | 60 | 1.1 | 56.57 | 57.91 | 35.2 | 203.0 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 633.65 | n/a | 82.6 | 94.6 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.8 | 77.04 | 68.72 | 25.8 | 76.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 442.63 | n/a | 134.5 | 135.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 2.0 | 30.12 | 26.05 | 66.3 | 317.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 960.74 | n/a | 92.0 | 93.0 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.1 | 83.84 | 100.01 | 35.5 | 100.0 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 846.62 | n/a | 104.7 | 105.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.2 | 75.92 | 71.58 | 39.2 | 109.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 866.98 | n/a | 102.2 | 103.1 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.4 | 62.24 | 62.16 | 47.5 | 160.3 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 132.2 |
| _P99_ | 132.5ms |
| _P95_ | 132.5ms |
| _P50_ | 132.3ms |
| _Tx validation time p50 (ms)_ | 124.3 |
| _End-to-end TPS_ | 226.01 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.07 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.3 |
| _P99_ | 17.9ms |
| _P95_ | 6.7ms |
| _P50_ | 3.3ms |
| _Tx validation time p50 (ms)_ | 1.2 |
| _End-to-end TPS_ | 232.55 tx/s |
| _Sustained TPS_ | 222.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 232.55 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 212.0 |
| _P99_ | 212.4ms |
| _P95_ | 212.3ms |
| _P50_ | 212.1ms |
| _Tx validation time p50 (ms)_ | 201.0 |
| _End-to-end TPS_ | 141.23 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.42 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 12.1 |
| _P99_ | 69.7ms |
| _P95_ | 53.0ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.2 |
| _End-to-end TPS_ | 82.39 tx/s |
| _Sustained TPS_ | 94.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 82.39 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 39.9 |
| _P99_ | 40.4ms |
| _P95_ | 40.4ms |
| _P50_ | 40.2ms |
| _Tx validation time p50 (ms)_ | 27.3 |
| _End-to-end TPS_ | 738.97 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 49.26 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 9.3 |
| _P99_ | 51.9ms |
| _P95_ | 27.4ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 107.10 tx/s |
| _Sustained TPS_ | 154.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 107.10 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 90.3 |
| _P99_ | 91.4ms |
| _P95_ | 91.3ms |
| _P50_ | 90.4ms |
| _Tx validation time p50 (ms)_ | 69.2 |
| _End-to-end TPS_ | 654.24 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.81 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.2 |
| _P99_ | 238.9ms |
| _P95_ | 203.0ms |
| _P50_ | 13.6ms |
| _Tx validation time p50 (ms)_ | 2.7 |
| _End-to-end TPS_ | 56.57 tx/s |
| _Sustained TPS_ | 57.91 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 56.57 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 82.6 |
| _P99_ | 94.6ms |
| _P95_ | 94.6ms |
| _P50_ | 72.2ms |
| _Tx validation time p50 (ms)_ | 45.9 |
| _End-to-end TPS_ | 633.65 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.12 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.8 |
| _P99_ | 93.6ms |
| _P95_ | 76.4ms |
| _P50_ | 16.4ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 77.04 tx/s |
| _Sustained TPS_ | 68.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 77.04 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 134.5 |
| _P99_ | 135.2ms |
| _P95_ | 135.0ms |
| _P50_ | 134.6ms |
| _Tx validation time p50 (ms)_ | 111.0 |
| _End-to-end TPS_ | 442.63 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.75 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 66.3 |
| _P99_ | 637.9ms |
| _P95_ | 317.6ms |
| _P50_ | 14.2ms |
| _Tx validation time p50 (ms)_ | 4.1 |
| _End-to-end TPS_ | 30.12 tx/s |
| _Sustained TPS_ | 26.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 30.12 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 92.0 |
| _P99_ | 93.0ms |
| _P95_ | 93.0ms |
| _P50_ | 92.1ms |
| _Tx validation time p50 (ms)_ | 39.9 |
| _End-to-end TPS_ | 960.74 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.35 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.5 |
| _P99_ | 131.4ms |
| _P95_ | 100.0ms |
| _P50_ | 20.4ms |
| _Tx validation time p50 (ms)_ | 4.8 |
| _End-to-end TPS_ | 83.84 tx/s |
| _Sustained TPS_ | 100.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 57.75 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 104.7 |
| _P99_ | 105.5ms |
| _P95_ | 105.5ms |
| _P50_ | 105.0ms |
| _Tx validation time p50 (ms)_ | 23.8 |
| _End-to-end TPS_ | 846.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.81 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 39.2 |
| _P99_ | 207.8ms |
| _P95_ | 109.0ms |
| _P50_ | 25.6ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 75.92 tx/s |
| _Sustained TPS_ | 71.58 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 51.46 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 102.2 |
| _P99_ | 103.2ms |
| _P95_ | 103.1ms |
| _P50_ | 102.8ms |
| _Tx validation time p50 (ms)_ | 45.7 |
| _End-to-end TPS_ | 866.98 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 19.27 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 47.5 |
| _P99_ | 188.9ms |
| _P95_ | 160.3ms |
| _P50_ | 26.3ms |
| _Tx validation time p50 (ms)_ | 6.0 |
| _End-to-end TPS_ | 62.24 tx/s |
| _Sustained TPS_ | 62.16 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 42.18 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
