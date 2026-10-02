--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-02 10:54:08.133170042 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 938.66 | n/a | 31.3 | 31.7 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 196.97 | 198.17 | 5.0 | 6.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 799.23 | n/a | 36.7 | 37.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 152.46 | 157.65 | 6.5 | 9.0 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 664.15 | n/a | 44.4 | 44.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 175.37 | 171.84 | 5.6 | 6.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 897.33 | n/a | 65.0 | 66.5 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 131.93 | 132.76 | 15.0 | 19.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 742.24 | n/a | 78.4 | 79.8 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 103.73 | 102.24 | 19.0 | 22.9 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 798.03 | n/a | 73.6 | 74.8 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 103.04 | 99.11 | 19.2 | 24.0 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 710.80 | n/a | 123.5 | 126.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 110.31 | 108.50 | 26.7 | 34.7 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 536.25 | n/a | 163.8 | 167.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 84.33 | 84.02 | 34.9 | 41.4 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 584.96 | n/a | 150.2 | 152.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 90.62 | 88.72 | 32.7 | 40.9 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.3 |
| _P99_ | 31.8ms |
| _P95_ | 31.7ms |
| _P50_ | 31.5ms |
| _Tx validation time p50 (ms)_ | 13.1 |
| _End-to-end TPS_ | 938.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.58 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 141.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.0 |
| _P99_ | 6.8ms |
| _P95_ | 6.2ms |
| _P50_ | 4.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 196.97 tx/s |
| _Sustained TPS_ | 198.17 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 196.97 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.7 |
| _P99_ | 37.3ms |
| _P95_ | 37.3ms |
| _P50_ | 37.0ms |
| _Tx validation time p50 (ms)_ | 13.1 |
| _End-to-end TPS_ | 799.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 53.28 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 132.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.5 |
| _P99_ | 11.8ms |
| _P95_ | 9.0ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 152.46 tx/s |
| _Sustained TPS_ | 157.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 152.46 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 44.4 |
| _P99_ | 45.0ms |
| _P95_ | 44.9ms |
| _P50_ | 44.6ms |
| _Tx validation time p50 (ms)_ | 19.2 |
| _End-to-end TPS_ | 664.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 44.28 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 8.0ms |
| _P95_ | 6.7ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 175.37 tx/s |
| _Sustained TPS_ | 171.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 175.37 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 65.0 |
| _P99_ | 66.7ms |
| _P95_ | 66.5ms |
| _P50_ | 65.5ms |
| _Tx validation time p50 (ms)_ | 28.5 |
| _End-to-end TPS_ | 897.33 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.91 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.0 |
| _P99_ | 21.1ms |
| _P95_ | 19.9ms |
| _P50_ | 14.2ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 131.93 tx/s |
| _Sustained TPS_ | 132.76 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 131.93 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 135.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 78.4 |
| _P99_ | 80.3ms |
| _P95_ | 79.8ms |
| _P50_ | 79.2ms |
| _Tx validation time p50 (ms)_ | 22.8 |
| _End-to-end TPS_ | 742.24 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.74 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.0 |
| _P99_ | 24.2ms |
| _P95_ | 22.9ms |
| _P50_ | 19.4ms |
| _Tx validation time p50 (ms)_ | 6.7 |
| _End-to-end TPS_ | 103.73 tx/s |
| _Sustained TPS_ | 102.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 103.73 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 134.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 73.6 |
| _P99_ | 74.9ms |
| _P95_ | 74.8ms |
| _P50_ | 73.9ms |
| _Tx validation time p50 (ms)_ | 27.1 |
| _End-to-end TPS_ | 798.03 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.60 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.2 |
| _P99_ | 25.7ms |
| _P95_ | 24.0ms |
| _P50_ | 19.5ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 103.04 tx/s |
| _Sustained TPS_ | 99.11 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 103.04 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 123.5 |
| _P99_ | 126.3ms |
| _P95_ | 126.2ms |
| _P50_ | 124.1ms |
| _Tx validation time p50 (ms)_ | 56.4 |
| _End-to-end TPS_ | 710.80 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.80 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.7 |
| _P99_ | 38.3ms |
| _P95_ | 34.7ms |
| _P50_ | 26.3ms |
| _Tx validation time p50 (ms)_ | 7.5 |
| _End-to-end TPS_ | 110.31 tx/s |
| _Sustained TPS_ | 108.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 74.77 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 163.8 |
| _P99_ | 167.6ms |
| _P95_ | 167.5ms |
| _P50_ | 164.4ms |
| _Tx validation time p50 (ms)_ | 78.9 |
| _End-to-end TPS_ | 536.25 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.92 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.9 |
| _P99_ | 43.7ms |
| _P95_ | 41.4ms |
| _P50_ | 35.2ms |
| _Tx validation time p50 (ms)_ | 10.1 |
| _End-to-end TPS_ | 84.33 tx/s |
| _Sustained TPS_ | 84.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.15 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 150.2 |
| _P99_ | 152.8ms |
| _P95_ | 152.7ms |
| _P50_ | 152.1ms |
| _Tx validation time p50 (ms)_ | 51.1 |
| _End-to-end TPS_ | 584.96 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.00 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.7 |
| _P99_ | 48.4ms |
| _P95_ | 40.9ms |
| _P50_ | 32.7ms |
| _Tx validation time p50 (ms)_ | 9.8 |
| _End-to-end TPS_ | 90.62 tx/s |
| _Sustained TPS_ | 88.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 61.42 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
