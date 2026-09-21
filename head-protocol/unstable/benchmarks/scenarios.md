--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-21 09:54:18.148759636 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 990.06 | n/a | 29.5 | 30.1 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 202.06 | 204.95 | 4.9 | 6.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 927.38 | n/a | 31.5 | 32.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 164.15 | 164.00 | 6.0 | 6.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 992.77 | n/a | 29.4 | 29.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 183.01 | 182.69 | 5.4 | 6.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 873.66 | n/a | 67.2 | 67.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 138.60 | 138.48 | 14.1 | 19.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 668.98 | n/a | 88.0 | 88.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 98.41 | 98.43 | 20.0 | 26.9 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 784.81 | n/a | 74.7 | 75.5 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 102.78 | 97.89 | 19.2 | 27.0 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 739.37 | n/a | 117.7 | 119.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 108.99 | 108.02 | 27.2 | 38.6 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 536.18 | n/a | 165.0 | 167.3 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 84.32 | 82.54 | 35.2 | 50.6 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 572.96 | n/a | 153.8 | 156.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.34 | 87.15 | 33.0 | 44.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.5 |
| _P99_ | 30.2ms |
| _P95_ | 30.1ms |
| _P50_ | 29.7ms |
| _Tx validation time p50 (ms)_ | 11.6 |
| _End-to-end TPS_ | 990.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 66.00 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.9 |
| _P99_ | 7.1ms |
| _P95_ | 6.7ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 202.06 tx/s |
| _Sustained TPS_ | 204.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 202.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.5 |
| _P99_ | 32.1ms |
| _P95_ | 32.1ms |
| _P50_ | 31.7ms |
| _Tx validation time p50 (ms)_ | 10.2 |
| _End-to-end TPS_ | 927.38 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.83 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 9.3ms |
| _P95_ | 6.9ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 164.15 tx/s |
| _Sustained TPS_ | 164.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 164.15 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.4 |
| _P99_ | 30.0ms |
| _P95_ | 29.9ms |
| _P50_ | 29.6ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 992.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 66.18 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 6.9ms |
| _P95_ | 6.5ms |
| _P50_ | 5.3ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 183.01 tx/s |
| _Sustained TPS_ | 182.69 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 183.01 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 67.2 |
| _P99_ | 68.2ms |
| _P95_ | 67.8ms |
| _P50_ | 67.5ms |
| _Tx validation time p50 (ms)_ | 26.2 |
| _End-to-end TPS_ | 873.66 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.12 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.1 |
| _P99_ | 20.2ms |
| _P95_ | 19.5ms |
| _P50_ | 13.6ms |
| _Tx validation time p50 (ms)_ | 4.2 |
| _End-to-end TPS_ | 138.60 tx/s |
| _Sustained TPS_ | 138.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 138.60 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 88.0 |
| _P99_ | 88.9ms |
| _P95_ | 88.9ms |
| _P50_ | 88.5ms |
| _Tx validation time p50 (ms)_ | 38.6 |
| _End-to-end TPS_ | 668.98 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.30 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.0 |
| _P99_ | 31.1ms |
| _P95_ | 26.9ms |
| _P50_ | 19.5ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 98.41 tx/s |
| _Sustained TPS_ | 98.43 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 98.41 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 74.7 |
| _P99_ | 75.5ms |
| _P95_ | 75.5ms |
| _P50_ | 75.1ms |
| _Tx validation time p50 (ms)_ | 23.6 |
| _End-to-end TPS_ | 784.81 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.16 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.2 |
| _P99_ | 27.7ms |
| _P95_ | 27.0ms |
| _P50_ | 18.8ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 102.78 tx/s |
| _Sustained TPS_ | 97.89 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 102.78 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 117.7 |
| _P99_ | 119.4ms |
| _P95_ | 119.3ms |
| _P50_ | 118.1ms |
| _Tx validation time p50 (ms)_ | 47.2 |
| _End-to-end TPS_ | 739.37 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.43 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.2 |
| _P99_ | 45.7ms |
| _P95_ | 38.6ms |
| _P50_ | 26.4ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 108.99 tx/s |
| _Sustained TPS_ | 108.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 73.87 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 165.0 |
| _P99_ | 167.4ms |
| _P95_ | 167.3ms |
| _P50_ | 165.1ms |
| _Tx validation time p50 (ms)_ | 46.7 |
| _End-to-end TPS_ | 536.18 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.92 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.2 |
| _P99_ | 55.2ms |
| _P95_ | 50.6ms |
| _P50_ | 34.0ms |
| _Tx validation time p50 (ms)_ | 10.9 |
| _End-to-end TPS_ | 84.32 tx/s |
| _Sustained TPS_ | 82.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 58.09 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 153.8 |
| _P99_ | 156.8ms |
| _P95_ | 156.7ms |
| _P50_ | 153.9ms |
| _Tx validation time p50 (ms)_ | 45.0 |
| _End-to-end TPS_ | 572.96 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.73 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.0 |
| _P99_ | 50.2ms |
| _P95_ | 44.5ms |
| _P50_ | 32.8ms |
| _Tx validation time p50 (ms)_ | 9.3 |
| _End-to-end TPS_ | 88.34 tx/s |
| _Sustained TPS_ | 87.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 63 |
| _Snapshots per second_ | 61.83 /s |
| _Avg txs per snapshot_ | 1.4 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
