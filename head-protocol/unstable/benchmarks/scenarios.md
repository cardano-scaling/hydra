--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-14 10:41:26.130734551 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 838.66 | n/a | 35.0 | 35.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 197.03 | 195.55 | 5.0 | 6.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 932.06 | n/a | 31.3 | 32.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 165.24 | 165.86 | 6.0 | 7.3 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 867.47 | n/a | 33.2 | 34.0 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 175.29 | 177.67 | 5.6 | 7.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 893.73 | n/a | 65.0 | 66.2 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 132.96 | 132.44 | 14.9 | 19.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 687.28 | n/a | 85.6 | 86.4 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 92.37 | 91.19 | 21.3 | 28.1 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 899.52 | n/a | 64.6 | 65.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 105.59 | 102.39 | 18.7 | 24.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 601.53 | n/a | 145.8 | 149.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 112.66 | 111.88 | 26.2 | 32.5 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 536.72 | n/a | 163.4 | 165.8 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 85.95 | 84.78 | 34.6 | 41.3 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 574.07 | n/a | 153.6 | 155.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 89.15 | 87.14 | 33.0 | 43.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 35.0 |
| _P99_ | 35.5ms |
| _P95_ | 35.5ms |
| _P50_ | 35.2ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 838.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.91 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.0 |
| _P99_ | 7.0ms |
| _P95_ | 6.7ms |
| _P50_ | 4.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 197.03 tx/s |
| _Sustained TPS_ | 195.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 197.03 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.3 |
| _P99_ | 32.1ms |
| _P95_ | 32.1ms |
| _P50_ | 31.7ms |
| _Tx validation time p50 (ms)_ | 19.3 |
| _End-to-end TPS_ | 932.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.14 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 7.9ms |
| _P95_ | 7.3ms |
| _P50_ | 6.0ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 165.24 tx/s |
| _Sustained TPS_ | 165.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 165.24 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.2 |
| _P99_ | 34.3ms |
| _P95_ | 34.0ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 12.0 |
| _End-to-end TPS_ | 867.47 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 57.83 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 7.7ms |
| _P95_ | 7.7ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 175.29 tx/s |
| _Sustained TPS_ | 177.67 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 175.29 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 65.0 |
| _P99_ | 66.3ms |
| _P95_ | 66.2ms |
| _P50_ | 65.0ms |
| _Tx validation time p50 (ms)_ | 20.4 |
| _End-to-end TPS_ | 893.73 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.79 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.9 |
| _P99_ | 20.2ms |
| _P95_ | 19.5ms |
| _P50_ | 14.6ms |
| _Tx validation time p50 (ms)_ | 4.2 |
| _End-to-end TPS_ | 132.96 tx/s |
| _Sustained TPS_ | 132.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 132.96 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 85.6 |
| _P99_ | 86.5ms |
| _P95_ | 86.4ms |
| _P50_ | 86.0ms |
| _Tx validation time p50 (ms)_ | 31.6 |
| _End-to-end TPS_ | 687.28 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.91 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.3 |
| _P99_ | 31.0ms |
| _P95_ | 28.1ms |
| _P50_ | 20.9ms |
| _Tx validation time p50 (ms)_ | 6.1 |
| _End-to-end TPS_ | 92.37 tx/s |
| _Sustained TPS_ | 91.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 92.37 /s |
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
| _Avg. Confirmation Time (ms)_ | 64.6 |
| _P99_ | 66.2ms |
| _P95_ | 65.7ms |
| _P50_ | 65.1ms |
| _Tx validation time p50 (ms)_ | 24.8 |
| _End-to-end TPS_ | 899.52 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.98 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.7 |
| _P99_ | 26.7ms |
| _P95_ | 24.6ms |
| _P50_ | 18.2ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 105.59 tx/s |
| _Sustained TPS_ | 102.39 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 105.59 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 145.8 |
| _P99_ | 149.3ms |
| _P95_ | 149.2ms |
| _P50_ | 145.9ms |
| _Tx validation time p50 (ms)_ | 54.1 |
| _End-to-end TPS_ | 601.53 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.37 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.2 |
| _P99_ | 35.9ms |
| _P95_ | 32.5ms |
| _P50_ | 26.0ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 112.66 tx/s |
| _Sustained TPS_ | 111.88 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 76.36 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 163.4 |
| _P99_ | 166.2ms |
| _P95_ | 165.8ms |
| _P50_ | 165.0ms |
| _Tx validation time p50 (ms)_ | 58.1 |
| _End-to-end TPS_ | 536.72 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.93 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.6 |
| _P99_ | 49.4ms |
| _P95_ | 41.3ms |
| _P50_ | 34.5ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 85.95 tx/s |
| _Sustained TPS_ | 84.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 58.26 /s |
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
| _Avg. Confirmation Time (ms)_ | 153.6 |
| _P99_ | 155.8ms |
| _P95_ | 155.8ms |
| _P50_ | 154.8ms |
| _Tx validation time p50 (ms)_ | 81.9 |
| _End-to-end TPS_ | 574.07 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.76 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.0 |
| _P99_ | 46.7ms |
| _P95_ | 43.8ms |
| _P50_ | 32.6ms |
| _Tx validation time p50 (ms)_ | 9.4 |
| _End-to-end TPS_ | 89.15 tx/s |
| _Sustained TPS_ | 87.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.42 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
