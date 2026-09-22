--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-22 20:01:55.1278829 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1087.29 | n/a | 26.8 | 27.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 164.91 | 207.83 | 6.0 | 14.6 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 967.06 | n/a | 30.2 | 30.8 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 171.41 | 170.41 | 5.8 | 6.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 944.85 | n/a | 30.9 | 31.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 185.51 | 182.98 | 5.3 | 6.2 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 832.90 | n/a | 70.6 | 71.7 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 135.34 | 136.44 | 14.6 | 17.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 771.35 | n/a | 76.2 | 77.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 101.93 | 99.88 | 19.4 | 23.6 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 735.13 | n/a | 79.9 | 81.3 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 105.16 | 100.36 | 18.9 | 24.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 623.80 | n/a | 141.1 | 142.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 110.14 | 107.67 | 26.8 | 35.6 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 597.16 | n/a | 148.2 | 149.7 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 86.48 | 85.93 | 34.0 | 43.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 640.51 | n/a | 138.1 | 139.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 89.74 | 89.34 | 32.4 | 45.9 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.8 |
| _P99_ | 27.4ms |
| _P95_ | 27.3ms |
| _P50_ | 27.0ms |
| _Tx validation time p50 (ms)_ | 11.5 |
| _End-to-end TPS_ | 1087.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 72.49 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 24.4ms |
| _P95_ | 14.6ms |
| _P50_ | 4.5ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 164.91 tx/s |
| _Sustained TPS_ | 207.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 164.91 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.2 |
| _P99_ | 30.8ms |
| _P95_ | 30.8ms |
| _P50_ | 30.5ms |
| _Tx validation time p50 (ms)_ | 10.2 |
| _End-to-end TPS_ | 967.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 64.47 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 6.9ms |
| _P95_ | 6.6ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 171.41 tx/s |
| _Sustained TPS_ | 170.41 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 171.41 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.9 |
| _P99_ | 31.5ms |
| _P95_ | 31.5ms |
| _P50_ | 31.1ms |
| _Tx validation time p50 (ms)_ | 11.8 |
| _End-to-end TPS_ | 944.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.99 /s |
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
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 7.4ms |
| _P95_ | 6.2ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 185.51 tx/s |
| _Sustained TPS_ | 182.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 185.51 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.6 |
| _P99_ | 71.8ms |
| _P95_ | 71.7ms |
| _P50_ | 71.0ms |
| _Tx validation time p50 (ms)_ | 25.5 |
| _End-to-end TPS_ | 832.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.76 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.6 |
| _P99_ | 20.4ms |
| _P95_ | 17.8ms |
| _P50_ | 14.0ms |
| _Tx validation time p50 (ms)_ | 4.6 |
| _End-to-end TPS_ | 135.34 tx/s |
| _Sustained TPS_ | 136.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 135.34 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 76.2 |
| _P99_ | 77.6ms |
| _P95_ | 77.5ms |
| _P50_ | 76.6ms |
| _Tx validation time p50 (ms)_ | 27.7 |
| _End-to-end TPS_ | 771.35 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.71 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.4 |
| _P99_ | 24.5ms |
| _P95_ | 23.6ms |
| _P50_ | 19.4ms |
| _Tx validation time p50 (ms)_ | 6.2 |
| _End-to-end TPS_ | 101.93 tx/s |
| _Sustained TPS_ | 99.88 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 101.93 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.9 |
| _P99_ | 81.4ms |
| _P95_ | 81.3ms |
| _P50_ | 80.2ms |
| _Tx validation time p50 (ms)_ | 30.3 |
| _End-to-end TPS_ | 735.13 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.50 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.9 |
| _P99_ | 25.6ms |
| _P95_ | 24.1ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 105.16 tx/s |
| _Sustained TPS_ | 100.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 105.16 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 141.1 |
| _P99_ | 142.6ms |
| _P95_ | 142.4ms |
| _P50_ | 141.3ms |
| _Tx validation time p50 (ms)_ | 40.0 |
| _End-to-end TPS_ | 623.80 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.86 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.8 |
| _P99_ | 39.0ms |
| _P95_ | 35.6ms |
| _P50_ | 26.3ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 110.14 tx/s |
| _Sustained TPS_ | 107.67 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 74.65 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 148.2 |
| _P99_ | 149.9ms |
| _P95_ | 149.7ms |
| _P50_ | 149.2ms |
| _Tx validation time p50 (ms)_ | 49.4 |
| _End-to-end TPS_ | 597.16 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.27 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.0 |
| _P99_ | 52.1ms |
| _P95_ | 43.5ms |
| _P50_ | 33.9ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 86.48 tx/s |
| _Sustained TPS_ | 85.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 58.61 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 138.1 |
| _P99_ | 139.1ms |
| _P95_ | 139.0ms |
| _P50_ | 138.4ms |
| _Tx validation time p50 (ms)_ | 47.4 |
| _End-to-end TPS_ | 640.51 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.23 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.4 |
| _P99_ | 50.3ms |
| _P95_ | 45.9ms |
| _P50_ | 32.0ms |
| _Tx validation time p50 (ms)_ | 8.7 |
| _End-to-end TPS_ | 89.74 tx/s |
| _Sustained TPS_ | 89.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 61.82 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
