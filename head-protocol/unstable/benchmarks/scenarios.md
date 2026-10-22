--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-22 18:28:35.677928481 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1001.07 | n/a | 29.3 | 29.7 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 199.11 | 200.66 | 5.0 | 6.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 714.29 | n/a | 40.9 | 41.8 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 169.42 | 170.98 | 5.8 | 6.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 937.48 | n/a | 31.1 | 31.8 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.3 | 110.28 | 107.54 | 9.0 | 12.6 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 795.52 | n/a | 73.8 | 74.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 129.72 | 132.24 | 15.2 | 20.0 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 679.87 | n/a | 86.0 | 88.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.8 | 77.48 | 75.70 | 25.2 | 36.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 759.77 | n/a | 77.2 | 78.1 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 107.74 | 102.66 | 18.4 | 23.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 688.68 | n/a | 126.8 | 130.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 107.98 | 109.53 | 27.1 | 33.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 547.62 | n/a | 160.5 | 163.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 87.99 | 87.37 | 33.8 | 40.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 613.90 | n/a | 143.3 | 145.3 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 91.68 | 89.73 | 32.5 | 40.2 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.3 |
| _P99_ | 29.8ms |
| _P95_ | 29.7ms |
| _P50_ | 29.5ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 1001.07 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 66.74 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 135.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.0 |
| _P99_ | 7.1ms |
| _P95_ | 6.2ms |
| _P50_ | 4.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 199.11 tx/s |
| _Sustained TPS_ | 200.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 199.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 40.9 |
| _P99_ | 41.8ms |
| _P95_ | 41.8ms |
| _P50_ | 41.5ms |
| _Tx validation time p50 (ms)_ | 10.2 |
| _End-to-end TPS_ | 714.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 47.62 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 7.1ms |
| _P95_ | 6.9ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 169.42 tx/s |
| _Sustained TPS_ | 170.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 169.42 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 135.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.1 |
| _P99_ | 31.8ms |
| _P95_ | 31.8ms |
| _P50_ | 31.3ms |
| _Tx validation time p50 (ms)_ | 11.8 |
| _End-to-end TPS_ | 937.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.50 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 9.0 |
| _P99_ | 14.8ms |
| _P95_ | 12.6ms |
| _P50_ | 8.8ms |
| _Tx validation time p50 (ms)_ | 2.7 |
| _End-to-end TPS_ | 110.28 tx/s |
| _Sustained TPS_ | 107.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 110.28 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 73.8 |
| _P99_ | 74.7ms |
| _P95_ | 74.6ms |
| _P50_ | 74.2ms |
| _Tx validation time p50 (ms)_ | 28.3 |
| _End-to-end TPS_ | 795.52 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.52 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 141.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.2 |
| _P99_ | 24.1ms |
| _P95_ | 20.0ms |
| _P50_ | 14.8ms |
| _Tx validation time p50 (ms)_ | 3.7 |
| _End-to-end TPS_ | 129.72 tx/s |
| _Sustained TPS_ | 132.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 129.72 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 86.0 |
| _P99_ | 88.1ms |
| _P95_ | 88.0ms |
| _P50_ | 86.4ms |
| _Tx validation time p50 (ms)_ | 29.7 |
| _End-to-end TPS_ | 679.87 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.66 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.2 |
| _P99_ | 41.2ms |
| _P95_ | 36.2ms |
| _P50_ | 25.6ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 77.48 tx/s |
| _Sustained TPS_ | 75.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 77.48 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 77.2 |
| _P99_ | 78.5ms |
| _P95_ | 78.1ms |
| _P50_ | 77.7ms |
| _Tx validation time p50 (ms)_ | 31.3 |
| _End-to-end TPS_ | 759.77 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.33 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 24.2ms |
| _P95_ | 23.1ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 107.74 tx/s |
| _Sustained TPS_ | 102.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 107.74 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 141.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 126.8 |
| _P99_ | 130.5ms |
| _P95_ | 130.4ms |
| _P50_ | 128.0ms |
| _Tx validation time p50 (ms)_ | 68.2 |
| _End-to-end TPS_ | 688.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.30 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.1 |
| _P99_ | 37.1ms |
| _P95_ | 33.9ms |
| _P50_ | 27.2ms |
| _Tx validation time p50 (ms)_ | 7.4 |
| _End-to-end TPS_ | 107.98 tx/s |
| _Sustained TPS_ | 109.53 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 73.18 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 160.5 |
| _P99_ | 163.7ms |
| _P95_ | 163.5ms |
| _P50_ | 160.8ms |
| _Tx validation time p50 (ms)_ | 54.1 |
| _End-to-end TPS_ | 547.62 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.17 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.8 |
| _P99_ | 42.3ms |
| _P95_ | 40.9ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 10.5 |
| _End-to-end TPS_ | 87.99 tx/s |
| _Sustained TPS_ | 87.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 58.66 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 143.3 |
| _P99_ | 145.6ms |
| _P95_ | 145.3ms |
| _P50_ | 144.4ms |
| _Tx validation time p50 (ms)_ | 51.1 |
| _End-to-end TPS_ | 613.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.64 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.5 |
| _P99_ | 44.2ms |
| _P95_ | 40.2ms |
| _P50_ | 32.7ms |
| _Tx validation time p50 (ms)_ | 8.9 |
| _End-to-end TPS_ | 91.68 tx/s |
| _Sustained TPS_ | 89.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 61.12 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
