--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-26 12:39:52.066244128 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1374.66 | n/a | 21.1 | 21.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 1.0 | 30.07 | 24.80 | 33.2 | 95.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1416.32 | n/a | 20.6 | 21.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 209.25 | 208.46 | 4.7 | 5.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1070.42 | n/a | 27.5 | 27.8 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.4 | 69.21 | 59.27 | 14.4 | 83.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 451.04 | n/a | 131.5 | 132.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 138.88 | 136.77 | 14.3 | 33.6 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 1124.17 | n/a | 52.0 | 53.1 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 109.04 | 114.58 | 18.1 | 35.9 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 809.81 | n/a | 72.7 | 73.3 |
| Nodes=2, Mixed, wait for tx valid | 60 | 2.4 | 24.56 | 24.09 | 81.3 | 337.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 812.25 | n/a | 109.1 | 109.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.4 | 66.37 | 61.11 | 44.5 | 115.0 |
| Nodes=3, Growing, fire and forget | 90 | 0.4 | 218.33 | n/a | 353.2 | 411.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 89.88 | 88.37 | 32.8 | 54.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 827.51 | n/a | 106.8 | 107.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 100.98 | 106.76 | 28.9 | 44.3 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 21.1 |
| _P99_ | 21.6ms |
| _P95_ | 21.6ms |
| _P50_ | 21.3ms |
| _Tx validation time p50 (ms)_ | 8.6 |
| _End-to-end TPS_ | 1374.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 91.64 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.2 |
| _P99_ | 450.8ms |
| _P95_ | 95.2ms |
| _P50_ | 3.8ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 30.07 tx/s |
| _Sustained TPS_ | 24.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 30.07 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 20.6 |
| _P99_ | 21.0ms |
| _P95_ | 21.0ms |
| _P50_ | 20.8ms |
| _Tx validation time p50 (ms)_ | 13.5 |
| _End-to-end TPS_ | 1416.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 94.42 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.7 |
| _P99_ | 6.4ms |
| _P95_ | 5.4ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 209.25 tx/s |
| _Sustained TPS_ | 208.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 209.25 /s |
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
| _Avg. Confirmation Time (ms)_ | 27.5 |
| _P99_ | 27.8ms |
| _P95_ | 27.8ms |
| _P50_ | 27.6ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 1070.42 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 71.36 /s |
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
| _Avg. Confirmation Time (ms)_ | 14.4 |
| _P99_ | 91.2ms |
| _P95_ | 83.4ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 69.21 tx/s |
| _Sustained TPS_ | 59.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 69.21 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 131.5 |
| _P99_ | 132.8ms |
| _P95_ | 132.8ms |
| _P50_ | 131.6ms |
| _Tx validation time p50 (ms)_ | 22.0 |
| _End-to-end TPS_ | 451.04 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.03 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.3 |
| _P99_ | 64.9ms |
| _P95_ | 33.6ms |
| _P50_ | 10.9ms |
| _Tx validation time p50 (ms)_ | 2.8 |
| _End-to-end TPS_ | 138.88 tx/s |
| _Sustained TPS_ | 136.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 138.88 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 52.0 |
| _P99_ | 53.1ms |
| _P95_ | 53.1ms |
| _P50_ | 52.2ms |
| _Tx validation time p50 (ms)_ | 19.8 |
| _End-to-end TPS_ | 1124.17 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 37.47 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.1 |
| _P99_ | 37.5ms |
| _P95_ | 35.9ms |
| _P50_ | 15.7ms |
| _Tx validation time p50 (ms)_ | 4.8 |
| _End-to-end TPS_ | 109.04 tx/s |
| _Sustained TPS_ | 114.58 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 109.04 /s |
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
| _Avg. Confirmation Time (ms)_ | 72.7 |
| _P99_ | 73.6ms |
| _P95_ | 73.3ms |
| _P50_ | 73.0ms |
| _Tx validation time p50 (ms)_ | 29.4 |
| _End-to-end TPS_ | 809.81 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.99 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 81.3 |
| _P99_ | 451.9ms |
| _P95_ | 337.4ms |
| _P50_ | 15.1ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 24.56 tx/s |
| _Sustained TPS_ | 24.09 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 24.56 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 109.1 |
| _P99_ | 109.9ms |
| _P95_ | 109.8ms |
| _P50_ | 109.4ms |
| _Tx validation time p50 (ms)_ | 40.9 |
| _End-to-end TPS_ | 812.25 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.05 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 44.5 |
| _P99_ | 118.4ms |
| _P95_ | 115.0ms |
| _P50_ | 23.5ms |
| _Tx validation time p50 (ms)_ | 6.4 |
| _End-to-end TPS_ | 66.37 tx/s |
| _Sustained TPS_ | 61.11 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 63 |
| _Snapshots per second_ | 46.46 /s |
| _Avg txs per snapshot_ | 1.4 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 353.2 |
| _P99_ | 411.0ms |
| _P95_ | 411.0ms |
| _P50_ | 328.0ms |
| _Tx validation time p50 (ms)_ | 174.9 |
| _End-to-end TPS_ | 218.33 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 4.85 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.8 |
| _P99_ | 60.0ms |
| _P95_ | 54.5ms |
| _P50_ | 29.6ms |
| _Tx validation time p50 (ms)_ | 8.8 |
| _End-to-end TPS_ | 89.88 tx/s |
| _Sustained TPS_ | 88.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 63 |
| _Snapshots per second_ | 62.92 /s |
| _Avg txs per snapshot_ | 1.4 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 106.8 |
| _P99_ | 108.0ms |
| _P95_ | 107.8ms |
| _P50_ | 107.4ms |
| _Tx validation time p50 (ms)_ | 46.4 |
| _End-to-end TPS_ | 827.51 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.39 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.9 |
| _P99_ | 50.1ms |
| _P95_ | 44.3ms |
| _P50_ | 26.3ms |
| _Tx validation time p50 (ms)_ | 7.0 |
| _End-to-end TPS_ | 100.98 tx/s |
| _Sustained TPS_ | 106.76 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 69.56 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
