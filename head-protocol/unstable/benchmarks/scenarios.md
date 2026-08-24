--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-24 14:45:25.274642594 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 861.72 | n/a | 34.1 | 34.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 195.06 | 195.23 | 5.1 | 6.6 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 802.20 | n/a | 36.6 | 37.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 154.76 | 152.99 | 6.4 | 8.5 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 884.66 | n/a | 32.9 | 33.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 161.85 | 154.07 | 6.1 | 7.8 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 941.21 | n/a | 61.9 | 63.5 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 122.26 | 120.16 | 16.2 | 20.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 719.13 | n/a | 81.0 | 82.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 84.25 | 83.76 | 23.4 | 34.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 862.38 | n/a | 67.8 | 69.3 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 95.66 | 92.26 | 20.7 | 30.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 690.30 | n/a | 127.6 | 130.1 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 101.10 | 101.59 | 29.1 | 38.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 543.70 | n/a | 161.8 | 165.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.3 | 69.00 | 68.78 | 42.7 | 57.7 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 656.04 | n/a | 132.5 | 136.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.2 | 77.83 | 75.48 | 37.6 | 51.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.1 |
| _P99_ | 34.6ms |
| _P95_ | 34.5ms |
| _P50_ | 34.3ms |
| _Tx validation time p50 (ms)_ | 19.3 |
| _End-to-end TPS_ | 861.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 57.45 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 7.2ms |
| _P95_ | 6.6ms |
| _P50_ | 4.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 195.06 tx/s |
| _Sustained TPS_ | 195.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 195.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.6 |
| _P99_ | 37.2ms |
| _P95_ | 37.1ms |
| _P50_ | 36.9ms |
| _Tx validation time p50 (ms)_ | 18.5 |
| _End-to-end TPS_ | 802.20 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 53.48 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.4 |
| _P99_ | 11.0ms |
| _P95_ | 8.5ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 154.76 tx/s |
| _Sustained TPS_ | 152.99 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 154.76 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.9 |
| _P99_ | 33.7ms |
| _P95_ | 33.7ms |
| _P50_ | 33.3ms |
| _Tx validation time p50 (ms)_ | 12.0 |
| _End-to-end TPS_ | 884.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.98 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 19.1ms |
| _P95_ | 7.8ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 161.85 tx/s |
| _Sustained TPS_ | 154.07 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 161.85 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 61.9 |
| _P99_ | 63.5ms |
| _P95_ | 63.5ms |
| _P50_ | 62.2ms |
| _Tx validation time p50 (ms)_ | 21.7 |
| _End-to-end TPS_ | 941.21 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.37 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.2 |
| _P99_ | 25.7ms |
| _P95_ | 20.9ms |
| _P50_ | 15.7ms |
| _Tx validation time p50 (ms)_ | 4.5 |
| _End-to-end TPS_ | 122.26 tx/s |
| _Sustained TPS_ | 120.16 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 122.26 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.0 |
| _P99_ | 82.9ms |
| _P95_ | 82.5ms |
| _P50_ | 81.3ms |
| _Tx validation time p50 (ms)_ | 29.7 |
| _End-to-end TPS_ | 719.13 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.97 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.4 |
| _P99_ | 37.0ms |
| _P95_ | 34.2ms |
| _P50_ | 22.0ms |
| _Tx validation time p50 (ms)_ | 7.0 |
| _End-to-end TPS_ | 84.25 tx/s |
| _Sustained TPS_ | 83.76 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 84.25 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 67.8 |
| _P99_ | 69.4ms |
| _P95_ | 69.3ms |
| _P50_ | 68.2ms |
| _Tx validation time p50 (ms)_ | 21.1 |
| _End-to-end TPS_ | 862.38 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.75 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.7 |
| _P99_ | 32.5ms |
| _P95_ | 30.6ms |
| _P50_ | 19.5ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 95.66 tx/s |
| _Sustained TPS_ | 92.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 95.66 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 127.6 |
| _P99_ | 130.2ms |
| _P95_ | 130.1ms |
| _P50_ | 127.5ms |
| _Tx validation time p50 (ms)_ | 46.7 |
| _End-to-end TPS_ | 690.30 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.34 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.1 |
| _P99_ | 43.3ms |
| _P95_ | 38.9ms |
| _P50_ | 28.3ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 101.10 tx/s |
| _Sustained TPS_ | 101.59 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 68.52 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 161.8 |
| _P99_ | 165.3ms |
| _P95_ | 165.2ms |
| _P50_ | 161.6ms |
| _Tx validation time p50 (ms)_ | 60.2 |
| _End-to-end TPS_ | 543.70 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.08 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 42.7 |
| _P99_ | 61.0ms |
| _P95_ | 57.7ms |
| _P50_ | 41.5ms |
| _Tx validation time p50 (ms)_ | 12.3 |
| _End-to-end TPS_ | 69.00 tx/s |
| _Sustained TPS_ | 68.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 46.77 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 132.5 |
| _P99_ | 136.9ms |
| _P95_ | 136.8ms |
| _P50_ | 132.6ms |
| _Tx validation time p50 (ms)_ | 70.0 |
| _End-to-end TPS_ | 656.04 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.58 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 37.6 |
| _P99_ | 57.4ms |
| _P95_ | 51.6ms |
| _P50_ | 37.1ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 77.83 tx/s |
| _Sustained TPS_ | 75.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 53.62 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
