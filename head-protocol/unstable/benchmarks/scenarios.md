--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-21 10:32:25.21128418 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1007.30 | n/a | 28.9 | 29.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 224.16 | 225.35 | 4.4 | 5.0 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1145.46 | n/a | 25.3 | 25.9 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 176.91 | 176.23 | 5.6 | 6.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1104.10 | n/a | 26.4 | 26.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 188.55 | 183.90 | 5.2 | 7.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1113.22 | n/a | 52.3 | 53.1 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 137.64 | 134.33 | 14.4 | 21.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 689.84 | n/a | 85.5 | 86.6 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 99.35 | 98.77 | 19.9 | 27.6 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 775.41 | n/a | 75.6 | 77.1 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 106.76 | 101.37 | 18.4 | 25.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 616.56 | n/a | 141.6 | 144.5 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 115.86 | 114.65 | 25.7 | 34.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 590.55 | n/a | 148.2 | 152.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 80.32 | 80.12 | 36.9 | 46.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 621.87 | n/a | 141.3 | 144.4 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 89.37 | 85.89 | 33.0 | 43.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 28.9 |
| _P99_ | 29.5ms |
| _P95_ | 29.5ms |
| _P50_ | 29.2ms |
| _Tx validation time p50 (ms)_ | 10.1 |
| _End-to-end TPS_ | 1007.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 67.15 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 4.4 |
| _P99_ | 6.6ms |
| _P95_ | 5.0ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 224.16 tx/s |
| _Sustained TPS_ | 225.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 224.16 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 25.3 |
| _P99_ | 25.9ms |
| _P95_ | 25.9ms |
| _P50_ | 25.6ms |
| _Tx validation time p50 (ms)_ | 16.2 |
| _End-to-end TPS_ | 1145.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 76.36 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 7.4ms |
| _P95_ | 6.4ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 176.91 tx/s |
| _Sustained TPS_ | 176.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 176.91 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 26.4 |
| _P99_ | 26.9ms |
| _P95_ | 26.9ms |
| _P50_ | 26.6ms |
| _Tx validation time p50 (ms)_ | 12.1 |
| _End-to-end TPS_ | 1104.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 73.61 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 7.8ms |
| _P95_ | 7.4ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 188.55 tx/s |
| _Sustained TPS_ | 183.90 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 188.55 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 52.3 |
| _P99_ | 53.1ms |
| _P95_ | 53.1ms |
| _P50_ | 52.4ms |
| _Tx validation time p50 (ms)_ | 24.6 |
| _End-to-end TPS_ | 1113.22 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 37.11 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 14.4 |
| _P99_ | 24.7ms |
| _P95_ | 21.8ms |
| _P50_ | 13.6ms |
| _Tx validation time p50 (ms)_ | 4.0 |
| _End-to-end TPS_ | 137.64 tx/s |
| _Sustained TPS_ | 134.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 137.64 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 85.5 |
| _P99_ | 86.7ms |
| _P95_ | 86.6ms |
| _P50_ | 85.8ms |
| _Tx validation time p50 (ms)_ | 41.8 |
| _End-to-end TPS_ | 689.84 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.99 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 19.9 |
| _P99_ | 29.9ms |
| _P95_ | 27.6ms |
| _P50_ | 19.5ms |
| _Tx validation time p50 (ms)_ | 6.0 |
| _End-to-end TPS_ | 99.35 tx/s |
| _Sustained TPS_ | 98.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.35 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 75.6 |
| _P99_ | 77.1ms |
| _P95_ | 77.1ms |
| _P50_ | 75.8ms |
| _Tx validation time p50 (ms)_ | 33.7 |
| _End-to-end TPS_ | 775.41 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.85 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 28.7ms |
| _P95_ | 25.4ms |
| _P50_ | 18.2ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 106.76 tx/s |
| _Sustained TPS_ | 101.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 106.76 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 141.6 |
| _P99_ | 144.6ms |
| _P95_ | 144.5ms |
| _P50_ | 141.0ms |
| _Tx validation time p50 (ms)_ | 50.9 |
| _End-to-end TPS_ | 616.56 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.70 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 25.7 |
| _P99_ | 37.6ms |
| _P95_ | 34.1ms |
| _P50_ | 24.6ms |
| _Tx validation time p50 (ms)_ | 7.8 |
| _End-to-end TPS_ | 115.86 tx/s |
| _Sustained TPS_ | 114.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 78.53 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 148.2 |
| _P99_ | 152.0ms |
| _P95_ | 152.0ms |
| _P50_ | 148.8ms |
| _Tx validation time p50 (ms)_ | 45.6 |
| _End-to-end TPS_ | 590.55 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.12 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 36.9 |
| _P99_ | 49.2ms |
| _P95_ | 46.5ms |
| _P50_ | 36.7ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 80.32 tx/s |
| _Sustained TPS_ | 80.12 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 54.44 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 141.3 |
| _P99_ | 144.5ms |
| _P95_ | 144.4ms |
| _P50_ | 142.0ms |
| _Tx validation time p50 (ms)_ | 53.4 |
| _End-to-end TPS_ | 621.87 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.82 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 33.0 |
| _P99_ | 46.2ms |
| _P95_ | 43.6ms |
| _P50_ | 32.7ms |
| _Tx validation time p50 (ms)_ | 8.9 |
| _End-to-end TPS_ | 89.37 tx/s |
| _Sustained TPS_ | 85.89 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.58 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
