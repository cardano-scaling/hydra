--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-02 15:48:49.135536278 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 811.06 | n/a | 36.1 | 36.7 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 223.74 | 225.68 | 4.4 | 5.1 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 881.02 | n/a | 33.2 | 33.8 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 173.51 | 173.48 | 5.7 | 6.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 894.35 | n/a | 32.7 | 33.3 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 188.10 | 187.46 | 5.3 | 6.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 875.25 | n/a | 67.4 | 68.4 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 144.19 | 143.87 | 13.7 | 16.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 690.43 | n/a | 85.2 | 86.6 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 106.23 | 105.19 | 18.5 | 21.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 776.76 | n/a | 75.3 | 77.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 108.02 | 102.15 | 18.3 | 24.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 599.17 | n/a | 147.3 | 149.9 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 116.04 | 116.64 | 25.6 | 35.0 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 614.20 | n/a | 142.1 | 146.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 87.97 | 88.26 | 33.4 | 40.3 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 692.74 | n/a | 125.7 | 128.5 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 95.67 | 91.83 | 31.1 | 39.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.1 |
| _P99_ | 36.8ms |
| _P95_ | 36.7ms |
| _P50_ | 36.4ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 811.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 54.07 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.4 |
| _P99_ | 5.2ms |
| _P95_ | 5.1ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 223.74 tx/s |
| _Sustained TPS_ | 225.68 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 223.74 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.2 |
| _P99_ | 33.8ms |
| _P95_ | 33.8ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 881.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.73 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 7.1ms |
| _P95_ | 6.8ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 173.51 tx/s |
| _Sustained TPS_ | 173.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 173.51 /s |
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
| _Avg. Confirmation Time (ms)_ | 32.7 |
| _P99_ | 33.4ms |
| _P95_ | 33.3ms |
| _P50_ | 32.9ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 894.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.62 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 8.0ms |
| _P95_ | 6.4ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 188.10 tx/s |
| _Sustained TPS_ | 187.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 188.10 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 67.4 |
| _P99_ | 68.5ms |
| _P95_ | 68.4ms |
| _P50_ | 67.4ms |
| _Tx validation time p50 (ms)_ | 24.7 |
| _End-to-end TPS_ | 875.25 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.17 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.7 |
| _P99_ | 17.6ms |
| _P95_ | 16.9ms |
| _P50_ | 13.7ms |
| _Tx validation time p50 (ms)_ | 3.8 |
| _End-to-end TPS_ | 144.19 tx/s |
| _Sustained TPS_ | 143.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 144.19 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 85.2 |
| _P99_ | 86.8ms |
| _P95_ | 86.6ms |
| _P50_ | 85.8ms |
| _Tx validation time p50 (ms)_ | 24.2 |
| _End-to-end TPS_ | 690.43 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.01 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 134.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.5 |
| _P99_ | 28.1ms |
| _P95_ | 21.8ms |
| _P50_ | 18.6ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 106.23 tx/s |
| _Sustained TPS_ | 105.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 106.23 /s |
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
| _Avg. Confirmation Time (ms)_ | 75.3 |
| _P99_ | 77.0ms |
| _P95_ | 77.0ms |
| _P50_ | 75.5ms |
| _Tx validation time p50 (ms)_ | 29.9 |
| _End-to-end TPS_ | 776.76 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.89 /s |
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
| _Avg. Confirmation Time (ms)_ | 18.3 |
| _P99_ | 29.3ms |
| _P95_ | 24.9ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 6.5 |
| _End-to-end TPS_ | 108.02 tx/s |
| _Sustained TPS_ | 102.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 108.02 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 147.3 |
| _P99_ | 150.0ms |
| _P95_ | 149.9ms |
| _P50_ | 148.5ms |
| _Tx validation time p50 (ms)_ | 50.3 |
| _End-to-end TPS_ | 599.17 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.31 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.6 |
| _P99_ | 37.4ms |
| _P95_ | 35.0ms |
| _P50_ | 24.4ms |
| _Tx validation time p50 (ms)_ | 7.0 |
| _End-to-end TPS_ | 116.04 tx/s |
| _Sustained TPS_ | 116.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 77.36 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 142.1 |
| _P99_ | 146.3ms |
| _P95_ | 146.2ms |
| _P50_ | 143.5ms |
| _Tx validation time p50 (ms)_ | 48.5 |
| _End-to-end TPS_ | 614.20 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.65 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.4 |
| _P99_ | 42.9ms |
| _P95_ | 40.3ms |
| _P50_ | 33.6ms |
| _Tx validation time p50 (ms)_ | 9.8 |
| _End-to-end TPS_ | 87.97 tx/s |
| _Sustained TPS_ | 88.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 59.62 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 125.7 |
| _P99_ | 128.7ms |
| _P95_ | 128.5ms |
| _P50_ | 126.3ms |
| _Tx validation time p50 (ms)_ | 55.7 |
| _End-to-end TPS_ | 692.74 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.39 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.1 |
| _P99_ | 49.7ms |
| _P95_ | 39.8ms |
| _P50_ | 30.3ms |
| _Tx validation time p50 (ms)_ | 9.8 |
| _End-to-end TPS_ | 95.67 tx/s |
| _Sustained TPS_ | 91.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 64.85 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
