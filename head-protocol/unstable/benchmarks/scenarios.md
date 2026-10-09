--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-09 13:11:43.627470251 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 617.77 | n/a | 47.7 | 48.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 249.45 | 261.40 | 4.0 | 4.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1239.15 | n/a | 23.5 | 24.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 210.57 | 210.35 | 4.7 | 6.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1194.16 | n/a | 24.6 | 24.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 228.18 | 226.21 | 4.3 | 5.9 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1456.12 | n/a | 39.9 | 40.5 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.3 | 182.84 | 181.01 | 10.8 | 14.7 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 1149.38 | n/a | 51.0 | 52.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.4 | 140.56 | 137.84 | 14.1 | 18.7 |
| Nodes=2, Mixed, fire and forget | 60 | 0.0 | 1313.25 | n/a | 44.6 | 45.1 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 130.97 | 126.83 | 15.0 | 20.8 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 951.98 | n/a | 92.9 | 94.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.5 | 58.47 | 56.56 | 51.0 | 81.5 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 840.82 | n/a | 105.2 | 106.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.8 | 118.30 | 118.18 | 24.9 | 30.3 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 958.66 | n/a | 92.2 | 93.3 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.7 | 120.95 | 116.87 | 24.7 | 37.4 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 47.7 |
| _P99_ | 48.4ms |
| _P95_ | 48.4ms |
| _P50_ | 48.2ms |
| _Tx validation time p50 (ms)_ | 19.9 |
| _End-to-end TPS_ | 617.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 41.18 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.0 |
| _P99_ | 7.3ms |
| _P95_ | 4.9ms |
| _P50_ | 3.7ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 249.45 tx/s |
| _Sustained TPS_ | 261.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 249.45 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 23.5 |
| _P99_ | 24.1ms |
| _P95_ | 24.0ms |
| _P50_ | 23.8ms |
| _Tx validation time p50 (ms)_ | 9.7 |
| _End-to-end TPS_ | 1239.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 82.61 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.7 |
| _P99_ | 8.5ms |
| _P95_ | 6.8ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 210.57 tx/s |
| _Sustained TPS_ | 210.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 210.57 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.6 |
| _P99_ | 25.0ms |
| _P95_ | 24.9ms |
| _P50_ | 24.7ms |
| _Tx validation time p50 (ms)_ | 12.3 |
| _End-to-end TPS_ | 1194.16 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 79.61 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.3 |
| _P99_ | 7.4ms |
| _P95_ | 5.9ms |
| _P50_ | 4.1ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 228.18 tx/s |
| _Sustained TPS_ | 226.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 228.18 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 39.9 |
| _P99_ | 40.8ms |
| _P95_ | 40.5ms |
| _P50_ | 40.0ms |
| _Tx validation time p50 (ms)_ | 15.0 |
| _End-to-end TPS_ | 1456.12 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 48.54 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 10.8 |
| _P99_ | 17.2ms |
| _P95_ | 14.7ms |
| _P50_ | 10.2ms |
| _Tx validation time p50 (ms)_ | 3.6 |
| _End-to-end TPS_ | 182.84 tx/s |
| _Sustained TPS_ | 181.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 182.84 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 51.0 |
| _P99_ | 52.1ms |
| _P95_ | 52.0ms |
| _P50_ | 51.2ms |
| _Tx validation time p50 (ms)_ | 15.1 |
| _End-to-end TPS_ | 1149.38 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 38.31 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.1 |
| _P99_ | 19.4ms |
| _P95_ | 18.7ms |
| _P50_ | 13.9ms |
| _Tx validation time p50 (ms)_ | 4.8 |
| _End-to-end TPS_ | 140.56 tx/s |
| _Sustained TPS_ | 137.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 140.56 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 44.6 |
| _P99_ | 45.1ms |
| _P95_ | 45.1ms |
| _P50_ | 44.8ms |
| _Tx validation time p50 (ms)_ | 17.2 |
| _End-to-end TPS_ | 1313.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 43.78 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.0 |
| _P99_ | 28.0ms |
| _P95_ | 20.8ms |
| _P50_ | 14.2ms |
| _Tx validation time p50 (ms)_ | 3.4 |
| _End-to-end TPS_ | 130.97 tx/s |
| _Sustained TPS_ | 126.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 130.97 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 92.9 |
| _P99_ | 94.4ms |
| _P95_ | 94.3ms |
| _P50_ | 93.2ms |
| _Tx validation time p50 (ms)_ | 27.5 |
| _End-to-end TPS_ | 951.98 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.16 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 51.0 |
| _P99_ | 88.3ms |
| _P95_ | 81.5ms |
| _P50_ | 60.1ms |
| _Tx validation time p50 (ms)_ | 14.8 |
| _End-to-end TPS_ | 58.47 tx/s |
| _Sustained TPS_ | 56.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 39.63 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 105.2 |
| _P99_ | 106.5ms |
| _P95_ | 106.4ms |
| _P50_ | 105.7ms |
| _Tx validation time p50 (ms)_ | 28.1 |
| _End-to-end TPS_ | 840.82 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.68 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 24.9 |
| _P99_ | 34.9ms |
| _P95_ | 30.3ms |
| _P50_ | 24.6ms |
| _Tx validation time p50 (ms)_ | 6.1 |
| _End-to-end TPS_ | 118.30 tx/s |
| _Sustained TPS_ | 118.18 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 81.49 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 92.2 |
| _P99_ | 93.5ms |
| _P95_ | 93.3ms |
| _P50_ | 92.2ms |
| _Tx validation time p50 (ms)_ | 25.4 |
| _End-to-end TPS_ | 958.66 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.30 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 24.7 |
| _P99_ | 48.9ms |
| _P95_ | 37.4ms |
| _P50_ | 22.6ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 120.95 tx/s |
| _Sustained TPS_ | 116.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 83.32 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
