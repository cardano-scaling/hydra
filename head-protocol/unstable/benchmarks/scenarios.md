--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-29 11:16:07.251500754 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 905.28 | n/a | 32.4 | 32.9 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 195.63 | 196.74 | 5.0 | 5.8 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 797.80 | n/a | 36.8 | 37.4 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 156.20 | 155.32 | 6.3 | 7.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 832.84 | n/a | 35.2 | 35.8 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 167.58 | 167.02 | 5.9 | 7.0 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 794.23 | n/a | 74.1 | 74.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 131.28 | 131.67 | 15.0 | 19.4 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 745.04 | n/a | 78.4 | 80.3 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 90.70 | 87.66 | 21.8 | 26.7 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 881.41 | n/a | 66.4 | 67.1 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 97.39 | 92.81 | 20.4 | 26.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 604.39 | n/a | 146.0 | 148.1 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.0 | 94.59 | 98.07 | 31.0 | 40.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 582.85 | n/a | 149.0 | 154.1 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.2 | 77.11 | 76.45 | 38.1 | 46.2 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 644.73 | n/a | 135.8 | 139.4 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.72 | 86.34 | 32.9 | 41.7 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.4 |
| _P99_ | 32.9ms |
| _P95_ | 32.9ms |
| _P50_ | 32.6ms |
| _Tx validation time p50 (ms)_ | 13.4 |
| _End-to-end TPS_ | 905.28 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 60.35 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.0 |
| _P99_ | 6.3ms |
| _P95_ | 5.8ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 195.63 tx/s |
| _Sustained TPS_ | 196.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 195.63 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.8 |
| _P99_ | 37.4ms |
| _P95_ | 37.4ms |
| _P50_ | 37.1ms |
| _Tx validation time p50 (ms)_ | 13.6 |
| _End-to-end TPS_ | 797.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 53.19 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.3 |
| _P99_ | 8.7ms |
| _P95_ | 7.6ms |
| _P50_ | 6.3ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 156.20 tx/s |
| _Sustained TPS_ | 155.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 156.20 /s |
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
| _Avg. Confirmation Time (ms)_ | 35.2 |
| _P99_ | 35.8ms |
| _P95_ | 35.8ms |
| _P50_ | 35.5ms |
| _Tx validation time p50 (ms)_ | 18.3 |
| _End-to-end TPS_ | 832.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.52 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 7.5ms |
| _P95_ | 7.0ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 167.58 tx/s |
| _Sustained TPS_ | 167.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 167.58 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 74.1 |
| _P99_ | 74.9ms |
| _P95_ | 74.8ms |
| _P50_ | 74.4ms |
| _Tx validation time p50 (ms)_ | 32.2 |
| _End-to-end TPS_ | 794.23 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.47 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.0 |
| _P99_ | 20.1ms |
| _P95_ | 19.4ms |
| _P50_ | 14.3ms |
| _Tx validation time p50 (ms)_ | 4.0 |
| _End-to-end TPS_ | 131.28 tx/s |
| _Sustained TPS_ | 131.67 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 131.28 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 78.4 |
| _P99_ | 80.3ms |
| _P95_ | 80.3ms |
| _P50_ | 78.9ms |
| _Tx validation time p50 (ms)_ | 31.8 |
| _End-to-end TPS_ | 745.04 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.83 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.8 |
| _P99_ | 28.0ms |
| _P95_ | 26.7ms |
| _P50_ | 22.0ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 90.70 tx/s |
| _Sustained TPS_ | 87.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 90.70 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 66.4 |
| _P99_ | 67.2ms |
| _P95_ | 67.1ms |
| _P50_ | 66.7ms |
| _Tx validation time p50 (ms)_ | 25.4 |
| _End-to-end TPS_ | 881.41 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.38 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.4 |
| _P99_ | 34.9ms |
| _P95_ | 26.4ms |
| _P50_ | 20.4ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 97.39 tx/s |
| _Sustained TPS_ | 92.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 97.39 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 146.0 |
| _P99_ | 148.2ms |
| _P95_ | 148.1ms |
| _P50_ | 146.2ms |
| _Tx validation time p50 (ms)_ | 50.0 |
| _End-to-end TPS_ | 604.39 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.43 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.0 |
| _P99_ | 48.9ms |
| _P95_ | 40.8ms |
| _P50_ | 29.5ms |
| _Tx validation time p50 (ms)_ | 8.4 |
| _End-to-end TPS_ | 94.59 tx/s |
| _Sustained TPS_ | 98.07 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 64.11 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 149.0 |
| _P99_ | 154.2ms |
| _P95_ | 154.1ms |
| _P50_ | 151.1ms |
| _Tx validation time p50 (ms)_ | 83.7 |
| _End-to-end TPS_ | 582.85 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.95 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 38.1 |
| _P99_ | 51.3ms |
| _P95_ | 46.2ms |
| _P50_ | 37.8ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 77.11 tx/s |
| _Sustained TPS_ | 76.45 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 52.27 /s |
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
| _Avg. Confirmation Time (ms)_ | 135.8 |
| _P99_ | 139.5ms |
| _P95_ | 139.4ms |
| _P50_ | 136.4ms |
| _Tx validation time p50 (ms)_ | 46.3 |
| _End-to-end TPS_ | 644.73 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.33 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.9 |
| _P99_ | 46.0ms |
| _P95_ | 41.7ms |
| _P50_ | 32.7ms |
| _Tx validation time p50 (ms)_ | 9.4 |
| _End-to-end TPS_ | 88.72 tx/s |
| _Sustained TPS_ | 86.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 61.12 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      
