--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-01 16:37:15.124603284 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1318.69 | n/a | 22.0 | 22.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 217.30 | 216.19 | 4.5 | 5.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 902.57 | n/a | 32.5 | 33.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 162.98 | 161.32 | 6.1 | 9.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1142.50 | n/a | 25.4 | 26.0 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 185.17 | 185.63 | 5.3 | 6.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1018.16 | n/a | 57.0 | 58.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 144.39 | 142.42 | 13.7 | 16.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 910.68 | n/a | 64.1 | 65.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 107.45 | 104.70 | 18.4 | 23.5 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 969.92 | n/a | 59.9 | 60.9 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 109.19 | 103.45 | 18.1 | 23.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 709.12 | n/a | 122.4 | 126.1 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 117.84 | 117.19 | 25.1 | 31.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 603.95 | n/a | 145.0 | 147.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 89.68 | 88.34 | 33.2 | 41.7 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 697.44 | n/a | 126.4 | 128.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 93.24 | 90.40 | 31.7 | 39.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 22.0 |
| _P99_ | 22.5ms |
| _P95_ | 22.5ms |
| _P50_ | 22.1ms |
| _Tx validation time p50 (ms)_ | 11.0 |
| _End-to-end TPS_ | 1318.69 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 87.91 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.5 |
| _P99_ | 7.7ms |
| _P95_ | 5.5ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 217.30 tx/s |
| _Sustained TPS_ | 216.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 217.30 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.5 |
| _P99_ | 33.0ms |
| _P95_ | 33.0ms |
| _P50_ | 32.7ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 902.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 60.17 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 12.6ms |
| _P95_ | 9.6ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 162.98 tx/s |
| _Sustained TPS_ | 161.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 162.98 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.4 |
| _P99_ | 26.0ms |
| _P95_ | 26.0ms |
| _P50_ | 25.6ms |
| _Tx validation time p50 (ms)_ | 11.5 |
| _End-to-end TPS_ | 1142.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 76.17 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.6 |
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
| _P95_ | 6.7ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 185.17 tx/s |
| _Sustained TPS_ | 185.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 185.17 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 57.0 |
| _P99_ | 58.4ms |
| _P95_ | 58.0ms |
| _P50_ | 57.3ms |
| _Tx validation time p50 (ms)_ | 21.6 |
| _End-to-end TPS_ | 1018.16 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 33.94 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.7 |
| _P99_ | 17.3ms |
| _P95_ | 16.3ms |
| _P50_ | 13.4ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 144.39 tx/s |
| _Sustained TPS_ | 142.42 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 144.39 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 64.1 |
| _P99_ | 65.1ms |
| _P95_ | 65.0ms |
| _P50_ | 64.5ms |
| _Tx validation time p50 (ms)_ | 24.2 |
| _End-to-end TPS_ | 910.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.36 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 26.5ms |
| _P95_ | 23.5ms |
| _P50_ | 18.1ms |
| _Tx validation time p50 (ms)_ | 5.7 |
| _End-to-end TPS_ | 107.45 tx/s |
| _Sustained TPS_ | 104.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 107.45 /s |
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
| _Avg. Confirmation Time (ms)_ | 59.9 |
| _P99_ | 61.3ms |
| _P95_ | 60.9ms |
| _P50_ | 60.3ms |
| _Tx validation time p50 (ms)_ | 24.9 |
| _End-to-end TPS_ | 969.92 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.33 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.1 |
| _P99_ | 26.2ms |
| _P95_ | 23.9ms |
| _P50_ | 18.2ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 109.19 tx/s |
| _Sustained TPS_ | 103.45 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 109.19 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 122.4 |
| _P99_ | 126.3ms |
| _P95_ | 126.1ms |
| _P50_ | 121.6ms |
| _Tx validation time p50 (ms)_ | 53.8 |
| _End-to-end TPS_ | 709.12 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.76 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.1 |
| _P99_ | 38.2ms |
| _P95_ | 31.9ms |
| _P50_ | 24.2ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 117.84 tx/s |
| _Sustained TPS_ | 117.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 79.87 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 145.0 |
| _P99_ | 147.6ms |
| _P95_ | 147.5ms |
| _P50_ | 144.7ms |
| _Tx validation time p50 (ms)_ | 61.5 |
| _End-to-end TPS_ | 603.95 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.42 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.2 |
| _P99_ | 50.3ms |
| _P95_ | 41.7ms |
| _P50_ | 32.9ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 89.68 tx/s |
| _Sustained TPS_ | 88.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.79 /s |
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
| _Avg. Confirmation Time (ms)_ | 126.4 |
| _P99_ | 128.7ms |
| _P95_ | 128.7ms |
| _P50_ | 126.8ms |
| _Tx validation time p50 (ms)_ | 59.3 |
| _End-to-end TPS_ | 697.44 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.50 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.7 |
| _P99_ | 46.8ms |
| _P95_ | 39.6ms |
| _P50_ | 31.4ms |
| _Tx validation time p50 (ms)_ | 9.6 |
| _End-to-end TPS_ | 93.24 tx/s |
| _Sustained TPS_ | 90.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 63.20 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
