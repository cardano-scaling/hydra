--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-01 16:51:45.029479655 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 882.01 | n/a | 33.4 | 33.8 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 208.48 | 203.95 | 4.7 | 6.3 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1114.01 | n/a | 26.1 | 26.7 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 172.85 | 171.71 | 5.7 | 7.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1124.82 | n/a | 26.0 | 26.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 191.79 | 185.74 | 5.2 | 7.8 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1138.57 | n/a | 50.6 | 52.3 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 142.92 | 142.97 | 13.8 | 18.0 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 877.48 | n/a | 66.4 | 67.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 104.93 | 104.08 | 18.8 | 23.9 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 985.75 | n/a | 58.7 | 59.9 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 113.53 | 107.95 | 17.5 | 22.7 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 715.20 | n/a | 122.1 | 124.7 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 112.58 | 114.02 | 26.2 | 38.5 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 665.87 | n/a | 132.0 | 134.1 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 91.23 | 89.50 | 32.4 | 41.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 656.15 | n/a | 133.8 | 136.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 99.86 | 97.66 | 29.6 | 36.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.4 |
| _P99_ | 33.8ms |
| _P95_ | 33.8ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 20.4 |
| _End-to-end TPS_ | 882.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.80 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.7 |
| _P99_ | 9.5ms |
| _P95_ | 6.3ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 208.48 tx/s |
| _Sustained TPS_ | 203.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 208.48 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.1 |
| _P99_ | 26.7ms |
| _P95_ | 26.7ms |
| _P50_ | 26.2ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 1114.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 74.27 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 9.9ms |
| _P95_ | 7.8ms |
| _P50_ | 5.3ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 172.85 tx/s |
| _Sustained TPS_ | 171.71 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 172.85 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.0 |
| _P99_ | 26.4ms |
| _P95_ | 26.4ms |
| _P50_ | 26.1ms |
| _Tx validation time p50 (ms)_ | 11.6 |
| _End-to-end TPS_ | 1124.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 74.99 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 9.9ms |
| _P95_ | 7.8ms |
| _P50_ | 4.8ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 191.79 tx/s |
| _Sustained TPS_ | 185.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 191.79 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 50.6 |
| _P99_ | 52.4ms |
| _P95_ | 52.3ms |
| _P50_ | 50.7ms |
| _Tx validation time p50 (ms)_ | 26.8 |
| _End-to-end TPS_ | 1138.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 37.95 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 134.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.8 |
| _P99_ | 21.2ms |
| _P95_ | 18.0ms |
| _P50_ | 13.2ms |
| _Tx validation time p50 (ms)_ | 3.8 |
| _End-to-end TPS_ | 142.92 tx/s |
| _Sustained TPS_ | 142.97 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 142.92 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 66.4 |
| _P99_ | 67.6ms |
| _P95_ | 67.5ms |
| _P50_ | 67.2ms |
| _Tx validation time p50 (ms)_ | 26.0 |
| _End-to-end TPS_ | 877.48 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.25 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.8 |
| _P99_ | 24.3ms |
| _P95_ | 23.9ms |
| _P50_ | 18.6ms |
| _Tx validation time p50 (ms)_ | 6.7 |
| _End-to-end TPS_ | 104.93 tx/s |
| _Sustained TPS_ | 104.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 104.93 /s |
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
| _Avg. Confirmation Time (ms)_ | 58.7 |
| _P99_ | 60.3ms |
| _P95_ | 59.9ms |
| _P50_ | 59.0ms |
| _Tx validation time p50 (ms)_ | 21.0 |
| _End-to-end TPS_ | 985.75 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.86 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.5 |
| _P99_ | 24.9ms |
| _P95_ | 22.7ms |
| _P50_ | 17.3ms |
| _Tx validation time p50 (ms)_ | 6.1 |
| _End-to-end TPS_ | 113.53 tx/s |
| _Sustained TPS_ | 107.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 113.53 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 122.1 |
| _P99_ | 124.7ms |
| _P95_ | 124.7ms |
| _P50_ | 124.0ms |
| _Tx validation time p50 (ms)_ | 37.1 |
| _End-to-end TPS_ | 715.20 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.89 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.2 |
| _P99_ | 44.0ms |
| _P95_ | 38.5ms |
| _P50_ | 24.7ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 112.58 tx/s |
| _Sustained TPS_ | 114.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 77.55 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 132.0 |
| _P99_ | 134.2ms |
| _P95_ | 134.1ms |
| _P50_ | 133.1ms |
| _Tx validation time p50 (ms)_ | 45.7 |
| _End-to-end TPS_ | 665.87 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.80 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.4 |
| _P99_ | 43.3ms |
| _P95_ | 41.9ms |
| _P50_ | 32.3ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 91.23 tx/s |
| _Sustained TPS_ | 89.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 61.84 /s |
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
| _Avg. Confirmation Time (ms)_ | 133.8 |
| _P99_ | 136.9ms |
| _P95_ | 136.8ms |
| _P50_ | 133.1ms |
| _Tx validation time p50 (ms)_ | 51.3 |
| _End-to-end TPS_ | 656.15 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.58 /s |
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
| _Avg. Confirmation Time (ms)_ | 29.6 |
| _P99_ | 40.9ms |
| _P95_ | 36.5ms |
| _P50_ | 29.9ms |
| _Tx validation time p50 (ms)_ | 8.5 |
| _End-to-end TPS_ | 99.86 tx/s |
| _Sustained TPS_ | 97.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 67.68 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 134.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
