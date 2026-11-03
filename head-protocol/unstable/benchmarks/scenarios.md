--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-03 09:40:53.257581383 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 843.18 | n/a | 34.6 | 35.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 182.21 | 182.09 | 5.4 | 7.3 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 887.66 | n/a | 32.8 | 33.6 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.3 | 111.00 | 110.60 | 8.9 | 12.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 953.37 | n/a | 30.5 | 31.2 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 170.16 | 170.32 | 5.8 | 7.6 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 780.39 | n/a | 75.2 | 76.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 118.97 | 122.50 | 16.6 | 21.1 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 712.49 | n/a | 82.0 | 83.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 96.59 | 95.16 | 20.5 | 25.0 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 608.85 | n/a | 96.3 | 97.4 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 97.60 | 94.25 | 20.3 | 26.5 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 734.07 | n/a | 118.9 | 122.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 104.46 | 101.00 | 28.4 | 37.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 560.65 | n/a | 157.2 | 160.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 84.13 | 82.98 | 35.3 | 43.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 593.29 | n/a | 147.8 | 150.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 89.61 | 87.85 | 33.0 | 38.9 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.6 |
| _P99_ | 35.4ms |
| _P95_ | 35.4ms |
| _P50_ | 34.9ms |
| _Tx validation time p50 (ms)_ | 12.5 |
| _End-to-end TPS_ | 843.18 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 56.21 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 7.9ms |
| _P95_ | 7.3ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 182.21 tx/s |
| _Sustained TPS_ | 182.09 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 182.21 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 170.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.8 |
| _P99_ | 33.6ms |
| _P95_ | 33.6ms |
| _P50_ | 33.1ms |
| _Tx validation time p50 (ms)_ | 13.1 |
| _End-to-end TPS_ | 887.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.18 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 8.9 |
| _P99_ | 12.4ms |
| _P95_ | 12.1ms |
| _P50_ | 8.6ms |
| _Tx validation time p50 (ms)_ | 2.4 |
| _End-to-end TPS_ | 111.00 tx/s |
| _Sustained TPS_ | 110.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 111.00 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.5 |
| _P99_ | 31.3ms |
| _P95_ | 31.2ms |
| _P50_ | 30.7ms |
| _Tx validation time p50 (ms)_ | 10.9 |
| _End-to-end TPS_ | 953.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.56 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 8.3ms |
| _P95_ | 7.6ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 170.16 tx/s |
| _Sustained TPS_ | 170.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 170.16 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 170.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 75.2 |
| _P99_ | 76.7ms |
| _P95_ | 76.6ms |
| _P50_ | 75.6ms |
| _Tx validation time p50 (ms)_ | 36.2 |
| _End-to-end TPS_ | 780.39 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.01 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.6 |
| _P99_ | 23.4ms |
| _P95_ | 21.1ms |
| _P50_ | 16.1ms |
| _Tx validation time p50 (ms)_ | 4.4 |
| _End-to-end TPS_ | 118.97 tx/s |
| _Sustained TPS_ | 122.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 118.97 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 82.0 |
| _P99_ | 83.6ms |
| _P95_ | 83.2ms |
| _P50_ | 82.5ms |
| _Tx validation time p50 (ms)_ | 35.2 |
| _End-to-end TPS_ | 712.49 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.75 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 172.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.5 |
| _P99_ | 26.3ms |
| _P95_ | 25.0ms |
| _P50_ | 20.1ms |
| _Tx validation time p50 (ms)_ | 7.0 |
| _End-to-end TPS_ | 96.59 tx/s |
| _Sustained TPS_ | 95.16 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 96.59 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 96.3 |
| _P99_ | 97.5ms |
| _P95_ | 97.4ms |
| _P50_ | 96.8ms |
| _Tx validation time p50 (ms)_ | 31.5 |
| _End-to-end TPS_ | 608.85 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 20.30 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.3 |
| _P99_ | 30.8ms |
| _P95_ | 26.5ms |
| _P50_ | 19.8ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 97.60 tx/s |
| _Sustained TPS_ | 94.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 97.60 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 118.9 |
| _P99_ | 122.4ms |
| _P95_ | 122.3ms |
| _P50_ | 120.6ms |
| _Tx validation time p50 (ms)_ | 61.6 |
| _End-to-end TPS_ | 734.07 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.31 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.4 |
| _P99_ | 39.4ms |
| _P95_ | 37.1ms |
| _P50_ | 27.5ms |
| _Tx validation time p50 (ms)_ | 7.5 |
| _End-to-end TPS_ | 104.46 tx/s |
| _Sustained TPS_ | 101.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 70.80 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 171.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 157.2 |
| _P99_ | 160.3ms |
| _P95_ | 160.2ms |
| _P50_ | 158.1ms |
| _Tx validation time p50 (ms)_ | 54.8 |
| _End-to-end TPS_ | 560.65 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.46 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.3 |
| _P99_ | 49.0ms |
| _P95_ | 43.0ms |
| _P50_ | 34.9ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 84.13 tx/s |
| _Sustained TPS_ | 82.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.02 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 174.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 147.8 |
| _P99_ | 150.8ms |
| _P95_ | 150.7ms |
| _P50_ | 148.8ms |
| _Tx validation time p50 (ms)_ | 54.6 |
| _End-to-end TPS_ | 593.29 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.18 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.0 |
| _P99_ | 41.3ms |
| _P95_ | 38.9ms |
| _P50_ | 33.2ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 89.61 tx/s |
| _Sustained TPS_ | 87.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.73 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
