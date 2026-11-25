--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-25 13:23:21.363601178 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 895.95 | n/a | 32.8 | 33.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 183.89 | 186.08 | 5.4 | 7.4 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 805.61 | n/a | 36.3 | 37.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 146.18 | 150.85 | 6.8 | 9.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 859.34 | n/a | 33.9 | 34.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 163.11 | 160.41 | 6.1 | 7.9 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 682.98 | n/a | 85.2 | 86.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 123.84 | 125.43 | 16.0 | 20.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 691.47 | n/a | 84.4 | 85.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 97.73 | 96.08 | 20.2 | 24.5 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 581.02 | n/a | 101.2 | 102.3 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 97.08 | 92.91 | 20.4 | 26.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 620.92 | n/a | 140.8 | 144.1 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.0 | 94.71 | 97.35 | 31.0 | 39.4 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 501.13 | n/a | 175.8 | 177.6 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.2 | 75.94 | 77.02 | 38.6 | 49.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 416.20 | n/a | 212.4 | 215.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 86.57 | 84.46 | 34.4 | 42.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.8 |
| _P99_ | 33.3ms |
| _P95_ | 33.3ms |
| _P50_ | 33.0ms |
| _Tx validation time p50 (ms)_ | 12.2 |
| _End-to-end TPS_ | 895.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.73 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 7.4ms |
| _P95_ | 7.4ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 183.89 tx/s |
| _Sustained TPS_ | 186.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 183.89 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.3 |
| _P99_ | 37.1ms |
| _P95_ | 37.0ms |
| _P50_ | 36.6ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 805.61 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 53.71 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 171.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.8 |
| _P99_ | 13.0ms |
| _P95_ | 9.9ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 146.18 tx/s |
| _Sustained TPS_ | 150.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 146.18 /s |
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
| _Avg. Confirmation Time (ms)_ | 33.9 |
| _P99_ | 34.7ms |
| _P95_ | 34.7ms |
| _P50_ | 34.2ms |
| _Tx validation time p50 (ms)_ | 10.8 |
| _End-to-end TPS_ | 859.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 57.29 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 171.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 8.6ms |
| _P95_ | 7.9ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 163.11 tx/s |
| _Sustained TPS_ | 160.41 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 163.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 170.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 85.2 |
| _P99_ | 87.3ms |
| _P95_ | 86.9ms |
| _P50_ | 85.8ms |
| _Tx validation time p50 (ms)_ | 35.2 |
| _End-to-end TPS_ | 682.98 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.77 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.0 |
| _P99_ | 22.0ms |
| _P95_ | 20.5ms |
| _P50_ | 16.1ms |
| _Tx validation time p50 (ms)_ | 4.0 |
| _End-to-end TPS_ | 123.84 tx/s |
| _Sustained TPS_ | 125.43 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 123.84 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 84.4 |
| _P99_ | 86.3ms |
| _P95_ | 85.9ms |
| _P50_ | 84.8ms |
| _Tx validation time p50 (ms)_ | 29.6 |
| _End-to-end TPS_ | 691.47 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.05 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.2 |
| _P99_ | 26.4ms |
| _P95_ | 24.5ms |
| _P50_ | 19.9ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 97.73 tx/s |
| _Sustained TPS_ | 96.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 97.73 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 173.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 101.2 |
| _P99_ | 102.4ms |
| _P95_ | 102.3ms |
| _P50_ | 101.6ms |
| _Tx validation time p50 (ms)_ | 31.6 |
| _End-to-end TPS_ | 581.02 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 19.37 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.4 |
| _P99_ | 30.5ms |
| _P95_ | 26.9ms |
| _P50_ | 20.5ms |
| _Tx validation time p50 (ms)_ | 6.0 |
| _End-to-end TPS_ | 97.08 tx/s |
| _Sustained TPS_ | 92.91 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 97.08 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 140.8 |
| _P99_ | 144.3ms |
| _P95_ | 144.1ms |
| _P50_ | 143.2ms |
| _Tx validation time p50 (ms)_ | 50.6 |
| _End-to-end TPS_ | 620.92 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.80 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.0 |
| _P99_ | 42.0ms |
| _P95_ | 39.4ms |
| _P50_ | 30.4ms |
| _Tx validation time p50 (ms)_ | 8.4 |
| _End-to-end TPS_ | 94.71 tx/s |
| _Sustained TPS_ | 97.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 64.19 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 175.8 |
| _P99_ | 177.7ms |
| _P95_ | 177.6ms |
| _P50_ | 176.4ms |
| _Tx validation time p50 (ms)_ | 62.9 |
| _End-to-end TPS_ | 501.13 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.14 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 38.6 |
| _P99_ | 58.5ms |
| _P95_ | 49.0ms |
| _P50_ | 38.4ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 75.94 tx/s |
| _Sustained TPS_ | 77.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 51.47 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 173.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 212.4 |
| _P99_ | 215.1ms |
| _P95_ | 215.0ms |
| _P50_ | 212.8ms |
| _Tx validation time p50 (ms)_ | 80.5 |
| _End-to-end TPS_ | 416.20 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.25 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.4 |
| _P99_ | 43.7ms |
| _P95_ | 42.0ms |
| _P50_ | 35.2ms |
| _Tx validation time p50 (ms)_ | 8.8 |
| _End-to-end TPS_ | 86.57 tx/s |
| _Sustained TPS_ | 84.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 57.71 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
