--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-05 13:46:31.131728521 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 979.88 | n/a | 29.9 | 30.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 185.26 | 190.93 | 5.3 | 8.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 814.19 | n/a | 36.1 | 36.6 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 154.93 | 153.37 | 6.4 | 7.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 940.02 | n/a | 31.0 | 31.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 172.53 | 174.96 | 5.7 | 7.1 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 806.40 | n/a | 72.7 | 74.2 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 137.45 | 137.46 | 14.4 | 17.4 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 757.31 | n/a | 77.7 | 78.4 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 91.64 | 89.07 | 21.6 | 28.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 880.27 | n/a | 66.1 | 67.3 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 95.24 | 92.68 | 20.8 | 27.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 683.36 | n/a | 127.1 | 130.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 104.34 | 102.92 | 28.5 | 36.4 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 542.21 | n/a | 162.6 | 164.9 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 86.52 | 85.33 | 34.4 | 42.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 660.04 | n/a | 131.5 | 135.6 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.09 | 85.22 | 33.6 | 43.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.9 |
| _P99_ | 30.4ms |
| _P95_ | 30.4ms |
| _P50_ | 30.1ms |
| _Tx validation time p50 (ms)_ | 10.8 |
| _End-to-end TPS_ | 979.88 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 65.33 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 9.8ms |
| _P95_ | 8.2ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 185.26 tx/s |
| _Sustained TPS_ | 190.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 185.26 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 127.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.1 |
| _P99_ | 36.7ms |
| _P95_ | 36.6ms |
| _P50_ | 36.4ms |
| _Tx validation time p50 (ms)_ | 13.2 |
| _End-to-end TPS_ | 814.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 54.28 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.4 |
| _P99_ | 8.9ms |
| _P95_ | 7.9ms |
| _P50_ | 6.3ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 154.93 tx/s |
| _Sustained TPS_ | 153.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 154.93 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.0 |
| _P99_ | 31.8ms |
| _P95_ | 31.7ms |
| _P50_ | 31.3ms |
| _Tx validation time p50 (ms)_ | 13.4 |
| _End-to-end TPS_ | 940.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.67 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 8.5ms |
| _P95_ | 7.1ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 172.53 tx/s |
| _Sustained TPS_ | 174.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 172.53 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 127.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 72.7 |
| _P99_ | 74.2ms |
| _P95_ | 74.2ms |
| _P50_ | 72.6ms |
| _Tx validation time p50 (ms)_ | 27.8 |
| _End-to-end TPS_ | 806.40 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.88 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.4 |
| _P99_ | 18.0ms |
| _P95_ | 17.4ms |
| _P50_ | 14.1ms |
| _Tx validation time p50 (ms)_ | 3.7 |
| _End-to-end TPS_ | 137.45 tx/s |
| _Sustained TPS_ | 137.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 137.45 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 77.7 |
| _P99_ | 78.5ms |
| _P95_ | 78.4ms |
| _P50_ | 78.1ms |
| _Tx validation time p50 (ms)_ | 27.6 |
| _End-to-end TPS_ | 757.31 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.24 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.6 |
| _P99_ | 30.5ms |
| _P95_ | 28.2ms |
| _P50_ | 21.2ms |
| _Tx validation time p50 (ms)_ | 7.1 |
| _End-to-end TPS_ | 91.64 tx/s |
| _Sustained TPS_ | 89.07 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 91.64 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 137.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 66.1 |
| _P99_ | 67.4ms |
| _P95_ | 67.3ms |
| _P50_ | 66.3ms |
| _Tx validation time p50 (ms)_ | 29.7 |
| _End-to-end TPS_ | 880.27 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.34 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 134.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.8 |
| _P99_ | 29.1ms |
| _P95_ | 27.1ms |
| _P50_ | 20.2ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 95.24 tx/s |
| _Sustained TPS_ | 92.68 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 95.24 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 127.1 |
| _P99_ | 130.8ms |
| _P95_ | 130.8ms |
| _P50_ | 130.0ms |
| _Tx validation time p50 (ms)_ | 48.0 |
| _End-to-end TPS_ | 683.36 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.19 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.5 |
| _P99_ | 38.8ms |
| _P95_ | 36.4ms |
| _P50_ | 27.2ms |
| _Tx validation time p50 (ms)_ | 7.3 |
| _End-to-end TPS_ | 104.34 tx/s |
| _Sustained TPS_ | 102.92 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 69.56 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 162.6 |
| _P99_ | 165.1ms |
| _P95_ | 164.9ms |
| _P50_ | 164.0ms |
| _Tx validation time p50 (ms)_ | 43.1 |
| _End-to-end TPS_ | 542.21 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.05 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.4 |
| _P99_ | 45.9ms |
| _P95_ | 42.0ms |
| _P50_ | 34.9ms |
| _Tx validation time p50 (ms)_ | 9.5 |
| _End-to-end TPS_ | 86.52 tx/s |
| _Sustained TPS_ | 85.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 57.68 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 131.5 |
| _P99_ | 135.7ms |
| _P95_ | 135.6ms |
| _P50_ | 134.9ms |
| _Tx validation time p50 (ms)_ | 51.6 |
| _End-to-end TPS_ | 660.04 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.67 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.6 |
| _P99_ | 49.9ms |
| _P95_ | 43.6ms |
| _P50_ | 32.8ms |
| _Tx validation time p50 (ms)_ | 8.9 |
| _End-to-end TPS_ | 88.09 tx/s |
| _Sustained TPS_ | 85.22 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 59.71 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
