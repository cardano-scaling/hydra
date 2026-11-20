--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-20 13:54:17.264077025 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1092.63 | n/a | 26.9 | 27.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.4 | 79.87 | 216.27 | 12.5 | 49.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.2 | 167.30 | n/a | 178.7 | 179.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 126.63 | 133.66 | 7.8 | 18.0 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1103.21 | n/a | 26.6 | 27.1 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.4 | 68.13 | 162.06 | 14.6 | 64.2 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1037.68 | n/a | 55.9 | 57.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 113.80 | 106.17 | 17.4 | 48.7 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 904.37 | n/a | 64.7 | 66.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 1.0 | 61.79 | 54.87 | 32.2 | 103.7 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 953.49 | n/a | 61.6 | 62.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.8 | 78.96 | 81.22 | 25.2 | 65.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 801.23 | n/a | 110.0 | 111.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 111.11 | 105.98 | 26.8 | 29.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.4 | 232.28 | n/a | 384.8 | 386.9 |
| Nodes=3, Growing, wait for tx valid | 90 | 3.1 | 28.65 | 29.80 | 104.1 | 242.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 438.76 | n/a | 201.6 | 204.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.4 | 63.85 | 56.36 | 46.8 | 136.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.9 |
| _P99_ | 27.3ms |
| _P95_ | 27.3ms |
| _P50_ | 27.0ms |
| _Tx validation time p50 (ms)_ | 14.5 |
| _End-to-end TPS_ | 1092.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 72.84 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 12.5 |
| _P99_ | 140.1ms |
| _P95_ | 49.5ms |
| _P50_ | 4.2ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 79.87 tx/s |
| _Sustained TPS_ | 216.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 79.87 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 170.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 178.7 |
| _P99_ | 179.1ms |
| _P95_ | 179.1ms |
| _P50_ | 178.9ms |
| _Tx validation time p50 (ms)_ | 9.8 |
| _End-to-end TPS_ | 167.30 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.15 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 172.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 7.8 |
| _P99_ | 40.3ms |
| _P95_ | 18.0ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 126.63 tx/s |
| _Sustained TPS_ | 133.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 126.63 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.6 |
| _P99_ | 27.1ms |
| _P95_ | 27.1ms |
| _P50_ | 26.8ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 1103.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 73.55 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.6 |
| _P99_ | 148.1ms |
| _P95_ | 64.2ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 68.13 tx/s |
| _Sustained TPS_ | 162.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 68.13 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 55.9 |
| _P99_ | 57.6ms |
| _P95_ | 57.6ms |
| _P50_ | 56.3ms |
| _Tx validation time p50 (ms)_ | 18.1 |
| _End-to-end TPS_ | 1037.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 34.59 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 172.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.4 |
| _P99_ | 81.5ms |
| _P95_ | 48.7ms |
| _P50_ | 13.1ms |
| _Tx validation time p50 (ms)_ | 3.3 |
| _End-to-end TPS_ | 113.80 tx/s |
| _Sustained TPS_ | 106.17 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 113.80 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 64.7 |
| _P99_ | 66.3ms |
| _P95_ | 66.2ms |
| _P50_ | 65.0ms |
| _Tx validation time p50 (ms)_ | 23.3 |
| _End-to-end TPS_ | 904.37 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.15 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 172.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.2 |
| _P99_ | 134.2ms |
| _P95_ | 103.7ms |
| _P50_ | 17.6ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 61.79 tx/s |
| _Sustained TPS_ | 54.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 61.79 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 61.6 |
| _P99_ | 62.2ms |
| _P95_ | 62.2ms |
| _P50_ | 61.8ms |
| _Tx validation time p50 (ms)_ | 30.5 |
| _End-to-end TPS_ | 953.49 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.78 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.2 |
| _P99_ | 70.0ms |
| _P95_ | 65.1ms |
| _P50_ | 17.6ms |
| _Tx validation time p50 (ms)_ | 5.7 |
| _End-to-end TPS_ | 78.96 tx/s |
| _Sustained TPS_ | 81.22 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 78.96 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 110.0 |
| _P99_ | 111.5ms |
| _P95_ | 111.4ms |
| _P50_ | 110.0ms |
| _Tx validation time p50 (ms)_ | 42.8 |
| _End-to-end TPS_ | 801.23 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.81 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.8 |
| _P99_ | 151.0ms |
| _P95_ | 29.9ms |
| _P50_ | 22.2ms |
| _Tx validation time p50 (ms)_ | 5.7 |
| _End-to-end TPS_ | 111.11 tx/s |
| _Sustained TPS_ | 105.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 75.31 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 384.8 |
| _P99_ | 387.0ms |
| _P95_ | 386.9ms |
| _P50_ | 385.0ms |
| _Tx validation time p50 (ms)_ | 207.5 |
| _End-to-end TPS_ | 232.28 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 5.16 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 104.1 |
| _P99_ | 247.2ms |
| _P95_ | 242.5ms |
| _P50_ | 36.5ms |
| _Tx validation time p50 (ms)_ | 8.0 |
| _End-to-end TPS_ | 28.65 tx/s |
| _Sustained TPS_ | 29.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 19.42 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 201.6 |
| _P99_ | 204.9ms |
| _P95_ | 204.8ms |
| _P50_ | 203.5ms |
| _Tx validation time p50 (ms)_ | 134.1 |
| _End-to-end TPS_ | 438.76 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.75 /s |
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
| _Avg. Confirmation Time (ms)_ | 46.8 |
| _P99_ | 238.2ms |
| _P95_ | 136.0ms |
| _P50_ | 27.9ms |
| _Tx validation time p50 (ms)_ | 8.2 |
| _End-to-end TPS_ | 63.85 tx/s |
| _Sustained TPS_ | 56.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 42.56 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
