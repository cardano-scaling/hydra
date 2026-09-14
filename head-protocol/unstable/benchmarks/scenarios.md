--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-14 13:34:36.12428623 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1011.25 | n/a | 28.9 | 29.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 212.83 | 215.18 | 4.6 | 5.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 877.73 | n/a | 33.5 | 34.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 165.68 | 164.41 | 6.0 | 7.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 941.37 | n/a | 31.1 | 31.6 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 175.85 | 173.67 | 5.6 | 8.2 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 908.93 | n/a | 64.6 | 65.7 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 133.41 | 131.40 | 14.8 | 18.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 727.03 | n/a | 80.9 | 81.7 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 91.45 | 88.17 | 21.6 | 25.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 890.16 | n/a | 65.9 | 66.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 103.92 | 99.65 | 18.9 | 23.7 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 606.08 | n/a | 144.6 | 147.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 107.59 | 106.82 | 27.2 | 36.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 599.32 | n/a | 146.6 | 149.8 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 84.48 | 83.43 | 35.1 | 43.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 581.34 | n/a | 151.9 | 154.4 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 92.01 | 88.15 | 32.4 | 40.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 28.9 |
| _P99_ | 29.5ms |
| _P95_ | 29.4ms |
| _P50_ | 29.1ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 1011.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 67.42 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.6 |
| _P99_ | 6.3ms |
| _P95_ | 5.7ms |
| _P50_ | 4.5ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 212.83 tx/s |
| _Sustained TPS_ | 215.18 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 212.83 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.5 |
| _P99_ | 34.1ms |
| _P95_ | 34.1ms |
| _P50_ | 33.8ms |
| _Tx validation time p50 (ms)_ | 11.9 |
| _End-to-end TPS_ | 877.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.52 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 8.2ms |
| _P95_ | 7.4ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 165.68 tx/s |
| _Sustained TPS_ | 164.41 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 165.68 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.1 |
| _P99_ | 31.7ms |
| _P95_ | 31.6ms |
| _P50_ | 31.3ms |
| _Tx validation time p50 (ms)_ | 11.6 |
| _End-to-end TPS_ | 941.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.76 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 9.1ms |
| _P95_ | 8.2ms |
| _P50_ | 5.3ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 175.85 tx/s |
| _Sustained TPS_ | 173.67 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 175.85 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 64.6 |
| _P99_ | 65.8ms |
| _P95_ | 65.7ms |
| _P50_ | 65.0ms |
| _Tx validation time p50 (ms)_ | 26.6 |
| _End-to-end TPS_ | 908.93 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.30 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.8 |
| _P99_ | 22.5ms |
| _P95_ | 18.8ms |
| _P50_ | 14.4ms |
| _Tx validation time p50 (ms)_ | 4.8 |
| _End-to-end TPS_ | 133.41 tx/s |
| _Sustained TPS_ | 131.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 133.41 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 80.9 |
| _P99_ | 81.8ms |
| _P95_ | 81.7ms |
| _P50_ | 81.3ms |
| _Tx validation time p50 (ms)_ | 28.9 |
| _End-to-end TPS_ | 727.03 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.23 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.6 |
| _P99_ | 67.6ms |
| _P95_ | 25.2ms |
| _P50_ | 20.0ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 91.45 tx/s |
| _Sustained TPS_ | 88.17 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 91.45 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 65.9 |
| _P99_ | 66.7ms |
| _P95_ | 66.7ms |
| _P50_ | 66.1ms |
| _Tx validation time p50 (ms)_ | 24.1 |
| _End-to-end TPS_ | 890.16 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.67 /s |
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
| _Avg. Confirmation Time (ms)_ | 18.9 |
| _P99_ | 27.2ms |
| _P95_ | 23.7ms |
| _P50_ | 19.2ms |
| _Tx validation time p50 (ms)_ | 5.9 |
| _End-to-end TPS_ | 103.92 tx/s |
| _Sustained TPS_ | 99.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 103.92 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 144.6 |
| _P99_ | 147.3ms |
| _P95_ | 147.2ms |
| _P50_ | 145.4ms |
| _Tx validation time p50 (ms)_ | 52.5 |
| _End-to-end TPS_ | 606.08 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.47 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.2 |
| _P99_ | 37.9ms |
| _P95_ | 36.2ms |
| _P50_ | 27.2ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 107.59 tx/s |
| _Sustained TPS_ | 106.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 72.92 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 146.6 |
| _P99_ | 149.8ms |
| _P95_ | 149.8ms |
| _P50_ | 146.9ms |
| _Tx validation time p50 (ms)_ | 69.5 |
| _End-to-end TPS_ | 599.32 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.32 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.1 |
| _P99_ | 50.4ms |
| _P95_ | 43.9ms |
| _P50_ | 35.1ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 84.48 tx/s |
| _Sustained TPS_ | 83.43 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.26 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 151.9 |
| _P99_ | 154.5ms |
| _P95_ | 154.4ms |
| _P50_ | 152.1ms |
| _Tx validation time p50 (ms)_ | 47.8 |
| _End-to-end TPS_ | 581.34 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.92 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.4 |
| _P99_ | 43.5ms |
| _P95_ | 40.5ms |
| _P50_ | 32.5ms |
| _Tx validation time p50 (ms)_ | 9.5 |
| _End-to-end TPS_ | 92.01 tx/s |
| _Sustained TPS_ | 88.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 61.34 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
