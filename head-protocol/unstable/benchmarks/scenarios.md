--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-26 14:02:23.123051623 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1162.04 | n/a | 24.9 | 25.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 192.36 | 201.23 | 5.1 | 7.1 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1055.37 | n/a | 27.6 | 28.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 184.35 | 184.30 | 5.4 | 6.0 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1119.57 | n/a | 25.9 | 26.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 184.62 | 181.98 | 5.4 | 7.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1073.12 | n/a | 54.2 | 55.2 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 146.03 | 146.95 | 13.4 | 17.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 869.42 | n/a | 67.0 | 68.7 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 109.17 | 106.87 | 18.1 | 23.0 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 961.80 | n/a | 60.8 | 62.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 113.56 | 110.04 | 17.5 | 21.5 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 812.03 | n/a | 106.4 | 110.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 119.93 | 121.08 | 24.6 | 30.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 582.93 | n/a | 150.0 | 153.3 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 82.69 | 81.25 | 36.0 | 44.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 686.47 | n/a | 127.9 | 130.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 94.68 | 93.33 | 31.2 | 38.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.9 |
| _P99_ | 25.6ms |
| _P95_ | 25.5ms |
| _P50_ | 25.0ms |
| _Tx validation time p50 (ms)_ | 12.0 |
| _End-to-end TPS_ | 1162.04 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 77.47 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 11.0ms |
| _P95_ | 7.1ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 192.36 tx/s |
| _Sustained TPS_ | 201.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 192.36 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.6 |
| _P99_ | 28.2ms |
| _P95_ | 28.1ms |
| _P50_ | 27.8ms |
| _Tx validation time p50 (ms)_ | 11.4 |
| _End-to-end TPS_ | 1055.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 70.36 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 6.3ms |
| _P95_ | 6.0ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 184.35 tx/s |
| _Sustained TPS_ | 184.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 184.35 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.9 |
| _P99_ | 26.6ms |
| _P95_ | 26.5ms |
| _P50_ | 26.1ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 1119.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 74.64 /s |
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
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 8.0ms |
| _P95_ | 7.3ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 184.62 tx/s |
| _Sustained TPS_ | 181.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 184.62 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 54.2 |
| _P99_ | 55.2ms |
| _P95_ | 55.2ms |
| _P50_ | 54.3ms |
| _Tx validation time p50 (ms)_ | 24.0 |
| _End-to-end TPS_ | 1073.12 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 35.77 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.4 |
| _P99_ | 18.3ms |
| _P95_ | 17.3ms |
| _P50_ | 13.1ms |
| _Tx validation time p50 (ms)_ | 3.8 |
| _End-to-end TPS_ | 146.03 tx/s |
| _Sustained TPS_ | 146.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 146.03 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 67.0 |
| _P99_ | 68.7ms |
| _P95_ | 68.7ms |
| _P50_ | 67.2ms |
| _Tx validation time p50 (ms)_ | 26.9 |
| _End-to-end TPS_ | 869.42 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.98 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.1 |
| _P99_ | 23.9ms |
| _P95_ | 23.0ms |
| _P50_ | 18.1ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 109.17 tx/s |
| _Sustained TPS_ | 106.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 109.17 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 60.8 |
| _P99_ | 62.1ms |
| _P95_ | 62.0ms |
| _P50_ | 61.1ms |
| _Tx validation time p50 (ms)_ | 24.2 |
| _End-to-end TPS_ | 961.80 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.06 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 133.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.5 |
| _P99_ | 22.5ms |
| _P95_ | 21.5ms |
| _P50_ | 17.6ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 113.56 tx/s |
| _Sustained TPS_ | 110.04 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 113.56 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 106.4 |
| _P99_ | 110.5ms |
| _P95_ | 110.4ms |
| _P50_ | 106.4ms |
| _Tx validation time p50 (ms)_ | 49.7 |
| _End-to-end TPS_ | 812.03 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.05 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 24.6 |
| _P99_ | 44.6ms |
| _P95_ | 30.8ms |
| _P50_ | 23.8ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 119.93 tx/s |
| _Sustained TPS_ | 121.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 81.28 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 150.0 |
| _P99_ | 153.5ms |
| _P95_ | 153.3ms |
| _P50_ | 152.0ms |
| _Tx validation time p50 (ms)_ | 56.6 |
| _End-to-end TPS_ | 582.93 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.95 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.0 |
| _P99_ | 55.4ms |
| _P95_ | 44.5ms |
| _P50_ | 35.9ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 82.69 tx/s |
| _Sustained TPS_ | 81.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.05 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 127.9 |
| _P99_ | 130.9ms |
| _P95_ | 130.8ms |
| _P50_ | 128.1ms |
| _Tx validation time p50 (ms)_ | 44.5 |
| _End-to-end TPS_ | 686.47 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.25 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.2 |
| _P99_ | 43.5ms |
| _P95_ | 38.8ms |
| _P50_ | 30.5ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 94.68 tx/s |
| _Sustained TPS_ | 93.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 64.17 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
