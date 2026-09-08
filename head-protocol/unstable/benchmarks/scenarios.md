--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-08 09:13:56.655016003 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1041.08 | n/a | 28.2 | 28.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 229.26 | 228.10 | 4.3 | 6.8 |
| Nodes=1, Growing, fire and forget | 30 | 0.2 | 174.49 | n/a | 171.3 | 171.8 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 189.04 | 193.58 | 5.2 | 8.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 947.56 | n/a | 31.1 | 31.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 173.62 | 174.48 | 5.7 | 10.6 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1027.32 | n/a | 57.4 | 57.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.9 | 66.66 | 63.37 | 29.8 | 169.7 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 452.08 | n/a | 131.3 | 132.4 |
| Nodes=2, Growing, wait for tx valid | 60 | 1.1 | 56.38 | 61.76 | 35.3 | 111.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.2 | 307.61 | n/a | 192.6 | 194.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 1.3 | 45.52 | 41.98 | 43.7 | 237.8 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 702.36 | n/a | 125.5 | 127.5 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 128.94 | 127.73 | 22.9 | 30.5 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 661.19 | n/a | 133.7 | 135.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 2.4 | 36.83 | 36.27 | 81.0 | 353.8 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 611.01 | n/a | 141.3 | 146.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.3 | 71.26 | 71.05 | 40.4 | 93.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 28.2 |
| _P99_ | 28.6ms |
| _P95_ | 28.6ms |
| _P50_ | 28.4ms |
| _Tx validation time p50 (ms)_ | 8.8 |
| _End-to-end TPS_ | 1041.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 69.41 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.3 |
| _P99_ | 9.7ms |
| _P95_ | 6.8ms |
| _P50_ | 3.9ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 229.26 tx/s |
| _Sustained TPS_ | 228.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 229.26 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 171.3 |
| _P99_ | 171.9ms |
| _P95_ | 171.8ms |
| _P50_ | 171.5ms |
| _Tx validation time p50 (ms)_ | 154.6 |
| _End-to-end TPS_ | 174.49 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.63 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 9.8ms |
| _P95_ | 8.1ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 189.04 tx/s |
| _Sustained TPS_ | 193.58 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 189.04 /s |
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
| _P99_ | 31.5ms |
| _P95_ | 31.5ms |
| _P50_ | 31.3ms |
| _Tx validation time p50 (ms)_ | 15.4 |
| _End-to-end TPS_ | 947.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.17 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 23.9ms |
| _P95_ | 10.6ms |
| _P50_ | 4.6ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 173.62 tx/s |
| _Sustained TPS_ | 174.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 173.62 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 57.4 |
| _P99_ | 57.8ms |
| _P95_ | 57.8ms |
| _P50_ | 57.5ms |
| _Tx validation time p50 (ms)_ | 20.6 |
| _End-to-end TPS_ | 1027.32 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 34.24 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.8 |
| _P99_ | 308.6ms |
| _P95_ | 169.7ms |
| _P50_ | 12.3ms |
| _Tx validation time p50 (ms)_ | 3.2 |
| _End-to-end TPS_ | 66.66 tx/s |
| _Sustained TPS_ | 63.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 66.66 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 131.3 |
| _P99_ | 132.4ms |
| _P95_ | 132.4ms |
| _P50_ | 131.5ms |
| _Tx validation time p50 (ms)_ | 99.3 |
| _End-to-end TPS_ | 452.08 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.07 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.3 |
| _P99_ | 174.7ms |
| _P95_ | 111.8ms |
| _P50_ | 20.7ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 56.38 tx/s |
| _Sustained TPS_ | 61.76 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 56.38 /s |
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
| _Avg. Confirmation Time (ms)_ | 192.6 |
| _P99_ | 194.8ms |
| _P95_ | 194.7ms |
| _P50_ | 191.9ms |
| _Tx validation time p50 (ms)_ | 22.1 |
| _End-to-end TPS_ | 307.61 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.25 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 43.7 |
| _P99_ | 269.9ms |
| _P95_ | 237.8ms |
| _P50_ | 17.9ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 45.52 tx/s |
| _Sustained TPS_ | 41.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 45.52 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 125.5 |
| _P99_ | 127.6ms |
| _P95_ | 127.5ms |
| _P50_ | 126.8ms |
| _Tx validation time p50 (ms)_ | 42.8 |
| _End-to-end TPS_ | 702.36 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.61 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 22.9 |
| _P99_ | 37.7ms |
| _P95_ | 30.5ms |
| _P50_ | 22.1ms |
| _Tx validation time p50 (ms)_ | 6.2 |
| _End-to-end TPS_ | 128.94 tx/s |
| _Sustained TPS_ | 127.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 87.39 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 142.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 133.7 |
| _P99_ | 135.5ms |
| _P95_ | 135.4ms |
| _P50_ | 134.9ms |
| _Tx validation time p50 (ms)_ | 32.0 |
| _End-to-end TPS_ | 661.19 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.69 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 81.0 |
| _P99_ | 569.0ms |
| _P95_ | 353.8ms |
| _P50_ | 32.1ms |
| _Tx validation time p50 (ms)_ | 8.1 |
| _End-to-end TPS_ | 36.83 tx/s |
| _Sustained TPS_ | 36.27 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 24.96 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 141.3 |
| _P99_ | 146.9ms |
| _P95_ | 146.8ms |
| _P50_ | 141.7ms |
| _Tx validation time p50 (ms)_ | 44.3 |
| _End-to-end TPS_ | 611.01 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.58 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 40.4 |
| _P99_ | 120.6ms |
| _P95_ | 93.5ms |
| _P50_ | 31.1ms |
| _Tx validation time p50 (ms)_ | 8.7 |
| _End-to-end TPS_ | 71.26 tx/s |
| _Sustained TPS_ | 71.05 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 48.30 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
