--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-02 07:15:52.536161792 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.4 | 78.64 | n/a | 380.3 | 381.2 |
| Nodes=1, Constant, wait for tx valid | 30 | 1.1 | 27.02 | 34.85 | 36.9 | 240.0 |
| Nodes=1, Growing, fire and forget | 30 | 0.1 | 230.40 | n/a | 129.5 | 130.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.5 | 59.03 | 50.57 | 16.9 | 94.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.1 | 463.21 | n/a | 64.2 | 64.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.9 | 32.08 | 40.93 | 31.1 | 122.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 408.58 | n/a | 143.8 | 146.2 |
| Nodes=2, Constant, wait for tx valid | 60 | 2.7 | 21.84 | 19.83 | 91.4 | 228.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.4 | 166.29 | n/a | 311.8 | 360.7 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.9 | 68.22 | 71.45 | 29.1 | 74.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.3 | 195.52 | n/a | 305.6 | 306.1 |
| Nodes=2, Mixed, wait for tx valid | 60 | 3.5 | 17.36 | 15.21 | 114.8 | 482.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.4 | 235.82 | n/a | 342.2 | 380.5 |
| Nodes=3, Constant, wait for tx valid | 90 | 3.0 | 29.73 | 27.95 | 100.5 | 330.6 |
| Nodes=3, Growing, fire and forget | 90 | 0.8 | 118.69 | n/a | 695.9 | 757.7 |
| Nodes=3, Growing, wait for tx valid | 90 | 4.2 | 21.26 | 22.30 | 136.6 | 406.4 |
| Nodes=3, Mixed, fire and forget | 90 | 0.6 | 152.45 | n/a | 543.4 | 589.6 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.5 | 60.39 | 60.80 | 49.1 | 173.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 380.3 |
| _P99_ | 381.2ms |
| _P95_ | 381.2ms |
| _P50_ | 381.1ms |
| _Tx validation time p50 (ms)_ | 202.6 |
| _End-to-end TPS_ | 78.64 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 5.24 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.9 |
| _P99_ | 402.0ms |
| _P95_ | 240.0ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 27.02 tx/s |
| _Sustained TPS_ | 34.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 27.02 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 129.5 |
| _P99_ | 130.0ms |
| _P95_ | 130.0ms |
| _P50_ | 129.8ms |
| _Tx validation time p50 (ms)_ | 19.7 |
| _End-to-end TPS_ | 230.40 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.36 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.9 |
| _P99_ | 155.2ms |
| _P95_ | 94.9ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 59.03 tx/s |
| _Sustained TPS_ | 50.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 59.03 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 64.2 |
| _P99_ | 64.6ms |
| _P95_ | 64.5ms |
| _P50_ | 64.4ms |
| _Tx validation time p50 (ms)_ | 50.0 |
| _End-to-end TPS_ | 463.21 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.88 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.1 |
| _P99_ | 197.9ms |
| _P95_ | 122.5ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 2.0 |
| _End-to-end TPS_ | 32.08 tx/s |
| _Sustained TPS_ | 40.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 32.08 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 143.8 |
| _P99_ | 146.3ms |
| _P95_ | 146.2ms |
| _P50_ | 145.3ms |
| _Tx validation time p50 (ms)_ | 36.8 |
| _End-to-end TPS_ | 408.58 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.62 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 91.4 |
| _P99_ | 268.2ms |
| _P95_ | 228.5ms |
| _P50_ | 75.4ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 21.84 tx/s |
| _Sustained TPS_ | 19.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 21.84 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 311.8 |
| _P99_ | 360.7ms |
| _P95_ | 360.7ms |
| _P50_ | 267.0ms |
| _Tx validation time p50 (ms)_ | 231.2 |
| _End-to-end TPS_ | 166.29 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 5.54 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.1 |
| _P99_ | 80.3ms |
| _P95_ | 74.2ms |
| _P50_ | 19.2ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 68.22 tx/s |
| _Sustained TPS_ | 71.45 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 68.22 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 305.6 |
| _P99_ | 306.4ms |
| _P95_ | 306.1ms |
| _P50_ | 305.9ms |
| _Tx validation time p50 (ms)_ | 274.7 |
| _End-to-end TPS_ | 195.52 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 6.52 /s |
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
| _Avg. Confirmation Time (ms)_ | 114.8 |
| _P99_ | 699.0ms |
| _P95_ | 482.9ms |
| _P50_ | 23.5ms |
| _Tx validation time p50 (ms)_ | 4.5 |
| _End-to-end TPS_ | 17.36 tx/s |
| _Sustained TPS_ | 15.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 17.36 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 342.2 |
| _P99_ | 380.5ms |
| _P95_ | 380.5ms |
| _P50_ | 326.6ms |
| _Tx validation time p50 (ms)_ | 40.1 |
| _End-to-end TPS_ | 235.82 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 5.24 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 100.5 |
| _P99_ | 348.3ms |
| _P95_ | 330.6ms |
| _P50_ | 41.8ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 29.73 tx/s |
| _Sustained TPS_ | 27.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 19.82 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 695.9 |
| _P99_ | 757.7ms |
| _P95_ | 757.7ms |
| _P50_ | 673.9ms |
| _Tx validation time p50 (ms)_ | 262.7 |
| _End-to-end TPS_ | 118.69 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.64 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 136.6 |
| _P99_ | 687.4ms |
| _P95_ | 406.4ms |
| _P50_ | 93.1ms |
| _Tx validation time p50 (ms)_ | 8.0 |
| _End-to-end TPS_ | 21.26 tx/s |
| _Sustained TPS_ | 22.30 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 14.65 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 543.4 |
| _P99_ | 589.6ms |
| _P95_ | 589.6ms |
| _P50_ | 526.0ms |
| _Tx validation time p50 (ms)_ | 88.0 |
| _End-to-end TPS_ | 152.45 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.39 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 49.1 |
| _P99_ | 205.9ms |
| _P95_ | 173.0ms |
| _P50_ | 29.1ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 60.39 tx/s |
| _Sustained TPS_ | 60.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 40.26 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
