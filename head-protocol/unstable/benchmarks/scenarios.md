--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-06 13:26:15.365947351 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 979.15 | n/a | 29.9 | 30.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 186.11 | 183.33 | 5.3 | 7.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 651.29 | n/a | 43.9 | 45.5 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 165.69 | 164.50 | 6.0 | 6.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 913.17 | n/a | 31.9 | 32.6 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 169.73 | 166.62 | 5.8 | 7.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 888.20 | n/a | 65.4 | 67.3 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 124.77 | 122.72 | 15.9 | 20.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 738.81 | n/a | 79.2 | 81.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 93.58 | 92.23 | 21.1 | 26.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 812.02 | n/a | 71.9 | 73.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 99.69 | 97.74 | 19.9 | 26.0 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 609.09 | n/a | 144.4 | 147.5 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 103.24 | 103.38 | 28.6 | 36.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 509.31 | n/a | 171.5 | 174.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 81.16 | 80.35 | 36.6 | 48.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 601.99 | n/a | 146.0 | 148.6 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 85.58 | 82.80 | 34.8 | 44.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.9 |
| _P99_ | 30.5ms |
| _P95_ | 30.5ms |
| _P50_ | 30.1ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 979.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 65.28 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 8.5ms |
| _P95_ | 7.7ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 186.11 tx/s |
| _Sustained TPS_ | 183.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 186.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 43.9 |
| _P99_ | 45.7ms |
| _P95_ | 45.5ms |
| _P50_ | 44.3ms |
| _Tx validation time p50 (ms)_ | 15.9 |
| _End-to-end TPS_ | 651.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 43.42 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 132.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 6.9ms |
| _P95_ | 6.9ms |
| _P50_ | 6.0ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 165.69 tx/s |
| _Sustained TPS_ | 164.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 165.69 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.9 |
| _P99_ | 32.7ms |
| _P95_ | 32.6ms |
| _P50_ | 32.2ms |
| _Tx validation time p50 (ms)_ | 13.3 |
| _End-to-end TPS_ | 913.17 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 60.88 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.8 |
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
| _P99_ | 7.7ms |
| _P95_ | 7.4ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 169.73 tx/s |
| _Sustained TPS_ | 166.62 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 169.73 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 65.4 |
| _P99_ | 67.3ms |
| _P95_ | 67.3ms |
| _P50_ | 65.8ms |
| _Tx validation time p50 (ms)_ | 29.8 |
| _End-to-end TPS_ | 888.20 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.61 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 134.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.9 |
| _P99_ | 23.8ms |
| _P95_ | 20.5ms |
| _P50_ | 15.3ms |
| _Tx validation time p50 (ms)_ | 4.0 |
| _End-to-end TPS_ | 124.77 tx/s |
| _Sustained TPS_ | 122.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 124.77 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.2 |
| _P99_ | 81.0ms |
| _P95_ | 81.0ms |
| _P50_ | 79.7ms |
| _Tx validation time p50 (ms)_ | 23.0 |
| _End-to-end TPS_ | 738.81 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.63 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.1 |
| _P99_ | 28.2ms |
| _P95_ | 26.3ms |
| _P50_ | 21.3ms |
| _Tx validation time p50 (ms)_ | 5.9 |
| _End-to-end TPS_ | 93.58 tx/s |
| _Sustained TPS_ | 92.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 93.58 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 71.9 |
| _P99_ | 73.1ms |
| _P95_ | 73.0ms |
| _P50_ | 72.3ms |
| _Tx validation time p50 (ms)_ | 25.5 |
| _End-to-end TPS_ | 812.02 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.07 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.9 |
| _P99_ | 29.4ms |
| _P95_ | 26.0ms |
| _P50_ | 19.7ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 99.69 tx/s |
| _Sustained TPS_ | 97.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.69 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 144.4 |
| _P99_ | 147.5ms |
| _P95_ | 147.5ms |
| _P50_ | 144.4ms |
| _Tx validation time p50 (ms)_ | 55.3 |
| _End-to-end TPS_ | 609.09 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.54 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.6 |
| _P99_ | 38.4ms |
| _P95_ | 36.1ms |
| _P50_ | 28.4ms |
| _Tx validation time p50 (ms)_ | 7.7 |
| _End-to-end TPS_ | 103.24 tx/s |
| _Sustained TPS_ | 103.38 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 69.98 /s |
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
| _Avg. Confirmation Time (ms)_ | 171.5 |
| _P99_ | 174.6ms |
| _P95_ | 174.5ms |
| _P50_ | 173.8ms |
| _Tx validation time p50 (ms)_ | 70.8 |
| _End-to-end TPS_ | 509.31 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.32 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.6 |
| _P99_ | 53.8ms |
| _P95_ | 48.0ms |
| _P50_ | 36.0ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 81.16 tx/s |
| _Sustained TPS_ | 80.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 55.01 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 147.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 146.0 |
| _P99_ | 148.8ms |
| _P95_ | 148.6ms |
| _P50_ | 147.2ms |
| _Tx validation time p50 (ms)_ | 51.0 |
| _End-to-end TPS_ | 601.99 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.38 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.8 |
| _P99_ | 48.1ms |
| _P95_ | 44.0ms |
| _P50_ | 33.7ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 85.58 tx/s |
| _Sustained TPS_ | 82.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 57.05 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
