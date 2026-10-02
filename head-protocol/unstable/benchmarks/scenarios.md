--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-02 14:22:24.891653233 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1177.49 | n/a | 25.1 | 25.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 1.5 | 19.54 | 17.08 | 51.1 | 145.6 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1190.14 | n/a | 24.6 | 25.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 1.9 | 15.96 | 18.91 | 62.6 | 212.5 |
| Nodes=1, Mixed, fire and forget | 30 | 0.2 | 132.98 | n/a | 220.4 | 225.2 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.4 | 81.54 | 83.66 | 12.2 | 50.0 |
| Nodes=2, Constant, fire and forget | 60 | 0.3 | 231.81 | n/a | 248.2 | 258.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 1.2 | 51.91 | 111.74 | 38.4 | 186.2 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 440.77 | n/a | 135.1 | 135.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 1.2 | 48.03 | 46.32 | 41.5 | 133.1 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 593.67 | n/a | 98.6 | 100.9 |
| Nodes=2, Mixed, wait for tx valid | 60 | 3.5 | 16.96 | 19.08 | 117.7 | 339.2 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 433.43 | n/a | 205.8 | 207.5 |
| Nodes=3, Constant, wait for tx valid | 90 | 2.1 | 43.15 | 48.86 | 67.5 | 229.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.4 | 234.76 | n/a | 344.3 | 381.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 4.0 | 22.56 | 26.21 | 132.2 | 373.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 854.41 | n/a | 103.7 | 104.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 2.8 | 31.96 | 30.32 | 93.5 | 245.9 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.1 |
| _P99_ | 25.4ms |
| _P95_ | 25.4ms |
| _P50_ | 25.3ms |
| _Tx validation time p50 (ms)_ | 6.5 |
| _End-to-end TPS_ | 1177.49 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 78.50 /s |
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
| _Avg. Confirmation Time (ms)_ | 51.1 |
| _P99_ | 239.4ms |
| _P95_ | 145.6ms |
| _P50_ | 31.3ms |
| _Tx validation time p50 (ms)_ | 3.2 |
| _End-to-end TPS_ | 19.54 tx/s |
| _Sustained TPS_ | 17.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 19.54 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.6 |
| _P99_ | 25.0ms |
| _P95_ | 25.0ms |
| _P50_ | 24.8ms |
| _Tx validation time p50 (ms)_ | 10.1 |
| _End-to-end TPS_ | 1190.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 79.34 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 62.6 |
| _P99_ | 237.4ms |
| _P95_ | 212.5ms |
| _P50_ | 27.6ms |
| _Tx validation time p50 (ms)_ | 2.3 |
| _End-to-end TPS_ | 15.96 tx/s |
| _Sustained TPS_ | 18.91 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 15.96 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 220.4 |
| _P99_ | 225.4ms |
| _P95_ | 225.2ms |
| _P50_ | 225.0ms |
| _Tx validation time p50 (ms)_ | 31.9 |
| _End-to-end TPS_ | 132.98 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 8.87 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 12.2 |
| _P99_ | 52.8ms |
| _P95_ | 50.0ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 81.54 tx/s |
| _Sustained TPS_ | 83.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 81.54 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 248.2 |
| _P99_ | 258.4ms |
| _P95_ | 258.0ms |
| _P50_ | 251.5ms |
| _Tx validation time p50 (ms)_ | 15.9 |
| _End-to-end TPS_ | 231.81 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 7.73 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 38.4 |
| _P99_ | 302.0ms |
| _P95_ | 186.2ms |
| _P50_ | 12.3ms |
| _Tx validation time p50 (ms)_ | 4.2 |
| _End-to-end TPS_ | 51.91 tx/s |
| _Sustained TPS_ | 111.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 51.91 /s |
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
| _Avg. Confirmation Time (ms)_ | 135.1 |
| _P99_ | 136.0ms |
| _P95_ | 135.9ms |
| _P50_ | 135.3ms |
| _Tx validation time p50 (ms)_ | 15.0 |
| _End-to-end TPS_ | 440.77 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.69 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 41.5 |
| _P99_ | 146.3ms |
| _P95_ | 133.1ms |
| _P50_ | 17.1ms |
| _Tx validation time p50 (ms)_ | 4.2 |
| _End-to-end TPS_ | 48.03 tx/s |
| _Sustained TPS_ | 46.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 48.03 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 98.6 |
| _P99_ | 101.0ms |
| _P95_ | 100.9ms |
| _P50_ | 97.2ms |
| _Tx validation time p50 (ms)_ | 57.5 |
| _End-to-end TPS_ | 593.67 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 19.79 /s |
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
| _Avg. Confirmation Time (ms)_ | 117.7 |
| _P99_ | 450.9ms |
| _P95_ | 339.2ms |
| _P50_ | 87.0ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 16.96 tx/s |
| _Sustained TPS_ | 19.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 16.96 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 205.8 |
| _P99_ | 207.5ms |
| _P95_ | 207.5ms |
| _P50_ | 206.4ms |
| _Tx validation time p50 (ms)_ | 163.4 |
| _End-to-end TPS_ | 433.43 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.63 /s |
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
| _Avg. Confirmation Time (ms)_ | 67.5 |
| _P99_ | 243.2ms |
| _P95_ | 229.9ms |
| _P50_ | 34.4ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 43.15 tx/s |
| _Sustained TPS_ | 48.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 29.72 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 344.3 |
| _P99_ | 381.7ms |
| _P95_ | 381.4ms |
| _P50_ | 341.2ms |
| _Tx validation time p50 (ms)_ | 201.0 |
| _End-to-end TPS_ | 234.76 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 5.22 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 132.2 |
| _P99_ | 467.6ms |
| _P95_ | 373.0ms |
| _P50_ | 72.6ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 22.56 tx/s |
| _Sustained TPS_ | 26.21 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 15.04 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 103.7 |
| _P99_ | 104.8ms |
| _P95_ | 104.8ms |
| _P50_ | 103.9ms |
| _Tx validation time p50 (ms)_ | 42.2 |
| _End-to-end TPS_ | 854.41 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.99 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 93.5 |
| _P99_ | 447.5ms |
| _P95_ | 245.9ms |
| _P50_ | 74.4ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 31.96 tx/s |
| _Sustained TPS_ | 30.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 21.66 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
