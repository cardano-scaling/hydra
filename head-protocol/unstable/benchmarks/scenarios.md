--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-09 15:15:11.653401016 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1033.81 | n/a | 28.1 | 28.8 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 188.48 | 188.48 | 5.2 | 5.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 925.58 | n/a | 31.5 | 32.2 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 170.11 | 170.45 | 5.8 | 6.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 994.94 | n/a | 29.4 | 29.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 178.58 | 175.81 | 5.5 | 7.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 821.12 | n/a | 71.1 | 72.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 140.52 | 136.96 | 14.1 | 17.2 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 693.51 | n/a | 84.6 | 86.3 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 97.69 | 96.11 | 20.2 | 24.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 829.58 | n/a | 70.3 | 71.4 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 104.75 | 102.02 | 18.9 | 23.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 593.74 | n/a | 148.3 | 150.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 111.76 | 110.90 | 26.3 | 35.5 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 633.07 | n/a | 138.3 | 141.8 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 85.78 | 84.85 | 34.6 | 42.6 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 575.30 | n/a | 153.3 | 156.2 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 92.41 | 91.33 | 32.1 | 39.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 28.1 |
| _P99_ | 28.8ms |
| _P95_ | 28.8ms |
| _P50_ | 28.3ms |
| _Tx validation time p50 (ms)_ | 11.6 |
| _End-to-end TPS_ | 1033.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 68.92 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 7.2ms |
| _P95_ | 5.9ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 188.48 tx/s |
| _Sustained TPS_ | 188.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 188.48 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.5 |
| _P99_ | 32.3ms |
| _P95_ | 32.2ms |
| _P50_ | 31.8ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 925.58 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.71 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 7.8ms |
| _P95_ | 6.8ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 170.11 tx/s |
| _Sustained TPS_ | 170.45 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 170.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.4 |
| _P99_ | 30.0ms |
| _P95_ | 29.9ms |
| _P50_ | 29.6ms |
| _Tx validation time p50 (ms)_ | 12.3 |
| _End-to-end TPS_ | 994.94 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 66.33 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.5 |
| _P99_ | 8.1ms |
| _P95_ | 7.7ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 178.58 tx/s |
| _Sustained TPS_ | 175.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 178.58 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 71.1 |
| _P99_ | 73.0ms |
| _P95_ | 72.9ms |
| _P50_ | 70.7ms |
| _Tx validation time p50 (ms)_ | 20.9 |
| _End-to-end TPS_ | 821.12 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.37 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.1 |
| _P99_ | 17.9ms |
| _P95_ | 17.2ms |
| _P50_ | 14.0ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 140.52 tx/s |
| _Sustained TPS_ | 136.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 140.52 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 84.6 |
| _P99_ | 86.4ms |
| _P95_ | 86.3ms |
| _P50_ | 85.2ms |
| _Tx validation time p50 (ms)_ | 28.1 |
| _End-to-end TPS_ | 693.51 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.12 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.2 |
| _P99_ | 32.0ms |
| _P95_ | 24.8ms |
| _P50_ | 20.0ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 97.69 tx/s |
| _Sustained TPS_ | 96.11 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 97.69 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.3 |
| _P99_ | 71.8ms |
| _P95_ | 71.4ms |
| _P50_ | 70.6ms |
| _Tx validation time p50 (ms)_ | 25.4 |
| _End-to-end TPS_ | 829.58 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.65 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.9 |
| _P99_ | 25.1ms |
| _P95_ | 23.4ms |
| _P50_ | 18.8ms |
| _Tx validation time p50 (ms)_ | 4.8 |
| _End-to-end TPS_ | 104.75 tx/s |
| _Sustained TPS_ | 102.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 104.75 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 148.3 |
| _P99_ | 150.5ms |
| _P95_ | 150.4ms |
| _P50_ | 149.3ms |
| _Tx validation time p50 (ms)_ | 42.1 |
| _End-to-end TPS_ | 593.74 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.19 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.3 |
| _P99_ | 46.3ms |
| _P95_ | 35.5ms |
| _P50_ | 25.4ms |
| _Tx validation time p50 (ms)_ | 6.7 |
| _End-to-end TPS_ | 111.76 tx/s |
| _Sustained TPS_ | 110.90 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 75.75 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 138.3 |
| _P99_ | 141.9ms |
| _P95_ | 141.8ms |
| _P50_ | 138.6ms |
| _Tx validation time p50 (ms)_ | 67.3 |
| _End-to-end TPS_ | 633.07 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.07 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.6 |
| _P99_ | 44.8ms |
| _P95_ | 42.6ms |
| _P50_ | 34.4ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 85.78 tx/s |
| _Sustained TPS_ | 84.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 57.19 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 153.3 |
| _P99_ | 156.3ms |
| _P95_ | 156.2ms |
| _P50_ | 153.3ms |
| _Tx validation time p50 (ms)_ | 80.0 |
| _End-to-end TPS_ | 575.30 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.78 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.1 |
| _P99_ | 41.7ms |
| _P95_ | 39.5ms |
| _P50_ | 32.2ms |
| _Tx validation time p50 (ms)_ | 8.9 |
| _End-to-end TPS_ | 92.41 tx/s |
| _Sustained TPS_ | 91.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 62.63 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
