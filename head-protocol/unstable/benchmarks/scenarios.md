--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-05 15:20:46.251104932 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 984.02 | n/a | 29.6 | 30.2 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 186.23 | 189.02 | 5.3 | 6.4 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 833.55 | n/a | 35.1 | 35.7 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 164.67 | 167.13 | 6.0 | 7.5 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 874.33 | n/a | 33.2 | 34.1 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 166.06 | 161.57 | 6.0 | 8.1 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 820.50 | n/a | 70.9 | 72.3 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 118.25 | 118.07 | 16.7 | 20.2 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 745.63 | n/a | 78.8 | 79.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 98.67 | 100.32 | 20.0 | 24.9 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 793.37 | n/a | 73.9 | 75.4 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 100.97 | 97.33 | 19.6 | 24.3 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 586.12 | n/a | 150.3 | 153.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 100.81 | 100.06 | 29.5 | 38.0 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 551.15 | n/a | 159.9 | 163.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 80.33 | 80.29 | 36.6 | 45.2 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 556.49 | n/a | 159.1 | 161.1 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 86.97 | 83.96 | 34.0 | 42.7 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.6 |
| _P99_ | 30.3ms |
| _P95_ | 30.2ms |
| _P50_ | 29.8ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 984.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 65.60 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 127.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 6.8ms |
| _P95_ | 6.4ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 186.23 tx/s |
| _Sustained TPS_ | 189.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 186.23 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 35.1 |
| _P99_ | 35.8ms |
| _P95_ | 35.7ms |
| _P50_ | 35.4ms |
| _Tx validation time p50 (ms)_ | 12.6 |
| _End-to-end TPS_ | 833.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.57 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 132.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 8.8ms |
| _P95_ | 7.5ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 164.67 tx/s |
| _Sustained TPS_ | 167.13 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 164.67 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.2 |
| _P99_ | 34.1ms |
| _P95_ | 34.1ms |
| _P50_ | 33.4ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 874.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.29 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 8.4ms |
| _P95_ | 8.1ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 166.06 tx/s |
| _Sustained TPS_ | 161.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 166.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.9 |
| _P99_ | 72.6ms |
| _P95_ | 72.3ms |
| _P50_ | 71.4ms |
| _Tx validation time p50 (ms)_ | 22.6 |
| _End-to-end TPS_ | 820.50 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.35 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.7 |
| _P99_ | 25.4ms |
| _P95_ | 20.2ms |
| _P50_ | 16.1ms |
| _Tx validation time p50 (ms)_ | 4.6 |
| _End-to-end TPS_ | 118.25 tx/s |
| _Sustained TPS_ | 118.07 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 118.25 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 78.8 |
| _P99_ | 79.6ms |
| _P95_ | 79.5ms |
| _P50_ | 79.2ms |
| _Tx validation time p50 (ms)_ | 26.7 |
| _End-to-end TPS_ | 745.63 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.85 /s |
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
| _Avg. Confirmation Time (ms)_ | 20.0 |
| _P99_ | 27.4ms |
| _P95_ | 24.9ms |
| _P50_ | 19.7ms |
| _Tx validation time p50 (ms)_ | 5.9 |
| _End-to-end TPS_ | 98.67 tx/s |
| _Sustained TPS_ | 100.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 98.67 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 73.9 |
| _P99_ | 75.5ms |
| _P95_ | 75.4ms |
| _P50_ | 73.8ms |
| _Tx validation time p50 (ms)_ | 23.8 |
| _End-to-end TPS_ | 793.37 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.45 /s |
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
| _Avg. Confirmation Time (ms)_ | 19.6 |
| _P99_ | 26.0ms |
| _P95_ | 24.3ms |
| _P50_ | 19.6ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 100.97 tx/s |
| _Sustained TPS_ | 97.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 100.97 /s |
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
| _Avg. Confirmation Time (ms)_ | 150.3 |
| _P99_ | 153.4ms |
| _P95_ | 153.3ms |
| _P50_ | 151.2ms |
| _Tx validation time p50 (ms)_ | 52.6 |
| _End-to-end TPS_ | 586.12 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.02 /s |
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
| _Avg. Confirmation Time (ms)_ | 29.5 |
| _P99_ | 40.5ms |
| _P95_ | 38.0ms |
| _P50_ | 29.1ms |
| _Tx validation time p50 (ms)_ | 7.8 |
| _End-to-end TPS_ | 100.81 tx/s |
| _Sustained TPS_ | 100.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 67.21 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 159.9 |
| _P99_ | 163.1ms |
| _P95_ | 163.0ms |
| _P50_ | 160.7ms |
| _Tx validation time p50 (ms)_ | 54.2 |
| _End-to-end TPS_ | 551.15 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.25 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.6 |
| _P99_ | 48.4ms |
| _P95_ | 45.2ms |
| _P50_ | 36.5ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 80.33 tx/s |
| _Sustained TPS_ | 80.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 54.45 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 159.1 |
| _P99_ | 161.3ms |
| _P95_ | 161.1ms |
| _P50_ | 159.4ms |
| _Tx validation time p50 (ms)_ | 65.4 |
| _End-to-end TPS_ | 556.49 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.37 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.0 |
| _P99_ | 46.5ms |
| _P95_ | 42.7ms |
| _P50_ | 33.9ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 86.97 tx/s |
| _Sustained TPS_ | 83.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 58.94 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
