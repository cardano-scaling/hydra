--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-06 15:37:06.699307231 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 973.05 | n/a | 29.6 | 30.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 206.66 | 208.81 | 4.8 | 5.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 906.52 | n/a | 32.3 | 32.9 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 166.47 | 167.23 | 5.9 | 7.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 906.03 | n/a | 32.3 | 32.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 182.60 | 181.02 | 5.4 | 6.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 837.17 | n/a | 69.6 | 71.4 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 130.86 | 129.43 | 15.1 | 17.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 739.22 | n/a | 79.5 | 81.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 102.58 | 100.21 | 19.3 | 24.6 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 778.86 | n/a | 74.9 | 76.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 106.29 | 102.53 | 18.6 | 22.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 603.76 | n/a | 145.7 | 148.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 109.23 | 106.78 | 27.3 | 34.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 541.22 | n/a | 161.9 | 164.7 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 87.44 | 86.84 | 34.0 | 39.7 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 567.22 | n/a | 154.8 | 157.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.99 | 86.52 | 33.3 | 42.2 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.6 |
| _P99_ | 30.7ms |
| _P95_ | 30.6ms |
| _P50_ | 29.6ms |
| _Tx validation time p50 (ms)_ | 11.1 |
| _End-to-end TPS_ | 973.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 64.87 /s |
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
| _Avg. Confirmation Time (ms)_ | 4.8 |
| _P99_ | 6.5ms |
| _P95_ | 5.7ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 206.66 tx/s |
| _Sustained TPS_ | 208.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 206.66 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.3 |
| _P99_ | 32.9ms |
| _P95_ | 32.9ms |
| _P50_ | 32.5ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 906.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 60.43 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 7.3ms |
| _P95_ | 7.1ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 166.47 tx/s |
| _Sustained TPS_ | 167.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 166.47 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.3 |
| _P99_ | 32.9ms |
| _P95_ | 32.9ms |
| _P50_ | 32.6ms |
| _Tx validation time p50 (ms)_ | 11.5 |
| _End-to-end TPS_ | 906.03 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 60.40 /s |
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
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 7.2ms |
| _P95_ | 6.7ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 182.60 tx/s |
| _Sustained TPS_ | 181.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 182.60 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 69.6 |
| _P99_ | 71.5ms |
| _P95_ | 71.4ms |
| _P50_ | 70.0ms |
| _Tx validation time p50 (ms)_ | 27.4 |
| _End-to-end TPS_ | 837.17 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.91 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.1 |
| _P99_ | 19.7ms |
| _P95_ | 17.8ms |
| _P50_ | 15.1ms |
| _Tx validation time p50 (ms)_ | 4.9 |
| _End-to-end TPS_ | 130.86 tx/s |
| _Sustained TPS_ | 129.43 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 130.86 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.5 |
| _P99_ | 81.1ms |
| _P95_ | 81.0ms |
| _P50_ | 79.5ms |
| _Tx validation time p50 (ms)_ | 29.6 |
| _End-to-end TPS_ | 739.22 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.64 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 132.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.3 |
| _P99_ | 32.8ms |
| _P95_ | 24.6ms |
| _P50_ | 18.7ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 102.58 tx/s |
| _Sustained TPS_ | 100.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 102.58 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 74.9 |
| _P99_ | 76.3ms |
| _P95_ | 76.2ms |
| _P50_ | 75.4ms |
| _Tx validation time p50 (ms)_ | 26.2 |
| _End-to-end TPS_ | 778.86 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.96 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.6 |
| _P99_ | 24.7ms |
| _P95_ | 22.6ms |
| _P50_ | 18.9ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 106.29 tx/s |
| _Sustained TPS_ | 102.53 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 106.29 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 145.7 |
| _P99_ | 148.9ms |
| _P95_ | 148.8ms |
| _P50_ | 146.8ms |
| _Tx validation time p50 (ms)_ | 56.8 |
| _End-to-end TPS_ | 603.76 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.42 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.3 |
| _P99_ | 37.3ms |
| _P95_ | 34.1ms |
| _P50_ | 27.0ms |
| _Tx validation time p50 (ms)_ | 7.3 |
| _End-to-end TPS_ | 109.23 tx/s |
| _Sustained TPS_ | 106.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 72.82 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 161.9 |
| _P99_ | 164.8ms |
| _P95_ | 164.7ms |
| _P50_ | 162.7ms |
| _Tx validation time p50 (ms)_ | 53.1 |
| _End-to-end TPS_ | 541.22 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.03 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.0 |
| _P99_ | 41.9ms |
| _P95_ | 39.7ms |
| _P50_ | 33.9ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 87.44 tx/s |
| _Sustained TPS_ | 86.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 58.29 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 154.8 |
| _P99_ | 157.1ms |
| _P95_ | 157.0ms |
| _P50_ | 154.7ms |
| _Tx validation time p50 (ms)_ | 41.1 |
| _End-to-end TPS_ | 567.22 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.60 /s |
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
| _Avg. Confirmation Time (ms)_ | 33.3 |
| _P99_ | 51.6ms |
| _P95_ | 42.2ms |
| _P50_ | 33.0ms |
| _Tx validation time p50 (ms)_ | 9.1 |
| _End-to-end TPS_ | 88.99 tx/s |
| _Sustained TPS_ | 86.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.32 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
