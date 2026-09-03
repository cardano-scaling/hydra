--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-03 10:31:17.127460478 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1068.96 | n/a | 27.2 | 27.8 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 218.16 | 219.28 | 4.5 | 5.1 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 776.57 | n/a | 37.8 | 38.4 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 171.22 | 173.73 | 5.8 | 7.2 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 809.52 | n/a | 36.2 | 36.8 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 192.56 | 188.75 | 5.1 | 5.8 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 901.83 | n/a | 65.0 | 65.7 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 146.11 | 148.10 | 13.5 | 16.7 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 815.88 | n/a | 71.8 | 73.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 104.27 | 102.00 | 18.9 | 24.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 903.09 | n/a | 64.9 | 65.5 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 108.63 | 103.74 | 18.2 | 22.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 595.95 | n/a | 148.2 | 149.7 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 105.07 | 102.98 | 28.1 | 41.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 543.89 | n/a | 162.0 | 164.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 83.88 | 84.98 | 34.9 | 41.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 652.96 | n/a | 135.1 | 136.9 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 95.29 | 92.53 | 31.0 | 41.7 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.2 |
| _P99_ | 27.8ms |
| _P95_ | 27.8ms |
| _P50_ | 27.4ms |
| _Tx validation time p50 (ms)_ | 18.5 |
| _End-to-end TPS_ | 1068.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 71.26 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 141.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.5 |
| _P99_ | 6.1ms |
| _P95_ | 5.1ms |
| _P50_ | 4.5ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 218.16 tx/s |
| _Sustained TPS_ | 219.28 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 218.16 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 37.8 |
| _P99_ | 38.4ms |
| _P95_ | 38.4ms |
| _P50_ | 38.1ms |
| _Tx validation time p50 (ms)_ | 23.0 |
| _End-to-end TPS_ | 776.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 51.77 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 7.7ms |
| _P95_ | 7.2ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 171.22 tx/s |
| _Sustained TPS_ | 173.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 171.22 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.2 |
| _P99_ | 36.8ms |
| _P95_ | 36.8ms |
| _P50_ | 36.5ms |
| _Tx validation time p50 (ms)_ | 17.5 |
| _End-to-end TPS_ | 809.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 53.97 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 7.4ms |
| _P95_ | 5.8ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 192.56 tx/s |
| _Sustained TPS_ | 188.75 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 192.56 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 65.0 |
| _P99_ | 66.0ms |
| _P95_ | 65.7ms |
| _P50_ | 65.3ms |
| _Tx validation time p50 (ms)_ | 20.1 |
| _End-to-end TPS_ | 901.83 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.06 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.5 |
| _P99_ | 18.9ms |
| _P95_ | 16.7ms |
| _P50_ | 13.1ms |
| _Tx validation time p50 (ms)_ | 3.9 |
| _End-to-end TPS_ | 146.11 tx/s |
| _Sustained TPS_ | 148.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 146.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 71.8 |
| _P99_ | 73.3ms |
| _P95_ | 73.2ms |
| _P50_ | 72.2ms |
| _Tx validation time p50 (ms)_ | 27.0 |
| _End-to-end TPS_ | 815.88 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.20 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.9 |
| _P99_ | 25.9ms |
| _P95_ | 24.3ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 104.27 tx/s |
| _Sustained TPS_ | 102.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 104.27 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 132.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 64.9 |
| _P99_ | 65.9ms |
| _P95_ | 65.5ms |
| _P50_ | 65.1ms |
| _Tx validation time p50 (ms)_ | 22.2 |
| _End-to-end TPS_ | 903.09 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.10 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.2 |
| _P99_ | 26.0ms |
| _P95_ | 22.9ms |
| _P50_ | 18.4ms |
| _Tx validation time p50 (ms)_ | 5.7 |
| _End-to-end TPS_ | 108.63 tx/s |
| _Sustained TPS_ | 103.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 108.63 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 148.2 |
| _P99_ | 149.7ms |
| _P95_ | 149.7ms |
| _P50_ | 149.3ms |
| _Tx validation time p50 (ms)_ | 47.2 |
| _End-to-end TPS_ | 595.95 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.24 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.1 |
| _P99_ | 46.0ms |
| _P95_ | 41.2ms |
| _P50_ | 26.9ms |
| _Tx validation time p50 (ms)_ | 7.4 |
| _End-to-end TPS_ | 105.07 tx/s |
| _Sustained TPS_ | 102.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 71.21 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 162.0 |
| _P99_ | 164.4ms |
| _P95_ | 164.2ms |
| _P50_ | 162.3ms |
| _Tx validation time p50 (ms)_ | 47.9 |
| _End-to-end TPS_ | 543.89 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.09 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.9 |
| _P99_ | 45.1ms |
| _P95_ | 41.9ms |
| _P50_ | 34.6ms |
| _Tx validation time p50 (ms)_ | 10.5 |
| _End-to-end TPS_ | 83.88 tx/s |
| _Sustained TPS_ | 84.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.85 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 135.1 |
| _P99_ | 137.0ms |
| _P95_ | 136.9ms |
| _P50_ | 136.0ms |
| _Tx validation time p50 (ms)_ | 34.2 |
| _End-to-end TPS_ | 652.96 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.51 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.0 |
| _P99_ | 47.8ms |
| _P95_ | 41.7ms |
| _P50_ | 30.4ms |
| _Tx validation time p50 (ms)_ | 8.7 |
| _End-to-end TPS_ | 95.29 tx/s |
| _Sustained TPS_ | 92.53 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 65.64 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
