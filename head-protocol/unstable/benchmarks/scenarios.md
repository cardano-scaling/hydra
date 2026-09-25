--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-25 13:48:58.0161045 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1075.98 | n/a | 27.2 | 27.7 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 200.87 | 202.65 | 4.9 | 6.1 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 899.65 | n/a | 32.5 | 33.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 159.07 | 159.27 | 6.2 | 8.3 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 959.66 | n/a | 30.3 | 31.0 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 181.66 | 180.95 | 5.4 | 6.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 796.08 | n/a | 73.7 | 75.1 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 145.92 | 144.34 | 13.5 | 15.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 725.46 | n/a | 80.5 | 81.3 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 96.93 | 95.47 | 20.4 | 27.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 741.90 | n/a | 79.1 | 80.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 102.53 | 100.27 | 19.3 | 25.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 619.55 | n/a | 142.0 | 143.7 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 110.29 | 108.36 | 26.8 | 33.3 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 513.80 | n/a | 171.2 | 174.1 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 84.57 | 83.83 | 34.9 | 41.7 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 751.05 | n/a | 116.5 | 119.5 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 91.29 | 87.72 | 32.6 | 40.4 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.2 |
| _P99_ | 27.7ms |
| _P95_ | 27.7ms |
| _P50_ | 27.4ms |
| _Tx validation time p50 (ms)_ | 9.8 |
| _End-to-end TPS_ | 1075.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 71.73 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.9 |
| _P99_ | 6.9ms |
| _P95_ | 6.1ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 200.87 tx/s |
| _Sustained TPS_ | 202.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 200.87 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.5 |
| _P99_ | 33.1ms |
| _P95_ | 33.1ms |
| _P50_ | 32.7ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 899.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.98 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.2 |
| _P99_ | 8.5ms |
| _P95_ | 8.3ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 159.07 tx/s |
| _Sustained TPS_ | 159.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 159.07 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.3 |
| _P99_ | 31.1ms |
| _P95_ | 31.0ms |
| _P50_ | 30.6ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 959.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.98 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 9.0ms |
| _P95_ | 6.7ms |
| _P50_ | 5.3ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 181.66 tx/s |
| _Sustained TPS_ | 180.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 181.66 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 73.7 |
| _P99_ | 75.2ms |
| _P95_ | 75.1ms |
| _P50_ | 74.0ms |
| _Tx validation time p50 (ms)_ | 20.9 |
| _End-to-end TPS_ | 796.08 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.54 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.5 |
| _P99_ | 18.3ms |
| _P95_ | 15.8ms |
| _P50_ | 13.4ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 145.92 tx/s |
| _Sustained TPS_ | 144.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 145.92 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 80.5 |
| _P99_ | 81.4ms |
| _P95_ | 81.3ms |
| _P50_ | 80.8ms |
| _Tx validation time p50 (ms)_ | 23.4 |
| _End-to-end TPS_ | 725.46 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.18 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.4 |
| _P99_ | 35.2ms |
| _P95_ | 27.3ms |
| _P50_ | 19.7ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 96.93 tx/s |
| _Sustained TPS_ | 95.47 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 96.93 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 136.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.1 |
| _P99_ | 80.3ms |
| _P95_ | 80.2ms |
| _P50_ | 79.2ms |
| _Tx validation time p50 (ms)_ | 25.2 |
| _End-to-end TPS_ | 741.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.73 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.3 |
| _P99_ | 27.3ms |
| _P95_ | 25.4ms |
| _P50_ | 18.8ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 102.53 tx/s |
| _Sustained TPS_ | 100.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 102.53 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 142.0 |
| _P99_ | 143.9ms |
| _P95_ | 143.7ms |
| _P50_ | 143.1ms |
| _Tx validation time p50 (ms)_ | 47.3 |
| _End-to-end TPS_ | 619.55 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.77 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.8 |
| _P99_ | 36.4ms |
| _P95_ | 33.3ms |
| _P50_ | 26.6ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 110.29 tx/s |
| _Sustained TPS_ | 108.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 74.75 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 171.2 |
| _P99_ | 174.2ms |
| _P95_ | 174.1ms |
| _P50_ | 173.1ms |
| _Tx validation time p50 (ms)_ | 56.9 |
| _End-to-end TPS_ | 513.80 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.42 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.9 |
| _P99_ | 43.0ms |
| _P95_ | 41.7ms |
| _P50_ | 34.8ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 84.57 tx/s |
| _Sustained TPS_ | 83.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.32 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 116.5 |
| _P99_ | 119.5ms |
| _P95_ | 119.5ms |
| _P50_ | 117.7ms |
| _Tx validation time p50 (ms)_ | 53.8 |
| _End-to-end TPS_ | 751.05 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.69 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.6 |
| _P99_ | 41.6ms |
| _P95_ | 40.4ms |
| _P50_ | 32.5ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 91.29 tx/s |
| _Sustained TPS_ | 87.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 60.86 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
