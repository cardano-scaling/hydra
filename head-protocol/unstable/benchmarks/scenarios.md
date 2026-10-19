--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-19 12:50:17.143078821 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 896.01 | n/a | 32.7 | 33.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 183.93 | 179.26 | 5.4 | 7.3 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 837.55 | n/a | 34.9 | 35.6 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 150.56 | 152.52 | 6.6 | 8.5 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 795.96 | n/a | 36.9 | 37.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 174.53 | 176.15 | 5.7 | 7.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 740.27 | n/a | 79.4 | 80.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 124.96 | 127.12 | 15.8 | 19.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 733.56 | n/a | 80.1 | 81.6 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 98.69 | 98.63 | 20.0 | 24.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 786.68 | n/a | 74.7 | 76.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 100.37 | 97.24 | 19.8 | 26.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 668.99 | n/a | 130.3 | 134.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 110.02 | 109.04 | 27.0 | 38.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 523.04 | n/a | 167.2 | 171.8 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 82.65 | 80.90 | 36.0 | 44.8 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 612.25 | n/a | 143.6 | 145.9 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 85.17 | 83.39 | 35.0 | 43.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.7 |
| _P99_ | 33.3ms |
| _P95_ | 33.3ms |
| _P50_ | 33.0ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 896.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.73 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 12.2ms |
| _P95_ | 7.3ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 183.93 tx/s |
| _Sustained TPS_ | 179.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 183.93 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.9 |
| _P99_ | 35.6ms |
| _P95_ | 35.6ms |
| _P50_ | 35.2ms |
| _Tx validation time p50 (ms)_ | 11.4 |
| _End-to-end TPS_ | 837.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.84 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.6 |
| _P99_ | 12.6ms |
| _P95_ | 8.5ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 150.56 tx/s |
| _Sustained TPS_ | 152.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 150.56 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 132.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.9 |
| _P99_ | 37.5ms |
| _P95_ | 37.5ms |
| _P50_ | 37.2ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 795.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 53.06 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 135.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 9.0ms |
| _P95_ | 7.4ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 174.53 tx/s |
| _Sustained TPS_ | 176.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 174.53 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.4 |
| _P99_ | 80.9ms |
| _P95_ | 80.8ms |
| _P50_ | 79.8ms |
| _Tx validation time p50 (ms)_ | 28.0 |
| _End-to-end TPS_ | 740.27 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.68 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.8 |
| _P99_ | 27.2ms |
| _P95_ | 19.8ms |
| _P50_ | 15.4ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 124.96 tx/s |
| _Sustained TPS_ | 127.12 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 124.96 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 136.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 80.1 |
| _P99_ | 81.7ms |
| _P95_ | 81.6ms |
| _P50_ | 80.6ms |
| _Tx validation time p50 (ms)_ | 32.2 |
| _End-to-end TPS_ | 733.56 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.45 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 141.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.0 |
| _P99_ | 26.8ms |
| _P95_ | 24.4ms |
| _P50_ | 20.1ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 98.69 tx/s |
| _Sustained TPS_ | 98.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 98.69 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 74.7 |
| _P99_ | 76.1ms |
| _P95_ | 76.0ms |
| _P50_ | 75.0ms |
| _Tx validation time p50 (ms)_ | 30.9 |
| _End-to-end TPS_ | 786.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.22 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 129.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.8 |
| _P99_ | 30.8ms |
| _P95_ | 26.6ms |
| _P50_ | 19.8ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 100.37 tx/s |
| _Sustained TPS_ | 97.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 100.37 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 130.3 |
| _P99_ | 134.3ms |
| _P95_ | 134.3ms |
| _P50_ | 130.7ms |
| _Tx validation time p50 (ms)_ | 57.2 |
| _End-to-end TPS_ | 668.99 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.87 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 142.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.0 |
| _P99_ | 42.6ms |
| _P95_ | 38.2ms |
| _P50_ | 26.0ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 110.02 tx/s |
| _Sustained TPS_ | 109.04 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 73.35 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 167.2 |
| _P99_ | 171.9ms |
| _P95_ | 171.8ms |
| _P50_ | 166.8ms |
| _Tx validation time p50 (ms)_ | 64.4 |
| _End-to-end TPS_ | 523.04 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.62 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.0 |
| _P99_ | 61.2ms |
| _P95_ | 44.8ms |
| _P50_ | 35.3ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 82.65 tx/s |
| _Sustained TPS_ | 80.90 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 55.10 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 143.6 |
| _P99_ | 146.0ms |
| _P95_ | 145.9ms |
| _P50_ | 143.5ms |
| _Tx validation time p50 (ms)_ | 41.4 |
| _End-to-end TPS_ | 612.25 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.61 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.0 |
| _P99_ | 46.2ms |
| _P95_ | 43.6ms |
| _P50_ | 35.0ms |
| _Tx validation time p50 (ms)_ | 9.7 |
| _End-to-end TPS_ | 85.17 tx/s |
| _Sustained TPS_ | 83.39 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 56.78 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
