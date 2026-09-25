--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-25 14:42:53.243339367 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1049.24 | n/a | 27.8 | 28.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 203.88 | 202.99 | 4.8 | 5.8 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 907.08 | n/a | 32.2 | 32.8 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 170.01 | 169.14 | 5.8 | 6.7 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 954.55 | n/a | 30.7 | 31.2 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 177.78 | 173.90 | 5.6 | 7.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 919.89 | n/a | 63.6 | 64.5 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 141.15 | 141.08 | 14.0 | 17.1 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 763.30 | n/a | 76.7 | 78.4 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 100.05 | 96.98 | 19.7 | 24.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 861.87 | n/a | 67.8 | 69.4 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 103.70 | 98.36 | 19.1 | 25.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 630.06 | n/a | 140.8 | 142.6 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 102.80 | 104.86 | 28.7 | 36.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 510.56 | n/a | 172.7 | 176.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 83.34 | 82.57 | 35.3 | 44.7 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 555.18 | n/a | 159.3 | 161.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 84.72 | 81.34 | 35.1 | 44.1 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.8 |
| _P99_ | 28.4ms |
| _P95_ | 28.4ms |
| _P50_ | 28.0ms |
| _Tx validation time p50 (ms)_ | 10.8 |
| _End-to-end TPS_ | 1049.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 69.95 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.8 |
| _P99_ | 6.8ms |
| _P95_ | 5.8ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 203.88 tx/s |
| _Sustained TPS_ | 202.99 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 203.88 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.2 |
| _P99_ | 32.9ms |
| _P95_ | 32.8ms |
| _P50_ | 32.5ms |
| _Tx validation time p50 (ms)_ | 10.9 |
| _End-to-end TPS_ | 907.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 60.47 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 7.0ms |
| _P95_ | 6.7ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 170.01 tx/s |
| _Sustained TPS_ | 169.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 170.01 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.7 |
| _P99_ | 31.3ms |
| _P95_ | 31.2ms |
| _P50_ | 30.9ms |
| _Tx validation time p50 (ms)_ | 10.5 |
| _End-to-end TPS_ | 954.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.64 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 7.5ms |
| _P95_ | 7.3ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 177.78 tx/s |
| _Sustained TPS_ | 173.90 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 177.78 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 127.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 63.6 |
| _P99_ | 64.8ms |
| _P95_ | 64.5ms |
| _P50_ | 63.9ms |
| _Tx validation time p50 (ms)_ | 25.1 |
| _End-to-end TPS_ | 919.89 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.66 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.0 |
| _P99_ | 18.8ms |
| _P95_ | 17.1ms |
| _P50_ | 13.6ms |
| _Tx validation time p50 (ms)_ | 3.8 |
| _End-to-end TPS_ | 141.15 tx/s |
| _Sustained TPS_ | 141.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 141.15 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 76.7 |
| _P99_ | 78.4ms |
| _P95_ | 78.4ms |
| _P50_ | 77.2ms |
| _Tx validation time p50 (ms)_ | 26.1 |
| _End-to-end TPS_ | 763.30 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.44 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.7 |
| _P99_ | 25.1ms |
| _P95_ | 24.4ms |
| _P50_ | 19.9ms |
| _Tx validation time p50 (ms)_ | 5.9 |
| _End-to-end TPS_ | 100.05 tx/s |
| _Sustained TPS_ | 96.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 100.05 /s |
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
| _Avg. Confirmation Time (ms)_ | 67.8 |
| _P99_ | 69.5ms |
| _P95_ | 69.4ms |
| _P50_ | 68.3ms |
| _Tx validation time p50 (ms)_ | 25.6 |
| _End-to-end TPS_ | 861.87 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.73 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.1 |
| _P99_ | 26.6ms |
| _P95_ | 25.6ms |
| _P50_ | 18.9ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 103.70 tx/s |
| _Sustained TPS_ | 98.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 103.70 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 140.8 |
| _P99_ | 142.7ms |
| _P95_ | 142.6ms |
| _P50_ | 141.2ms |
| _Tx validation time p50 (ms)_ | 44.1 |
| _End-to-end TPS_ | 630.06 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.00 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.7 |
| _P99_ | 38.6ms |
| _P95_ | 36.8ms |
| _P50_ | 28.7ms |
| _Tx validation time p50 (ms)_ | 7.8 |
| _End-to-end TPS_ | 102.80 tx/s |
| _Sustained TPS_ | 104.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 69.68 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 172.7 |
| _P99_ | 176.1ms |
| _P95_ | 176.0ms |
| _P50_ | 174.6ms |
| _Tx validation time p50 (ms)_ | 70.3 |
| _End-to-end TPS_ | 510.56 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.35 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.3 |
| _P99_ | 47.6ms |
| _P95_ | 44.7ms |
| _P50_ | 34.8ms |
| _Tx validation time p50 (ms)_ | 9.4 |
| _End-to-end TPS_ | 83.34 tx/s |
| _Sustained TPS_ | 82.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.48 /s |
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
| _Avg. Confirmation Time (ms)_ | 159.3 |
| _P99_ | 161.9ms |
| _P95_ | 161.8ms |
| _P50_ | 160.5ms |
| _Tx validation time p50 (ms)_ | 53.7 |
| _End-to-end TPS_ | 555.18 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.34 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.1 |
| _P99_ | 46.6ms |
| _P95_ | 44.1ms |
| _P50_ | 35.3ms |
| _Tx validation time p50 (ms)_ | 9.5 |
| _End-to-end TPS_ | 84.72 tx/s |
| _Sustained TPS_ | 81.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.42 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      
