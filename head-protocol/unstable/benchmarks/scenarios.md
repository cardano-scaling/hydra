--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-13 10:55:03.527047601 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1567.48 | n/a | 18.6 | 19.0 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 268.14 | 268.28 | 3.7 | 5.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1397.95 | n/a | 20.9 | 21.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 218.29 | 221.36 | 4.5 | 6.3 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1522.31 | n/a | 19.2 | 19.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 235.12 | 231.69 | 4.2 | 5.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1632.97 | n/a | 35.9 | 36.3 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.3 | 186.72 | 185.14 | 10.6 | 15.6 |
| Nodes=2, Growing, fire and forget | 60 | 0.0 | 1201.69 | n/a | 48.9 | 49.3 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.4 | 151.60 | 150.96 | 13.0 | 19.0 |
| Nodes=2, Mixed, fire and forget | 60 | 0.0 | 1510.35 | n/a | 38.6 | 39.5 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.4 | 140.30 | 132.55 | 14.1 | 18.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 1119.75 | n/a | 78.2 | 80.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.6 | 162.23 | 160.87 | 18.4 | 24.7 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 1073.48 | n/a | 82.2 | 83.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.9 | 103.62 | 126.49 | 28.7 | 74.3 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 1090.13 | n/a | 81.1 | 82.4 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.7 | 122.03 | 116.50 | 24.3 | 33.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 18.6 |
| _P99_ | 19.0ms |
| _P95_ | 19.0ms |
| _P50_ | 18.7ms |
| _Tx validation time p50 (ms)_ | 8.7 |
| _End-to-end TPS_ | 1567.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 104.50 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 133.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 3.7 |
| _P99_ | 5.9ms |
| _P95_ | 5.2ms |
| _P50_ | 3.5ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 268.14 tx/s |
| _Sustained TPS_ | 268.28 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 268.14 /s |
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
| _Avg. Confirmation Time (ms)_ | 20.9 |
| _P99_ | 21.3ms |
| _P95_ | 21.3ms |
| _P50_ | 21.1ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 1397.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 93.20 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.5 |
| _P99_ | 6.8ms |
| _P95_ | 6.3ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 218.29 tx/s |
| _Sustained TPS_ | 221.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 218.29 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 19.2 |
| _P99_ | 19.6ms |
| _P95_ | 19.5ms |
| _P50_ | 19.3ms |
| _Tx validation time p50 (ms)_ | 8.3 |
| _End-to-end TPS_ | 1522.31 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 101.49 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.2 |
| _P99_ | 6.0ms |
| _P95_ | 5.4ms |
| _P50_ | 4.1ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 235.12 tx/s |
| _Sustained TPS_ | 231.69 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 235.12 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 127.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 35.9 |
| _P99_ | 36.3ms |
| _P95_ | 36.3ms |
| _P50_ | 36.1ms |
| _Tx validation time p50 (ms)_ | 11.4 |
| _End-to-end TPS_ | 1632.97 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 54.43 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 10.6 |
| _P99_ | 19.3ms |
| _P95_ | 15.6ms |
| _P50_ | 9.7ms |
| _Tx validation time p50 (ms)_ | 3.1 |
| _End-to-end TPS_ | 186.72 tx/s |
| _Sustained TPS_ | 185.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 186.72 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 141.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 48.9 |
| _P99_ | 49.6ms |
| _P95_ | 49.3ms |
| _P50_ | 49.1ms |
| _Tx validation time p50 (ms)_ | 18.9 |
| _End-to-end TPS_ | 1201.69 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 40.06 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.0 |
| _P99_ | 19.8ms |
| _P95_ | 19.0ms |
| _P50_ | 12.7ms |
| _Tx validation time p50 (ms)_ | 3.6 |
| _End-to-end TPS_ | 151.60 tx/s |
| _Sustained TPS_ | 150.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 151.60 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 38.6 |
| _P99_ | 39.6ms |
| _P95_ | 39.5ms |
| _P50_ | 38.8ms |
| _Tx validation time p50 (ms)_ | 15.0 |
| _End-to-end TPS_ | 1510.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 50.34 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.1 |
| _P99_ | 60.1ms |
| _P95_ | 18.6ms |
| _P50_ | 12.5ms |
| _Tx validation time p50 (ms)_ | 3.5 |
| _End-to-end TPS_ | 140.30 tx/s |
| _Sustained TPS_ | 132.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 140.30 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 78.2 |
| _P99_ | 80.2ms |
| _P95_ | 80.2ms |
| _P50_ | 77.8ms |
| _Tx validation time p50 (ms)_ | 29.4 |
| _End-to-end TPS_ | 1119.75 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.88 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 142.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 27.6ms |
| _P95_ | 24.7ms |
| _P50_ | 18.0ms |
| _Tx validation time p50 (ms)_ | 4.8 |
| _End-to-end TPS_ | 162.23 tx/s |
| _Sustained TPS_ | 160.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 109.96 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 82.2 |
| _P99_ | 83.5ms |
| _P95_ | 83.5ms |
| _P50_ | 83.1ms |
| _Tx validation time p50 (ms)_ | 27.9 |
| _End-to-end TPS_ | 1073.48 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.86 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.7 |
| _P99_ | 113.7ms |
| _P95_ | 74.3ms |
| _P50_ | 22.9ms |
| _Tx validation time p50 (ms)_ | 6.2 |
| _End-to-end TPS_ | 103.62 tx/s |
| _Sustained TPS_ | 126.49 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 70.23 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.1 |
| _P99_ | 82.4ms |
| _P95_ | 82.4ms |
| _P50_ | 81.0ms |
| _Tx validation time p50 (ms)_ | 33.7 |
| _End-to-end TPS_ | 1090.13 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.23 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 24.3 |
| _P99_ | 39.5ms |
| _P95_ | 33.5ms |
| _P50_ | 24.1ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 122.03 tx/s |
| _Sustained TPS_ | 116.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 82.71 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
