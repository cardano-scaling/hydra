--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-15 12:53:07.76872315 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 438.43 | 1517.46 | 67.1 | 67.8 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 164.40 | 168.82 | 6.0 | 7.4 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 428.07 | 809.41 | 68.7 | 69.8 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 107.87 | 106.15 | 9.1 | 11.3 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 467.70 | 1418.87 | 63.2 | 63.8 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.3 | 119.97 | 123.51 | 8.2 | 9.9 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 329.18 | 1365.84 | 180.1 | 182.0 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 98.82 | 108.20 | 20.0 | 27.9 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 302.07 | 547.56 | 196.1 | 198.3 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.1 | 56.77 | 59.56 | 34.5 | 48.3 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 335.55 | 1155.07 | 176.8 | 178.5 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.9 | 70.44 | 74.57 | 27.8 | 42.0 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 294.34 | 969.38 | 302.3 | 303.3 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.1 | 82.30 | 69.03 | 35.6 | 45.7 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 220.30 | 408.29 | 404.9 | 408.3 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.3 | 39.92 | 38.31 | 70.5 | 102.0 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 269.46 | 890.25 | 330.5 | 332.8 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.6 | 57.29 | 51.63 | 51.5 | 80.0 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 67.1 |
| _P99_ | 68.0ms |
| _P95_ | 67.8ms |
| _P50_ | 67.4ms |
| _End-to-end TPS_ | 438.43 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1517.46 tx/s |
| _Per-snapshot TPS P95_ | 2867.87 tx/s |
| _Per-snapshot TPS max_ | 2987.91 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 8.3ms |
| _P95_ | 7.4ms |
| _P50_ | 5.8ms |
| _End-to-end TPS_ | 164.40 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 168.82 tx/s |
| _Per-snapshot TPS P95_ | 185.11 tx/s |
| _Per-snapshot TPS max_ | 186.17 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 68.7 |
| _P99_ | 69.8ms |
| _P95_ | 69.8ms |
| _P50_ | 69.3ms |
| _End-to-end TPS_ | 428.07 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 809.41 tx/s |
| _Per-snapshot TPS P95_ | 1520.55 tx/s |
| _Per-snapshot TPS max_ | 1583.76 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 9.1 |
| _P99_ | 11.5ms |
| _P95_ | 11.3ms |
| _P50_ | 9.3ms |
| _End-to-end TPS_ | 107.87 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 106.15 tx/s |
| _Per-snapshot TPS P95_ | 152.16 tx/s |
| _Per-snapshot TPS max_ | 155.32 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 63.2 |
| _P99_ | 63.9ms |
| _P95_ | 63.8ms |
| _P50_ | 63.5ms |
| _End-to-end TPS_ | 467.70 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1418.87 tx/s |
| _Per-snapshot TPS P95_ | 2679.13 tx/s |
| _Per-snapshot TPS max_ | 2791.15 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.2 |
| _P99_ | 11.1ms |
| _P95_ | 9.9ms |
| _P50_ | 7.9ms |
| _End-to-end TPS_ | 119.97 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 123.51 tx/s |
| _Per-snapshot TPS P95_ | 162.75 tx/s |
| _Per-snapshot TPS max_ | 172.34 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 180.1 |
| _P99_ | 182.0ms |
| _P95_ | 182.0ms |
| _P50_ | 180.4ms |
| _End-to-end TPS_ | 329.18 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1365.84 tx/s |
| _Per-snapshot TPS P95_ | 2589.47 tx/s |
| _Per-snapshot TPS max_ | 2698.24 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 20.0 |
| _P99_ | 32.6ms |
| _P95_ | 27.9ms |
| _P50_ | 18.7ms |
| _End-to-end TPS_ | 98.82 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 108.20 tx/s |
| _Per-snapshot TPS P95_ | 139.82 tx/s |
| _Per-snapshot TPS max_ | 142.56 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 196.1 |
| _P99_ | 198.3ms |
| _P95_ | 198.3ms |
| _P50_ | 197.0ms |
| _End-to-end TPS_ | 302.07 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 547.56 tx/s |
| _Per-snapshot TPS P95_ | 1034.09 tx/s |
| _Per-snapshot TPS max_ | 1077.34 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 34.5 |
| _P99_ | 51.4ms |
| _P95_ | 48.3ms |
| _P50_ | 35.4ms |
| _End-to-end TPS_ | 56.77 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 59.56 tx/s |
| _Per-snapshot TPS P95_ | 109.14 tx/s |
| _Per-snapshot TPS max_ | 119.05 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 176.8 |
| _P99_ | 178.5ms |
| _P95_ | 178.5ms |
| _P50_ | 177.2ms |
| _End-to-end TPS_ | 335.55 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1155.07 tx/s |
| _Per-snapshot TPS P95_ | 2188.73 tx/s |
| _Per-snapshot TPS max_ | 2280.61 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 27.8 |
| _P99_ | 49.7ms |
| _P95_ | 42.0ms |
| _P50_ | 27.2ms |
| _End-to-end TPS_ | 70.44 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 74.57 tx/s |
| _Per-snapshot TPS P95_ | 106.40 tx/s |
| _Per-snapshot TPS max_ | 132.49 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 302.3 |
| _P99_ | 303.7ms |
| _P95_ | 303.3ms |
| _P50_ | 302.8ms |
| _End-to-end TPS_ | 294.34 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 969.38 tx/s |
| _Per-snapshot TPS P95_ | 1838.33 tx/s |
| _Per-snapshot TPS max_ | 1915.57 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 35.6 |
| _P99_ | 48.4ms |
| _P95_ | 45.7ms |
| _P50_ | 35.0ms |
| _End-to-end TPS_ | 82.30 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 69.03 tx/s |
| _Per-snapshot TPS P95_ | 151.75 tx/s |
| _Per-snapshot TPS max_ | 167.33 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 404.9 |
| _P99_ | 408.4ms |
| _P95_ | 408.3ms |
| _P50_ | 405.2ms |
| _End-to-end TPS_ | 220.30 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 408.29 tx/s |
| _Per-snapshot TPS P95_ | 772.73 tx/s |
| _Per-snapshot TPS max_ | 805.12 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 70.5 |
| _P99_ | 127.3ms |
| _P95_ | 102.0ms |
| _P50_ | 71.7ms |
| _End-to-end TPS_ | 39.92 tx/s |
| _Snapshots observed_ | 65 |
| _Per-snapshot TPS P50_ | 38.31 tx/s |
| _Per-snapshot TPS P95_ | 90.45 tx/s |
| _Per-snapshot TPS max_ | 110.17 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 330.5 |
| _P99_ | 332.9ms |
| _P95_ | 332.8ms |
| _P50_ | 331.9ms |
| _End-to-end TPS_ | 269.46 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 890.25 tx/s |
| _Per-snapshot TPS P95_ | 1688.28 tx/s |
| _Per-snapshot TPS max_ | 1759.22 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 51.5 |
| _P99_ | 86.2ms |
| _P95_ | 80.0ms |
| _P50_ | 48.6ms |
| _End-to-end TPS_ | 57.29 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 51.63 tx/s |
| _Per-snapshot TPS P95_ | 108.55 tx/s |
| _Per-snapshot TPS max_ | 177.12 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
