--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-15 15:13:44.389542075 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 443.07 | 1692.11 | 66.9 | 67.4 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 146.71 | 157.79 | 6.7 | 9.5 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 357.70 | 784.02 | 82.5 | 83.6 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 106.77 | 107.38 | 9.2 | 12.1 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 426.53 | 1639.54 | 69.6 | 70.2 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 140.05 | 142.95 | 7.0 | 8.7 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 290.93 | 1060.30 | 203.7 | 205.9 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 99.30 | 104.84 | 19.9 | 28.6 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 278.23 | 581.97 | 213.0 | 215.3 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 59.59 | 60.07 | 32.8 | 48.2 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 347.96 | 1093.49 | 170.0 | 172.1 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.9 | 70.29 | 72.76 | 28.1 | 40.6 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 270.11 | 949.39 | 329.5 | 332.1 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.1 | 82.22 | 71.30 | 35.4 | 45.3 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 225.02 | 397.31 | 388.8 | 399.7 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.0 | 44.99 | 47.72 | 65.4 | 102.2 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 273.33 | 831.24 | 324.8 | 328.7 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.6 | 54.60 | 54.74 | 53.5 | 69.6 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 66.9 |
| _P99_ | 67.4ms |
| _P95_ | 67.4ms |
| _P50_ | 67.1ms |
| _End-to-end TPS_ | 443.07 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1692.11 tx/s |
| _Per-snapshot TPS P95_ | 3199.78 tx/s |
| _Per-snapshot TPS max_ | 3333.79 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 6.7 |
| _P99_ | 10.4ms |
| _P95_ | 9.5ms |
| _P50_ | 6.2ms |
| _End-to-end TPS_ | 146.71 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 157.79 tx/s |
| _Per-snapshot TPS P95_ | 180.02 tx/s |
| _Per-snapshot TPS max_ | 186.99 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 82.5 |
| _P99_ | 83.6ms |
| _P95_ | 83.6ms |
| _P50_ | 83.0ms |
| _End-to-end TPS_ | 357.70 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 784.02 tx/s |
| _Per-snapshot TPS P95_ | 1475.83 tx/s |
| _Per-snapshot TPS max_ | 1537.32 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 9.2 |
| _P99_ | 13.3ms |
| _P95_ | 12.1ms |
| _P50_ | 9.1ms |
| _End-to-end TPS_ | 106.77 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 107.38 tx/s |
| _Per-snapshot TPS P95_ | 146.63 tx/s |
| _Per-snapshot TPS max_ | 164.51 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 69.6 |
| _P99_ | 70.2ms |
| _P95_ | 70.2ms |
| _P50_ | 69.9ms |
| _End-to-end TPS_ | 426.53 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1639.54 tx/s |
| _Per-snapshot TPS P95_ | 3100.47 tx/s |
| _Per-snapshot TPS max_ | 3230.33 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.0 |
| _P99_ | 9.2ms |
| _P95_ | 8.7ms |
| _P50_ | 6.9ms |
| _End-to-end TPS_ | 140.05 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 142.95 tx/s |
| _Per-snapshot TPS P95_ | 168.59 tx/s |
| _Per-snapshot TPS max_ | 182.45 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 203.7 |
| _P99_ | 206.0ms |
| _P95_ | 205.9ms |
| _P50_ | 204.4ms |
| _End-to-end TPS_ | 290.93 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1060.30 tx/s |
| _Per-snapshot TPS P95_ | 2009.50 tx/s |
| _Per-snapshot TPS max_ | 2093.87 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 19.9 |
| _P99_ | 30.0ms |
| _P95_ | 28.6ms |
| _P50_ | 19.2ms |
| _End-to-end TPS_ | 99.30 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 104.84 tx/s |
| _Per-snapshot TPS P95_ | 129.30 tx/s |
| _Per-snapshot TPS max_ | 152.83 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 213.0 |
| _P99_ | 215.3ms |
| _P95_ | 215.3ms |
| _P50_ | 213.8ms |
| _End-to-end TPS_ | 278.23 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 581.97 tx/s |
| _Per-snapshot TPS P95_ | 1100.23 tx/s |
| _Per-snapshot TPS max_ | 1146.30 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 32.8 |
| _P99_ | 52.2ms |
| _P95_ | 48.2ms |
| _P50_ | 32.3ms |
| _End-to-end TPS_ | 59.59 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 60.07 tx/s |
| _Per-snapshot TPS P95_ | 100.70 tx/s |
| _Per-snapshot TPS max_ | 122.91 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 170.0 |
| _P99_ | 172.2ms |
| _P95_ | 172.1ms |
| _P50_ | 170.5ms |
| _End-to-end TPS_ | 347.96 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1093.49 tx/s |
| _Per-snapshot TPS P95_ | 2071.41 tx/s |
| _Per-snapshot TPS max_ | 2158.34 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 28.1 |
| _P99_ | 43.7ms |
| _P95_ | 40.6ms |
| _P50_ | 27.6ms |
| _End-to-end TPS_ | 70.29 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 72.76 tx/s |
| _Per-snapshot TPS P95_ | 118.67 tx/s |
| _Per-snapshot TPS max_ | 145.64 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 329.5 |
| _P99_ | 332.2ms |
| _P95_ | 332.1ms |
| _P50_ | 331.7ms |
| _End-to-end TPS_ | 270.11 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 949.39 tx/s |
| _Per-snapshot TPS P95_ | 1800.66 tx/s |
| _Per-snapshot TPS max_ | 1876.33 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 35.4 |
| _P99_ | 50.9ms |
| _P95_ | 45.3ms |
| _P50_ | 34.6ms |
| _End-to-end TPS_ | 82.22 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 71.30 tx/s |
| _Per-snapshot TPS P95_ | 157.69 tx/s |
| _Per-snapshot TPS max_ | 174.50 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 388.8 |
| _P99_ | 399.8ms |
| _P95_ | 399.7ms |
| _P50_ | 386.6ms |
| _End-to-end TPS_ | 225.02 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 397.31 tx/s |
| _Per-snapshot TPS P95_ | 751.64 tx/s |
| _Per-snapshot TPS max_ | 783.14 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 65.4 |
| _P99_ | 111.0ms |
| _P95_ | 102.2ms |
| _P50_ | 67.1ms |
| _End-to-end TPS_ | 44.99 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 47.72 tx/s |
| _Per-snapshot TPS P95_ | 104.26 tx/s |
| _Per-snapshot TPS max_ | 115.23 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 324.8 |
| _P99_ | 328.8ms |
| _P95_ | 328.7ms |
| _P50_ | 325.8ms |
| _End-to-end TPS_ | 273.33 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 831.24 tx/s |
| _Per-snapshot TPS P95_ | 1576.06 tx/s |
| _Per-snapshot TPS max_ | 1642.26 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 53.5 |
| _P99_ | 72.0ms |
| _P95_ | 69.6ms |
| _P50_ | 54.3ms |
| _End-to-end TPS_ | 54.60 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 54.74 tx/s |
| _Per-snapshot TPS P95_ | 104.29 tx/s |
| _Per-snapshot TPS max_ | 114.98 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
