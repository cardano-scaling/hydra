--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-18 10:08:02.117650694 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 525.67 | 2294.37 | 56.2 | 56.8 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 177.72 | 187.53 | 5.6 | 7.2 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 441.52 | 1068.12 | 66.9 | 67.7 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.2 | 123.50 | 122.00 | 8.0 | 10.0 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 494.05 | 2317.31 | 60.0 | 60.5 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 128.73 | 146.92 | 7.7 | 10.0 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 378.25 | 1465.37 | 156.2 | 157.2 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.5 | 113.76 | 121.49 | 17.4 | 23.5 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 335.45 | 776.28 | 176.0 | 178.6 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 62.21 | 63.37 | 31.6 | 43.7 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 376.00 | 1240.85 | 158.0 | 159.4 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 77.00 | 79.50 | 25.7 | 37.0 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 331.22 | 1585.58 | 267.9 | 271.2 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 90.26 | 77.12 | 32.5 | 40.9 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.3 | 262.43 | 520.25 | 338.5 | 342.2 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.0 | 45.69 | 44.14 | 64.7 | 105.8 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 298.38 | 1227.90 | 299.3 | 301.4 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.4 | 62.13 | 59.37 | 47.8 | 65.1 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 56.2 |
| _P99_ | 56.8ms |
| _P95_ | 56.8ms |
| _P50_ | 56.3ms |
| _End-to-end TPS_ | 525.67 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2294.37 tx/s |
| _Per-snapshot TPS P95_ | 4341.56 tx/s |
| _Per-snapshot TPS max_ | 4523.53 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 8.9ms |
| _P95_ | 7.2ms |
| _P50_ | 5.3ms |
| _End-to-end TPS_ | 177.72 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 187.53 tx/s |
| _Per-snapshot TPS P95_ | 206.35 tx/s |
| _Per-snapshot TPS max_ | 206.88 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 66.9 |
| _P99_ | 67.7ms |
| _P95_ | 67.7ms |
| _P50_ | 67.3ms |
| _End-to-end TPS_ | 441.52 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1068.12 tx/s |
| _Per-snapshot TPS P95_ | 2012.84 tx/s |
| _Per-snapshot TPS max_ | 2096.81 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.0 |
| _P99_ | 10.1ms |
| _P95_ | 10.0ms |
| _P50_ | 8.1ms |
| _End-to-end TPS_ | 123.50 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 122.00 tx/s |
| _Per-snapshot TPS P95_ | 167.13 tx/s |
| _Per-snapshot TPS max_ | 170.46 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 60.0 |
| _P99_ | 60.5ms |
| _P95_ | 60.5ms |
| _P50_ | 60.1ms |
| _End-to-end TPS_ | 494.05 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2317.31 tx/s |
| _Per-snapshot TPS P95_ | 4386.36 tx/s |
| _Per-snapshot TPS max_ | 4570.28 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.7 |
| _P99_ | 18.5ms |
| _P95_ | 10.0ms |
| _P50_ | 6.7ms |
| _End-to-end TPS_ | 128.73 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 146.92 tx/s |
| _Per-snapshot TPS P95_ | 181.66 tx/s |
| _Per-snapshot TPS max_ | 186.33 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 156.2 |
| _P99_ | 157.4ms |
| _P95_ | 157.2ms |
| _P50_ | 156.5ms |
| _End-to-end TPS_ | 378.25 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1465.37 tx/s |
| _Per-snapshot TPS P95_ | 2777.66 tx/s |
| _Per-snapshot TPS max_ | 2894.30 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 17.4 |
| _P99_ | 26.6ms |
| _P95_ | 23.5ms |
| _P50_ | 16.2ms |
| _End-to-end TPS_ | 113.76 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 121.49 tx/s |
| _Per-snapshot TPS P95_ | 151.67 tx/s |
| _Per-snapshot TPS max_ | 160.82 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 176.0 |
| _P99_ | 178.7ms |
| _P95_ | 178.6ms |
| _P50_ | 176.7ms |
| _End-to-end TPS_ | 335.45 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 776.28 tx/s |
| _Per-snapshot TPS P95_ | 1468.43 tx/s |
| _Per-snapshot TPS max_ | 1529.95 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 31.6 |
| _P99_ | 47.3ms |
| _P95_ | 43.7ms |
| _P50_ | 31.8ms |
| _End-to-end TPS_ | 62.21 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 63.37 tx/s |
| _Per-snapshot TPS P95_ | 114.16 tx/s |
| _Per-snapshot TPS max_ | 123.98 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 158.0 |
| _P99_ | 159.5ms |
| _P95_ | 159.4ms |
| _P50_ | 158.6ms |
| _End-to-end TPS_ | 376.00 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1240.85 tx/s |
| _Per-snapshot TPS P95_ | 2350.95 tx/s |
| _Per-snapshot TPS max_ | 2449.62 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 25.7 |
| _P99_ | 39.5ms |
| _P95_ | 37.0ms |
| _P50_ | 26.0ms |
| _End-to-end TPS_ | 77.00 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 79.50 tx/s |
| _Per-snapshot TPS P95_ | 122.02 tx/s |
| _Per-snapshot TPS max_ | 126.34 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 267.9 |
| _P99_ | 271.3ms |
| _P95_ | 271.2ms |
| _P50_ | 268.8ms |
| _End-to-end TPS_ | 331.22 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1585.58 tx/s |
| _Per-snapshot TPS P95_ | 3008.87 tx/s |
| _Per-snapshot TPS max_ | 3135.39 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 32.5 |
| _P99_ | 48.0ms |
| _P95_ | 40.9ms |
| _P50_ | 32.3ms |
| _End-to-end TPS_ | 90.26 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 77.12 tx/s |
| _Per-snapshot TPS P95_ | 158.94 tx/s |
| _Per-snapshot TPS max_ | 168.72 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 338.5 |
| _P99_ | 342.3ms |
| _P95_ | 342.2ms |
| _P50_ | 340.9ms |
| _End-to-end TPS_ | 262.43 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 520.25 tx/s |
| _Per-snapshot TPS P95_ | 984.93 tx/s |
| _Per-snapshot TPS max_ | 1026.23 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 64.7 |
| _P99_ | 130.0ms |
| _P95_ | 105.8ms |
| _P50_ | 62.3ms |
| _End-to-end TPS_ | 45.69 tx/s |
| _Snapshots observed_ | 63 |
| _Per-snapshot TPS P50_ | 44.14 tx/s |
| _Per-snapshot TPS P95_ | 117.34 tx/s |
| _Per-snapshot TPS max_ | 124.41 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 299.3 |
| _P99_ | 301.5ms |
| _P95_ | 301.4ms |
| _P50_ | 299.7ms |
| _End-to-end TPS_ | 298.38 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1227.90 tx/s |
| _Per-snapshot TPS P95_ | 2329.60 tx/s |
| _Per-snapshot TPS max_ | 2427.53 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 47.8 |
| _P99_ | 68.2ms |
| _P95_ | 65.1ms |
| _P50_ | 48.3ms |
| _End-to-end TPS_ | 62.13 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 59.37 tx/s |
| _Per-snapshot TPS P95_ | 148.95 tx/s |
| _Per-snapshot TPS max_ | 167.31 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
