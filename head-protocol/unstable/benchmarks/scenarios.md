--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-15 09:13:28.597364074 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 551.30 | 1835.73 | 53.7 | 54.1 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 199.97 | 211.66 | 4.9 | 6.9 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 480.53 | 966.79 | 61.4 | 62.2 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.2 | 133.51 | 129.36 | 7.4 | 9.2 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 533.01 | 2000.01 | 55.7 | 56.1 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 153.37 | 159.99 | 6.4 | 8.2 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.1 | 428.88 | 1917.57 | 138.6 | 139.1 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.5 | 131.83 | 138.88 | 15.0 | 19.5 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 360.30 | 740.80 | 164.5 | 165.6 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 0.9 | 69.77 | 72.86 | 28.1 | 40.0 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 370.23 | 1099.26 | 160.3 | 161.8 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.7 | 90.77 | 95.77 | 21.8 | 29.0 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 359.24 | 1056.35 | 246.9 | 250.1 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 0.9 | 104.74 | 92.14 | 28.3 | 37.3 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.3 | 282.94 | 433.65 | 312.6 | 315.0 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 1.8 | 48.83 | 49.31 | 59.3 | 81.9 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 324.93 | 993.28 | 274.4 | 275.8 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.4 | 65.23 | 61.29 | 45.2 | 67.3 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 53.7 |
| _P99_ | 54.2ms |
| _P95_ | 54.1ms |
| _P50_ | 53.9ms |
| _End-to-end TPS_ | 551.30 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1835.73 tx/s |
| _Per-snapshot TPS P95_ | 3468.51 tx/s |
| _Per-snapshot TPS max_ | 3613.64 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 4.9 |
| _P99_ | 7.3ms |
| _P95_ | 6.9ms |
| _P50_ | 4.7ms |
| _End-to-end TPS_ | 199.97 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 211.66 tx/s |
| _Per-snapshot TPS P95_ | 233.55 tx/s |
| _Per-snapshot TPS max_ | 237.76 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 61.4 |
| _P99_ | 62.3ms |
| _P95_ | 62.2ms |
| _P50_ | 61.9ms |
| _End-to-end TPS_ | 480.53 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 966.79 tx/s |
| _Per-snapshot TPS P95_ | 1817.85 tx/s |
| _Per-snapshot TPS max_ | 1893.50 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.4 |
| _P99_ | 9.7ms |
| _P95_ | 9.2ms |
| _P50_ | 7.6ms |
| _End-to-end TPS_ | 133.51 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 129.36 tx/s |
| _Per-snapshot TPS P95_ | 176.33 tx/s |
| _Per-snapshot TPS max_ | 191.65 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 55.7 |
| _P99_ | 56.2ms |
| _P95_ | 56.1ms |
| _P50_ | 55.9ms |
| _End-to-end TPS_ | 533.01 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2000.01 tx/s |
| _Per-snapshot TPS P95_ | 3781.64 tx/s |
| _Per-snapshot TPS max_ | 3940.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 6.4 |
| _P99_ | 8.4ms |
| _P95_ | 8.2ms |
| _P50_ | 6.1ms |
| _End-to-end TPS_ | 153.37 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 159.99 tx/s |
| _Per-snapshot TPS P95_ | 187.46 tx/s |
| _Per-snapshot TPS max_ | 195.35 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 138.6 |
| _P99_ | 139.1ms |
| _P95_ | 139.1ms |
| _P50_ | 138.9ms |
| _End-to-end TPS_ | 428.88 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1917.57 tx/s |
| _Per-snapshot TPS P95_ | 3636.11 tx/s |
| _Per-snapshot TPS max_ | 3788.87 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 15.0 |
| _P99_ | 23.8ms |
| _P95_ | 19.5ms |
| _P50_ | 14.6ms |
| _End-to-end TPS_ | 131.83 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 138.88 tx/s |
| _Per-snapshot TPS P95_ | 171.72 tx/s |
| _Per-snapshot TPS max_ | 185.11 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 164.5 |
| _P99_ | 165.6ms |
| _P95_ | 165.6ms |
| _P50_ | 165.2ms |
| _End-to-end TPS_ | 360.30 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 740.80 tx/s |
| _Per-snapshot TPS P95_ | 1400.34 tx/s |
| _Per-snapshot TPS max_ | 1458.96 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 28.1 |
| _P99_ | 45.4ms |
| _P95_ | 40.0ms |
| _P50_ | 26.9ms |
| _End-to-end TPS_ | 69.77 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 72.86 tx/s |
| _Per-snapshot TPS P95_ | 118.20 tx/s |
| _Per-snapshot TPS max_ | 130.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 160.3 |
| _P99_ | 161.9ms |
| _P95_ | 161.8ms |
| _P50_ | 160.8ms |
| _End-to-end TPS_ | 370.23 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1099.26 tx/s |
| _Per-snapshot TPS P95_ | 2081.90 tx/s |
| _Per-snapshot TPS max_ | 2169.25 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 21.8 |
| _P99_ | 34.0ms |
| _P95_ | 29.0ms |
| _P50_ | 21.7ms |
| _End-to-end TPS_ | 90.77 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 95.77 tx/s |
| _Per-snapshot TPS P95_ | 144.56 tx/s |
| _Per-snapshot TPS max_ | 159.43 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 246.9 |
| _P99_ | 250.2ms |
| _P95_ | 250.1ms |
| _P50_ | 246.8ms |
| _End-to-end TPS_ | 359.24 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1056.35 tx/s |
| _Per-snapshot TPS P95_ | 2002.67 tx/s |
| _Per-snapshot TPS max_ | 2086.78 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 28.3 |
| _P99_ | 41.9ms |
| _P95_ | 37.3ms |
| _P50_ | 27.7ms |
| _End-to-end TPS_ | 104.74 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 92.14 tx/s |
| _Per-snapshot TPS P95_ | 212.07 tx/s |
| _Per-snapshot TPS max_ | 228.70 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 312.6 |
| _P99_ | 315.4ms |
| _P95_ | 315.0ms |
| _P50_ | 314.6ms |
| _End-to-end TPS_ | 282.94 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 433.65 tx/s |
| _Per-snapshot TPS P95_ | 819.65 tx/s |
| _Per-snapshot TPS max_ | 853.96 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 59.3 |
| _P99_ | 101.8ms |
| _P95_ | 81.9ms |
| _P50_ | 60.4ms |
| _End-to-end TPS_ | 48.83 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 49.31 tx/s |
| _Per-snapshot TPS P95_ | 116.25 tx/s |
| _Per-snapshot TPS max_ | 127.90 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 274.4 |
| _P99_ | 275.9ms |
| _P95_ | 275.8ms |
| _P50_ | 275.2ms |
| _End-to-end TPS_ | 324.93 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 993.28 tx/s |
| _Per-snapshot TPS P95_ | 1883.32 tx/s |
| _Per-snapshot TPS max_ | 1962.43 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 45.2 |
| _P99_ | 79.0ms |
| _P95_ | 67.3ms |
| _P50_ | 47.2ms |
| _End-to-end TPS_ | 65.23 tx/s |
| _Snapshots observed_ | 63 |
| _Per-snapshot TPS P50_ | 61.29 tx/s |
| _Per-snapshot TPS P95_ | 143.67 tx/s |
| _Per-snapshot TPS max_ | 164.45 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
