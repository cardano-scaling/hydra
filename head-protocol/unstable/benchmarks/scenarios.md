--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-15 15:29:13.21357126 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 449.63 | 1454.81 | 65.7 | 66.4 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 172.12 | 178.82 | 5.7 | 7.0 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 410.48 | 870.54 | 71.8 | 72.8 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.2 | 120.11 | 122.12 | 8.2 | 10.8 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 447.01 | 1496.55 | 66.3 | 66.8 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 142.09 | 146.30 | 7.0 | 8.9 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 341.38 | 1474.54 | 173.5 | 174.5 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 107.15 | 117.78 | 18.5 | 26.9 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 320.87 | 632.88 | 184.7 | 185.9 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 60.33 | 61.82 | 32.2 | 46.4 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 351.58 | 1458.87 | 169.1 | 170.3 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 79.00 | 82.31 | 24.9 | 37.6 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 302.47 | 966.18 | 294.3 | 296.5 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.1 | 85.10 | 73.36 | 34.7 | 43.1 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 252.78 | 463.08 | 348.6 | 355.6 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.1 | 43.79 | 45.12 | 65.4 | 105.6 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 291.77 | 994.33 | 303.9 | 308.1 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 59.79 | 56.82 | 49.4 | 66.4 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 65.7 |
| _P99_ | 66.4ms |
| _P95_ | 66.4ms |
| _P50_ | 65.9ms |
| _End-to-end TPS_ | 449.63 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1454.81 tx/s |
| _Per-snapshot TPS P95_ | 2748.25 tx/s |
| _Per-snapshot TPS max_ | 2863.22 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 7.5ms |
| _P95_ | 7.0ms |
| _P50_ | 5.5ms |
| _End-to-end TPS_ | 172.12 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 178.82 tx/s |
| _Per-snapshot TPS P95_ | 199.41 tx/s |
| _Per-snapshot TPS max_ | 204.83 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 71.8 |
| _P99_ | 72.9ms |
| _P95_ | 72.8ms |
| _P50_ | 72.3ms |
| _End-to-end TPS_ | 410.48 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 870.54 tx/s |
| _Per-snapshot TPS P95_ | 1638.02 tx/s |
| _Per-snapshot TPS max_ | 1706.24 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.2 |
| _P99_ | 12.1ms |
| _P95_ | 10.8ms |
| _P50_ | 8.1ms |
| _End-to-end TPS_ | 120.11 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 122.12 tx/s |
| _Per-snapshot TPS P95_ | 166.14 tx/s |
| _Per-snapshot TPS max_ | 181.58 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 66.3 |
| _P99_ | 66.9ms |
| _P95_ | 66.8ms |
| _P50_ | 66.5ms |
| _End-to-end TPS_ | 447.01 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1496.55 tx/s |
| _Per-snapshot TPS P95_ | 2827.75 tx/s |
| _Per-snapshot TPS max_ | 2946.07 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.0 |
| _P99_ | 9.2ms |
| _P95_ | 8.9ms |
| _P50_ | 6.7ms |
| _End-to-end TPS_ | 142.09 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 146.30 tx/s |
| _Per-snapshot TPS P95_ | 171.82 tx/s |
| _Per-snapshot TPS max_ | 190.13 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 173.5 |
| _P99_ | 174.5ms |
| _P95_ | 174.5ms |
| _P50_ | 173.9ms |
| _End-to-end TPS_ | 341.38 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1474.54 tx/s |
| _Per-snapshot TPS P95_ | 2795.78 tx/s |
| _Per-snapshot TPS max_ | 2913.22 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 18.5 |
| _P99_ | 28.7ms |
| _P95_ | 26.9ms |
| _P50_ | 17.1ms |
| _End-to-end TPS_ | 107.15 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 117.78 tx/s |
| _Per-snapshot TPS P95_ | 148.89 tx/s |
| _Per-snapshot TPS max_ | 162.03 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 184.7 |
| _P99_ | 186.0ms |
| _P95_ | 185.9ms |
| _P50_ | 185.5ms |
| _End-to-end TPS_ | 320.87 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 632.88 tx/s |
| _Per-snapshot TPS P95_ | 1196.00 tx/s |
| _Per-snapshot TPS max_ | 1246.05 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 32.2 |
| _P99_ | 56.5ms |
| _P95_ | 46.4ms |
| _P50_ | 31.9ms |
| _End-to-end TPS_ | 60.33 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 61.82 tx/s |
| _Per-snapshot TPS P95_ | 103.90 tx/s |
| _Per-snapshot TPS max_ | 111.21 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 169.1 |
| _P99_ | 170.4ms |
| _P95_ | 170.3ms |
| _P50_ | 169.5ms |
| _End-to-end TPS_ | 351.58 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1458.87 tx/s |
| _Per-snapshot TPS P95_ | 2765.83 tx/s |
| _Per-snapshot TPS max_ | 2882.01 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 24.9 |
| _P99_ | 40.4ms |
| _P95_ | 37.6ms |
| _P50_ | 24.6ms |
| _End-to-end TPS_ | 79.00 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 82.31 tx/s |
| _Per-snapshot TPS P95_ | 116.67 tx/s |
| _Per-snapshot TPS max_ | 130.96 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 294.3 |
| _P99_ | 296.7ms |
| _P95_ | 296.5ms |
| _P50_ | 294.1ms |
| _End-to-end TPS_ | 302.47 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 966.18 tx/s |
| _Per-snapshot TPS P95_ | 1832.12 tx/s |
| _Per-snapshot TPS max_ | 1909.10 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 34.7 |
| _P99_ | 46.2ms |
| _P95_ | 43.1ms |
| _P50_ | 34.0ms |
| _End-to-end TPS_ | 85.10 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 73.36 tx/s |
| _Per-snapshot TPS P95_ | 146.03 tx/s |
| _Per-snapshot TPS max_ | 175.50 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 348.6 |
| _P99_ | 355.7ms |
| _P95_ | 355.6ms |
| _P50_ | 348.3ms |
| _End-to-end TPS_ | 252.78 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 463.08 tx/s |
| _Per-snapshot TPS P95_ | 876.30 tx/s |
| _Per-snapshot TPS max_ | 913.03 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 65.4 |
| _P99_ | 116.4ms |
| _P95_ | 105.6ms |
| _P50_ | 61.5ms |
| _End-to-end TPS_ | 43.79 tx/s |
| _Snapshots observed_ | 63 |
| _Per-snapshot TPS P50_ | 45.12 tx/s |
| _Per-snapshot TPS P95_ | 104.58 tx/s |
| _Per-snapshot TPS max_ | 122.40 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 303.9 |
| _P99_ | 308.2ms |
| _P95_ | 308.1ms |
| _P50_ | 304.6ms |
| _End-to-end TPS_ | 291.77 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 994.33 tx/s |
| _Per-snapshot TPS P95_ | 1885.79 tx/s |
| _Per-snapshot TPS max_ | 1965.03 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 49.4 |
| _P99_ | 79.4ms |
| _P95_ | 66.4ms |
| _P50_ | 49.2ms |
| _End-to-end TPS_ | 59.79 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 56.82 tx/s |
| _Per-snapshot TPS P95_ | 109.03 tx/s |
| _Per-snapshot TPS max_ | 126.32 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
