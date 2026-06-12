--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-12 15:33:13.896315977 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | fd75e24c9ea915ce8e48d3ff1d0c54ad09cc01191c24416ad7dba4a3 | 11621 | 
| μHead | 83a964e973c065bbe70588f5e089817f92182ae81743e7a54cf3e29e* | 4856 | 
| νDeposit | c78e8c9205721eb3ef4410f3db9c6169fa6db497c24641d29c20529c | 1615 | 
| νCRS | 09db7ee6cf7a4b358dd5c8a2f19d2c048336ffc5a01ef35a47ca7072 | 2736 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5351 | 8.63 | 2.83 | 0.48 |
| 2| 5452 | 9.41 | 3.09 | 0.49 |
| 3| 5541 | 10.50 | 3.47 | 0.50 |
| 5| 5737 | 10.93 | 3.58 | 0.52 |
| 10| 6221 | 13.57 | 4.42 | 0.57 |
| 50| 10059 | 35.00 | 11.12 | 0.96 |
| 100| 14862 | 61.81 | 19.51 | 1.44 |
| 115| 16298 | 69.61 | 21.96 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2278 | 18.33 | 6.59 | 0.45 |
| 2| 2409 | 18.83 | 7.36 | 0.46 |
| 3| 2540 | 19.85 | 8.30 | 0.48 |
| 5| 2803 | 21.43 | 10.03 | 0.52 |
| 10| 3458 | 26.71 | 14.79 | 0.62 |
| 50| 8698 | 63.97 | 51.00 | 1.42 |
| 75| 11978 | 90.77 | 74.73 | 1.95 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 600 | 16.03 | 5.75 | 0.35 |
| 2| 734 | 16.90 | 6.64 | 0.37 |
| 3| 860 | 17.83 | 7.55 | 0.39 |
| 5| 1123 | 19.62 | 9.35 | 0.43 |
| 10| 1779 | 24.14 | 13.85 | 0.52 |
| 50| 7021 | 62.14 | 50.30 | 1.32 |
| 75| 10294 | 85.30 | 72.91 | 1.82 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 2| 728 | 16.41 | 11.28 | 0.40 |
| 5| 1117 | 19.12 | 13.99 | 0.45 |
| 10| 1776 | 23.70 | 18.52 | 0.55 |
| 50| 7009 | 62.32 | 55.24 | 1.36 |
| 74| 10157 | 85.85 | 77.36 | 1.85 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 18.79 | 13.48 | 0.43 |
| 2| 759 | 19.89 | 14.44 | 0.45 |
| 3| 885 | 20.97 | 15.40 | 0.47 |
| 5| 1148 | 23.05 | 17.29 | 0.51 |
| 10| 1807 | 28.50 | 22.08 | 0.62 |
| 50| 7044 | 73.94 | 60.82 | 1.50 |
| 73| 10053 | 99.99 | 83.07 | 2.00 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 56 | 5562 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5700 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 569 | 5869 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1140 | 6210 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1136 | 6206 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 986 | 34.31 | 65.19 | 0.94 |
| 25 | 1308 | 1423 | 67.64 | 98.26 | 1.47 |
| 30 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 40 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 50 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 100 | 1306 | 1421 | 67.64 | 98.26 | 1.47 |
| 150 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 200 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 200 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 970 | 1444 | 41.40 | 67.69 | 1.04 |
| 25 | 2331 | 2584 | 75.99 | 98.08 | 1.59 |
| 30 | 2352 | 2610 | 75.99 | 98.08 | 1.59 |
| 40 | 2205 | 2456 | 75.99 | 98.03 | 1.58 |
| 50 | 2142 | 2391 | 75.99 | 98.03 | 1.58 |
| 100 | 2268 | 2519 | 75.99 | 98.03 | 1.58 |
| 150 | 2394 | 2655 | 75.99 | 98.08 | 1.59 |
| 200 | 2520 | 2787 | 75.99 | 98.13 | 1.59 |
| 200 | 2289 | 2545 | 75.99 | 98.03 | 1.58 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 97 | 5401 | 21.54 | 43.18 | 0.87 |
| 5 | 555 | 5776 | 35.15 | 54.72 | 1.08 |
| 10 | 1120 | 6235 | 53.35 | 69.52 | 1.36 |
| 10 | 1210 | 6325 | 53.23 | 69.52 | 1.36 |

