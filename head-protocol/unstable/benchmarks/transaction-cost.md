--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-15 12:39:38.227786166 UTC |
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
| 1| 5352 | 9.20 | 3.04 | 0.48 |
| 2| 5445 | 9.24 | 3.03 | 0.49 |
| 3| 5547 | 9.91 | 3.25 | 0.50 |
| 5| 5737 | 11.05 | 3.62 | 0.52 |
| 10| 6217 | 13.76 | 4.50 | 0.57 |
| 50| 10059 | 34.57 | 10.97 | 0.95 |
| 100| 14856 | 62.01 | 19.59 | 1.45 |
| 115| 16296 | 69.78 | 22.00 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2275 | 18.33 | 6.59 | 0.45 |
| 2| 2409 | 19.33 | 7.53 | 0.47 |
| 3| 2540 | 19.57 | 8.21 | 0.48 |
| 5| 2801 | 21.94 | 10.20 | 0.52 |
| 10| 3458 | 25.96 | 14.54 | 0.62 |
| 50| 8699 | 64.29 | 51.10 | 1.42 |
| 75| 11974 | 90.15 | 74.56 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 16.03 | 5.75 | 0.35 |
| 2| 734 | 16.94 | 6.65 | 0.37 |
| 3| 861 | 17.84 | 7.56 | 0.39 |
| 5| 1123 | 19.66 | 9.36 | 0.43 |
| 10| 1778 | 24.01 | 13.82 | 0.52 |
| 50| 7019 | 62.36 | 50.36 | 1.33 |
| 75| 10294 | 86.36 | 73.21 | 1.83 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 3| 858 | 17.29 | 12.18 | 0.42 |
| 10| 1772 | 23.68 | 18.52 | 0.55 |
| 50| 7013 | 62.03 | 55.16 | 1.36 |
| 74| 10157 | 85.63 | 77.30 | 1.85 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 18.79 | 13.48 | 0.43 |
| 2| 754 | 19.85 | 14.43 | 0.45 |
| 3| 891 | 20.94 | 15.39 | 0.47 |
| 5| 1148 | 23.10 | 17.30 | 0.51 |
| 10| 1799 | 28.52 | 22.09 | 0.62 |
| 50| 7044 | 73.70 | 60.75 | 1.49 |
| 72| 9922 | 99.21 | 82.20 | 1.98 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5564 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 284 | 5698 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 570 | 5869 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1138 | 6208 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1139 | 6208 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 568 | 985 | 34.31 | 65.19 | 0.94 |
| 25 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 30 | 1309 | 1424 | 67.64 | 98.26 | 1.47 |
| 40 | 1307 | 1426 | 67.64 | 98.26 | 1.47 |
| 50 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 100 | 1307 | 1426 | 67.64 | 98.26 | 1.47 |
| 150 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 200 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 980 | 1455 | 41.38 | 67.69 | 1.04 |
| 25 | 2310 | 2566 | 75.99 | 98.08 | 1.58 |
| 30 | 2394 | 2650 | 75.99 | 98.08 | 1.59 |
| 40 | 1995 | 2236 | 75.97 | 97.97 | 1.57 |
| 50 | 2478 | 2743 | 75.97 | 98.08 | 1.59 |
| 100 | 2310 | 2567 | 75.99 | 98.08 | 1.58 |
| 150 | 2436 | 2699 | 75.99 | 98.08 | 1.59 |
| 200 | 2100 | 2347 | 75.97 | 97.98 | 1.57 |
| 200 | 2436 | 2699 | 75.97 | 98.08 | 1.59 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 98 | 5402 | 21.42 | 43.15 | 0.87 |
| 5 | 550 | 5771 | 35.15 | 54.72 | 1.08 |
| 10 | 1140 | 6256 | 53.35 | 69.53 | 1.36 |
| 10 | 1100 | 6215 | 53.35 | 69.52 | 1.36 |

