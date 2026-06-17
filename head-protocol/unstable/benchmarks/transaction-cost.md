--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-17 14:45:24.461683645 UTC |
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
| 1| 5356 | 8.87 | 2.91 | 0.48 |
| 2| 5448 | 9.77 | 3.22 | 0.49 |
| 3| 5541 | 9.86 | 3.23 | 0.50 |
| 5| 5741 | 10.77 | 3.52 | 0.52 |
| 10| 6217 | 13.51 | 4.40 | 0.56 |
| 50| 10058 | 35.07 | 11.14 | 0.96 |
| 100| 14859 | 61.67 | 19.48 | 1.44 |
| 115| 16299 | 69.73 | 21.97 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2278 | 18.41 | 6.62 | 0.45 |
| 2| 2410 | 19.40 | 7.55 | 0.47 |
| 3| 2540 | 19.72 | 8.26 | 0.48 |
| 5| 2808 | 22.12 | 10.26 | 0.53 |
| 10| 3457 | 26.65 | 14.77 | 0.62 |
| 50| 8697 | 64.75 | 51.23 | 1.43 |
| 75| 11972 | 90.34 | 74.57 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 16.03 | 5.75 | 0.35 |
| 2| 730 | 16.90 | 6.64 | 0.37 |
| 3| 864 | 17.84 | 7.56 | 0.39 |
| 5| 1125 | 19.60 | 9.34 | 0.43 |
| 10| 1779 | 24.01 | 13.82 | 0.52 |
| 50| 7024 | 61.74 | 50.19 | 1.32 |
| 75| 10294 | 85.34 | 72.92 | 1.82 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 2| 723 | 16.41 | 11.28 | 0.40 |
| 3| 854 | 17.31 | 12.18 | 0.42 |
| 10| 1769 | 23.70 | 18.52 | 0.55 |
| 75| 10285 | 85.98 | 78.05 | 1.86 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 18.78 | 13.47 | 0.43 |
| 2| 751 | 19.89 | 14.44 | 0.45 |
| 3| 881 | 20.97 | 15.40 | 0.47 |
| 5| 1153 | 23.14 | 17.31 | 0.51 |
| 10| 1804 | 28.52 | 22.09 | 0.62 |
| 50| 7045 | 73.80 | 60.78 | 1.49 |
| 73| 10049 | 99.53 | 82.94 | 1.99 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5530 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5564 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 284 | 5698 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 568 | 5867 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1138 | 6207 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1137 | 6207 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 986 | 34.31 | 65.19 | 0.94 |
| 25 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 30 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 40 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 50 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 100 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 150 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 200 | 1309 | 1424 | 67.64 | 98.26 | 1.47 |
| 200 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 970 | 1444 | 41.40 | 67.69 | 1.04 |
| 25 | 2037 | 2280 | 75.99 | 97.98 | 1.57 |
| 30 | 2058 | 2302 | 75.97 | 97.98 | 1.57 |
| 40 | 2226 | 2478 | 75.99 | 98.03 | 1.58 |
| 50 | 1953 | 2193 | 75.99 | 97.98 | 1.57 |
| 100 | 2499 | 2765 | 75.99 | 98.13 | 1.59 |
| 150 | 2100 | 2347 | 75.97 | 97.98 | 1.57 |
| 200 | 2415 | 2673 | 75.97 | 98.08 | 1.59 |
| 200 | 2436 | 2699 | 75.99 | 98.08 | 1.59 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 97 | 5402 | 21.54 | 43.18 | 0.87 |
| 5 | 485 | 5705 | 35.15 | 54.70 | 1.08 |
| 10 | 980 | 6095 | 53.23 | 69.45 | 1.35 |
| 10 | 1010 | 6126 | 53.35 | 69.48 | 1.35 |

