--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-17 13:22:35.905356063 UTC |
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
| 1| 5356 | 8.94 | 2.94 | 0.48 |
| 2| 5447 | 9.16 | 3.00 | 0.49 |
| 3| 5544 | 10.04 | 3.30 | 0.50 |
| 5| 5736 | 11.07 | 3.62 | 0.52 |
| 10| 6217 | 13.50 | 4.40 | 0.56 |
| 50| 10056 | 34.74 | 11.04 | 0.95 |
| 100| 14859 | 61.44 | 19.37 | 1.44 |
| 115| 16299 | 69.79 | 22.01 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2278 | 17.76 | 6.40 | 0.44 |
| 2| 2411 | 19.33 | 7.53 | 0.47 |
| 3| 2540 | 19.62 | 8.22 | 0.48 |
| 5| 2803 | 21.57 | 10.07 | 0.52 |
| 10| 3458 | 26.53 | 14.73 | 0.62 |
| 50| 8698 | 65.74 | 51.56 | 1.44 |
| 75| 11974 | 89.47 | 74.36 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 16.01 | 5.75 | 0.35 |
| 2| 733 | 16.94 | 6.65 | 0.37 |
| 3| 865 | 17.84 | 7.56 | 0.39 |
| 5| 1123 | 19.57 | 9.33 | 0.43 |
| 10| 1783 | 24.05 | 13.83 | 0.52 |
| 50| 7020 | 62.00 | 50.26 | 1.32 |
| 75| 10295 | 85.96 | 73.10 | 1.82 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 596 | 15.47 | 10.37 | 0.38 |
| 2| 720 | 16.37 | 11.27 | 0.40 |
| 5| 1117 | 19.07 | 13.98 | 0.45 |
| 10| 1768 | 23.57 | 18.49 | 0.55 |
| 50| 7009 | 62.28 | 55.22 | 1.36 |
| 74| 10153 | 84.81 | 77.07 | 1.84 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 18.78 | 13.47 | 0.43 |
| 2| 751 | 19.85 | 14.43 | 0.45 |
| 3| 886 | 20.97 | 15.40 | 0.47 |
| 5| 1149 | 23.14 | 17.31 | 0.51 |
| 10| 1803 | 28.43 | 22.06 | 0.62 |
| 50| 7044 | 74.16 | 60.88 | 1.50 |
| 73| 10051 | 99.53 | 82.94 | 1.99 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5700 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 569 | 5868 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1138 | 6208 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1139 | 6209 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 986 | 34.31 | 65.19 | 0.94 |
| 25 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 30 | 1309 | 1424 | 67.64 | 98.26 | 1.47 |
| 40 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 50 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 100 | 1311 | 1426 | 67.64 | 98.26 | 1.47 |
| 150 | 1306 | 1421 | 67.64 | 98.26 | 1.47 |
| 200 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1030 | 1510 | 41.40 | 67.71 | 1.04 |
| 25 | 2373 | 2632 | 75.99 | 98.08 | 1.59 |
| 30 | 2289 | 2540 | 75.99 | 98.03 | 1.58 |
| 40 | 2352 | 2610 | 75.99 | 98.08 | 1.59 |
| 50 | 2520 | 2787 | 75.99 | 98.13 | 1.59 |
| 100 | 2394 | 2651 | 75.99 | 98.08 | 1.59 |
| 150 | 2037 | 2277 | 75.99 | 97.98 | 1.57 |
| 200 | 2079 | 2325 | 75.97 | 97.98 | 1.57 |
| 200 | 2541 | 2809 | 75.99 | 98.13 | 1.60 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 103 | 5408 | 21.54 | 43.18 | 0.87 |
| 5 | 565 | 5786 | 35.15 | 54.72 | 1.08 |
| 10 | 1090 | 6205 | 53.35 | 69.50 | 1.36 |
| 10 | 1110 | 6225 | 53.35 | 69.52 | 1.36 |

