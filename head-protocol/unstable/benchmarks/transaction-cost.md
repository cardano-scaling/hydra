--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-12 16:27:21.354151038 UTC |
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
| 1| 5349 | 8.83 | 2.90 | 0.48 |
| 2| 5445 | 9.34 | 3.07 | 0.49 |
| 3| 5543 | 10.02 | 3.29 | 0.50 |
| 5| 5736 | 11.02 | 3.61 | 0.52 |
| 10| 6216 | 13.50 | 4.40 | 0.56 |
| 50| 10058 | 34.90 | 11.10 | 0.95 |
| 100| 14858 | 61.66 | 19.47 | 1.44 |
| 115| 16298 | 69.49 | 21.89 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2278 | 17.76 | 6.40 | 0.44 |
| 2| 2409 | 18.65 | 7.30 | 0.46 |
| 3| 2541 | 19.67 | 8.24 | 0.48 |
| 5| 2801 | 22.08 | 10.25 | 0.53 |
| 10| 3457 | 26.81 | 14.82 | 0.63 |
| 50| 8699 | 65.78 | 51.59 | 1.44 |
| 75| 11975 | 90.95 | 74.79 | 1.95 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 599 | 16.01 | 5.75 | 0.35 |
| 2| 729 | 16.90 | 6.64 | 0.37 |
| 3| 861 | 17.84 | 7.56 | 0.39 |
| 5| 1124 | 19.62 | 9.35 | 0.43 |
| 10| 1779 | 24.17 | 13.86 | 0.52 |
| 50| 7023 | 62.46 | 50.39 | 1.33 |
| 75| 10299 | 85.66 | 73.01 | 1.82 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 2| 719 | 16.39 | 11.27 | 0.40 |
| 3| 854 | 17.29 | 12.18 | 0.42 |
| 5| 1117 | 19.07 | 13.98 | 0.45 |
| 10| 1772 | 23.75 | 18.54 | 0.55 |
| 50| 7007 | 62.16 | 55.19 | 1.36 |
| 75| 10289 | 86.28 | 78.13 | 1.86 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 18.79 | 13.48 | 0.43 |
| 2| 754 | 19.89 | 14.44 | 0.45 |
| 3| 881 | 20.97 | 15.40 | 0.47 |
| 5| 1149 | 23.07 | 17.29 | 0.51 |
| 10| 1803 | 28.52 | 22.09 | 0.62 |
| 50| 7045 | 74.16 | 60.88 | 1.50 |
| 73| 10057 | 99.91 | 83.05 | 2.00 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5699 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 569 | 5869 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1140 | 6210 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1138 | 6208 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 982 | 34.31 | 65.19 | 0.94 |
| 25 | 1307 | 1426 | 67.64 | 98.26 | 1.47 |
| 30 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 40 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 50 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 100 | 1306 | 1425 | 67.64 | 98.26 | 1.47 |
| 150 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 200 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1000 | 1473 | 41.38 | 67.69 | 1.04 |
| 25 | 1995 | 2236 | 75.97 | 97.97 | 1.57 |
| 30 | 1953 | 2192 | 75.97 | 97.97 | 1.57 |
| 40 | 2310 | 2566 | 75.99 | 98.08 | 1.58 |
| 50 | 2373 | 2633 | 75.97 | 98.08 | 1.59 |
| 100 | 2583 | 2853 | 75.97 | 98.13 | 1.60 |
| 150 | 2121 | 2369 | 75.99 | 97.98 | 1.57 |
| 200 | 1995 | 2237 | 75.99 | 97.98 | 1.57 |
| 200 | 2079 | 2325 | 75.99 | 97.98 | 1.57 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 100 | 5404 | 21.42 | 43.15 | 0.87 |
| 5 | 475 | 5695 | 35.02 | 54.66 | 1.08 |
| 10 | 930 | 6045 | 53.23 | 69.44 | 1.35 |
| 10 | 1120 | 6235 | 53.35 | 69.52 | 1.36 |

