--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-19 13:58:26.566271309 UTC |
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
| 1| 5355 | 9.04 | 2.98 | 0.48 |
| 2| 5447 | 9.14 | 3.00 | 0.49 |
| 3| 5543 | 9.65 | 3.15 | 0.49 |
| 5| 5736 | 11.43 | 3.76 | 0.52 |
| 10| 6217 | 13.59 | 4.43 | 0.57 |
| 50| 10058 | 35.08 | 11.15 | 0.96 |
| 100| 14858 | 61.42 | 19.37 | 1.44 |
| 115| 16298 | 69.71 | 21.97 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2277 | 18.33 | 6.59 | 0.45 |
| 2| 2412 | 19.33 | 7.53 | 0.47 |
| 3| 2540 | 20.10 | 8.39 | 0.48 |
| 5| 2801 | 21.59 | 10.08 | 0.52 |
| 10| 3457 | 26.14 | 14.60 | 0.62 |
| 50| 8702 | 64.53 | 51.16 | 1.42 |
| 75| 11975 | 90.23 | 74.57 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 602 | 16.03 | 5.75 | 0.35 |
| 2| 731 | 16.92 | 6.65 | 0.37 |
| 3| 861 | 17.81 | 7.54 | 0.39 |
| 5| 1123 | 19.59 | 9.34 | 0.43 |
| 10| 1779 | 24.07 | 13.83 | 0.52 |
| 50| 7020 | 62.10 | 50.29 | 1.32 |
| 75| 10299 | 86.69 | 73.30 | 1.83 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 596 | 15.49 | 10.37 | 0.38 |
| 2| 723 | 16.39 | 11.27 | 0.40 |
| 3| 854 | 17.27 | 12.17 | 0.42 |
| 50| 7013 | 62.12 | 55.18 | 1.36 |
| 73| 10026 | 84.50 | 76.34 | 1.82 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 18.79 | 13.48 | 0.43 |
| 2| 754 | 19.89 | 14.44 | 0.45 |
| 3| 886 | 20.92 | 15.38 | 0.47 |
| 5| 1149 | 23.09 | 17.30 | 0.51 |
| 10| 1804 | 28.49 | 22.08 | 0.62 |
| 50| 7041 | 73.53 | 60.70 | 1.49 |
| 72| 9926 | 98.69 | 82.05 | 1.98 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 283 | 5697 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 568 | 5867 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1136 | 6206 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1140 | 6210 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 987 | 34.31 | 65.19 | 0.94 |
| 25 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 30 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 40 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 50 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 100 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 150 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1425 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1110 | 1598 | 41.40 | 67.74 | 1.04 |
| 25 | 2226 | 2478 | 75.99 | 98.03 | 1.58 |
| 30 | 2037 | 2280 | 75.99 | 97.98 | 1.57 |
| 40 | 2037 | 2280 | 75.97 | 97.98 | 1.57 |
| 50 | 2100 | 2347 | 75.97 | 97.98 | 1.57 |
| 100 | 2499 | 2765 | 75.99 | 98.13 | 1.59 |
| 150 | 2415 | 2677 | 75.97 | 98.08 | 1.59 |
| 200 | 2499 | 2765 | 75.99 | 98.13 | 1.59 |
| 200 | 2352 | 2607 | 75.99 | 98.08 | 1.59 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 111 | 5416 | 21.54 | 43.18 | 0.87 |
| 5 | 530 | 5750 | 35.15 | 54.71 | 1.08 |
| 10 | 970 | 6085 | 53.35 | 69.48 | 1.35 |
| 10 | 960 | 6076 | 53.35 | 69.48 | 1.35 |

