--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-16 13:22:41.108030155 UTC |
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
| 1| 5351 | 9.45 | 3.13 | 0.48 |
| 2| 5448 | 9.23 | 3.03 | 0.49 |
| 3| 5541 | 10.21 | 3.36 | 0.50 |
| 5| 5734 | 10.83 | 3.54 | 0.52 |
| 10| 6214 | 13.66 | 4.46 | 0.57 |
| 50| 10058 | 35.07 | 11.14 | 0.96 |
| 100| 14863 | 61.50 | 19.40 | 1.44 |
| 115| 16302 | 69.66 | 21.95 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2277 | 18.33 | 6.59 | 0.45 |
| 2| 2410 | 19.33 | 7.53 | 0.47 |
| 3| 2540 | 20.22 | 8.42 | 0.49 |
| 5| 2802 | 22.26 | 10.31 | 0.53 |
| 10| 3458 | 25.91 | 14.52 | 0.62 |
| 50| 8696 | 65.97 | 51.61 | 1.44 |
| 75| 11973 | 90.37 | 74.58 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 599 | 16.03 | 5.75 | 0.35 |
| 2| 731 | 16.94 | 6.65 | 0.37 |
| 3| 861 | 17.83 | 7.55 | 0.39 |
| 5| 1124 | 19.60 | 9.34 | 0.43 |
| 10| 1782 | 24.09 | 13.84 | 0.52 |
| 50| 7024 | 62.52 | 50.41 | 1.33 |
| 75| 10295 | 86.42 | 73.22 | 1.83 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 588 | 15.47 | 10.37 | 0.38 |
| 2| 723 | 16.37 | 11.27 | 0.40 |
| 10| 1773 | 23.75 | 18.54 | 0.55 |
| 75| 10289 | 85.44 | 77.90 | 1.85 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 18.78 | 13.47 | 0.43 |
| 2| 750 | 19.89 | 14.44 | 0.45 |
| 3| 885 | 20.92 | 15.38 | 0.47 |
| 5| 1148 | 23.05 | 17.29 | 0.51 |
| 10| 1803 | 28.43 | 22.06 | 0.62 |
| 50| 7044 | 74.08 | 60.86 | 1.50 |
| 72| 9930 | 99.53 | 82.29 | 1.98 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5530 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5564 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5699 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 568 | 5867 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1139 | 6208 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1138 | 6208 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 987 | 34.31 | 65.19 | 0.94 |
| 25 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 30 | 1306 | 1425 | 67.64 | 98.26 | 1.47 |
| 40 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 50 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 100 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 150 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 200 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1080 | 1565 | 41.38 | 67.71 | 1.04 |
| 25 | 2205 | 2456 | 75.97 | 98.03 | 1.58 |
| 30 | 2310 | 2566 | 75.99 | 98.08 | 1.58 |
| 40 | 1953 | 2192 | 75.97 | 97.97 | 1.57 |
| 50 | 2373 | 2633 | 75.99 | 98.08 | 1.59 |
| 100 | 2520 | 2787 | 75.99 | 98.13 | 1.59 |
| 150 | 2058 | 2303 | 75.97 | 97.98 | 1.57 |
| 200 | 2079 | 2325 | 75.99 | 97.98 | 1.57 |
| 200 | 2436 | 2699 | 75.99 | 98.08 | 1.59 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 108 | 5412 | 21.42 | 43.15 | 0.87 |
| 5 | 525 | 5746 | 35.02 | 54.68 | 1.08 |
| 10 | 1100 | 6215 | 53.35 | 69.52 | 1.36 |
| 10 | 1100 | 6215 | 53.23 | 69.49 | 1.35 |

