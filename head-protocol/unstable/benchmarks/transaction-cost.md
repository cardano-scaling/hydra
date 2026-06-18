--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-18 09:54:43.500398237 UTC |
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
| 1| 5351 | 9.07 | 2.98 | 0.48 |
| 2| 5445 | 9.35 | 3.07 | 0.49 |
| 3| 5544 | 10.08 | 3.31 | 0.50 |
| 5| 5741 | 11.24 | 3.69 | 0.52 |
| 10| 6216 | 13.19 | 4.28 | 0.56 |
| 50| 10058 | 34.49 | 10.95 | 0.95 |
| 100| 14859 | 61.43 | 19.37 | 1.44 |
| 115| 16298 | 69.65 | 21.97 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2278 | 17.76 | 6.40 | 0.44 |
| 2| 2407 | 19.23 | 7.50 | 0.47 |
| 3| 2541 | 19.62 | 8.22 | 0.48 |
| 5| 2804 | 21.61 | 10.09 | 0.52 |
| 10| 3458 | 26.81 | 14.82 | 0.63 |
| 50| 8699 | 65.59 | 51.53 | 1.43 |
| 75| 11974 | 90.49 | 74.62 | 1.95 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 16.01 | 5.75 | 0.35 |
| 2| 730 | 16.90 | 6.64 | 0.37 |
| 3| 862 | 17.83 | 7.55 | 0.39 |
| 5| 1124 | 19.59 | 9.34 | 0.43 |
| 10| 1778 | 24.17 | 13.86 | 0.52 |
| 50| 7020 | 62.34 | 50.36 | 1.33 |
| 75| 10299 | 86.65 | 73.29 | 1.83 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 588 | 15.47 | 10.37 | 0.38 |
| 2| 724 | 16.37 | 11.27 | 0.40 |
| 3| 854 | 17.32 | 12.19 | 0.42 |
| 5| 1122 | 19.16 | 14.00 | 0.46 |
| 10| 1768 | 23.68 | 18.52 | 0.55 |
| 50| 7014 | 61.69 | 55.06 | 1.35 |
| 75| 10288 | 86.14 | 78.09 | 1.86 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 18.78 | 13.47 | 0.43 |
| 2| 755 | 19.85 | 14.43 | 0.45 |
| 3| 885 | 20.92 | 15.38 | 0.47 |
| 5| 1153 | 23.12 | 17.31 | 0.51 |
| 10| 1804 | 28.40 | 22.05 | 0.62 |
| 50| 7044 | 73.58 | 60.72 | 1.49 |
| 73| 10058 | 99.85 | 83.03 | 2.00 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5530 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5699 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 569 | 5869 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1138 | 6207 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1139 | 6209 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 986 | 34.31 | 65.19 | 0.94 |
| 25 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 30 | 1308 | 1423 | 67.64 | 98.26 | 1.47 |
| 40 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 50 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 100 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 150 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 200 | 1312 | 1427 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1090 | 1576 | 41.38 | 67.71 | 1.04 |
| 25 | 2436 | 2698 | 75.99 | 98.08 | 1.59 |
| 30 | 2268 | 2518 | 75.99 | 98.03 | 1.58 |
| 40 | 2373 | 2632 | 75.99 | 98.08 | 1.59 |
| 50 | 2520 | 2787 | 75.99 | 98.13 | 1.59 |
| 100 | 2499 | 2765 | 75.97 | 98.12 | 1.59 |
| 150 | 2058 | 2303 | 75.99 | 97.98 | 1.57 |
| 200 | 2436 | 2699 | 75.97 | 98.08 | 1.59 |
| 200 | 2562 | 2827 | 75.99 | 98.13 | 1.60 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 109 | 5413 | 21.42 | 43.15 | 0.87 |
| 5 | 580 | 5801 | 35.15 | 54.72 | 1.08 |
| 10 | 1080 | 6195 | 53.35 | 69.50 | 1.35 |
| 10 | 1090 | 6206 | 53.35 | 69.50 | 1.36 |

