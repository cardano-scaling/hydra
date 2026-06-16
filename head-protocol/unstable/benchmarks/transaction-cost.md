--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-16 10:17:18.324514559 UTC |
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
| 2| 5448 | 9.53 | 3.13 | 0.49 |
| 3| 5544 | 10.04 | 3.30 | 0.50 |
| 5| 5737 | 11.05 | 3.62 | 0.52 |
| 10| 6214 | 13.95 | 4.57 | 0.57 |
| 50| 10059 | 35.27 | 11.22 | 0.96 |
| 100| 14859 | 61.72 | 19.49 | 1.44 |
| 115| 16296 | 69.66 | 21.95 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2276 | 17.76 | 6.40 | 0.44 |
| 2| 2409 | 18.65 | 7.30 | 0.46 |
| 3| 2544 | 20.30 | 8.45 | 0.49 |
| 5| 2803 | 21.45 | 10.03 | 0.52 |
| 10| 3459 | 25.88 | 14.51 | 0.62 |
| 50| 8703 | 65.61 | 51.50 | 1.43 |
| 75| 11973 | 89.56 | 74.35 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 602 | 16.01 | 5.75 | 0.35 |
| 2| 731 | 16.92 | 6.65 | 0.37 |
| 3| 861 | 17.79 | 7.54 | 0.39 |
| 5| 1124 | 19.66 | 9.36 | 0.43 |
| 10| 1780 | 24.16 | 13.86 | 0.52 |
| 50| 7020 | 62.64 | 50.44 | 1.33 |
| 75| 10300 | 86.24 | 73.17 | 1.83 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 592 | 15.49 | 10.37 | 0.38 |
| 2| 724 | 16.39 | 11.27 | 0.40 |
| 3| 855 | 17.32 | 12.19 | 0.42 |
| 5| 1118 | 19.14 | 14.00 | 0.46 |
| 10| 1772 | 23.68 | 18.52 | 0.55 |
| 50| 7009 | 62.44 | 55.27 | 1.36 |
| 74| 10157 | 84.73 | 77.05 | 1.84 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 18.79 | 13.48 | 0.43 |
| 2| 751 | 19.89 | 14.44 | 0.45 |
| 3| 885 | 20.97 | 15.40 | 0.47 |
| 5| 1143 | 23.09 | 17.30 | 0.51 |
| 10| 1799 | 28.40 | 22.05 | 0.62 |
| 50| 7044 | 73.38 | 60.66 | 1.49 |
| 73| 10053 | 99.43 | 82.92 | 1.99 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5700 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 569 | 5868 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1139 | 6209 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1138 | 6208 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 987 | 34.31 | 65.19 | 0.94 |
| 25 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 30 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 40 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 50 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 100 | 1308 | 1423 | 67.64 | 98.26 | 1.47 |
| 150 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 200 | 1308 | 1423 | 67.64 | 98.26 | 1.47 |
| 200 | 1311 | 1426 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 990 | 1466 | 41.40 | 67.69 | 1.04 |
| 25 | 2079 | 2324 | 75.99 | 97.98 | 1.57 |
| 30 | 2331 | 2588 | 75.99 | 98.08 | 1.59 |
| 40 | 2100 | 2346 | 75.97 | 97.98 | 1.57 |
| 50 | 2415 | 2677 | 75.99 | 98.08 | 1.59 |
| 100 | 2205 | 2453 | 75.99 | 98.03 | 1.58 |
| 150 | 2415 | 2677 | 75.99 | 98.08 | 1.59 |
| 200 | 2310 | 2563 | 75.97 | 98.07 | 1.58 |
| 200 | 2562 | 2827 | 75.99 | 98.13 | 1.60 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 99 | 5404 | 21.54 | 43.18 | 0.87 |
| 5 | 495 | 5715 | 35.15 | 54.70 | 1.08 |
| 10 | 1110 | 6226 | 53.35 | 69.52 | 1.36 |
| 10 | 1180 | 6295 | 53.35 | 69.53 | 1.36 |

