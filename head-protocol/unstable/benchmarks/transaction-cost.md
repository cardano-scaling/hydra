--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-15 09:00:41.398543104 UTC |
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
| 1| 5351 | 9.04 | 2.98 | 0.48 |
| 2| 5447 | 9.22 | 3.03 | 0.49 |
| 3| 5544 | 10.04 | 3.30 | 0.50 |
| 5| 5737 | 10.64 | 3.47 | 0.51 |
| 10| 6214 | 13.45 | 4.38 | 0.56 |
| 50| 10059 | 34.66 | 11.00 | 0.95 |
| 100| 14858 | 61.75 | 19.49 | 1.44 |
| 115| 16299 | 69.85 | 22.02 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2277 | 18.33 | 6.59 | 0.45 |
| 2| 2408 | 18.85 | 7.36 | 0.46 |
| 3| 2540 | 20.12 | 8.39 | 0.49 |
| 5| 2804 | 21.96 | 10.21 | 0.52 |
| 10| 3458 | 26.77 | 14.80 | 0.63 |
| 50| 8703 | 64.77 | 51.26 | 1.43 |
| 75| 11974 | 89.84 | 74.47 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 16.01 | 5.75 | 0.35 |
| 2| 729 | 16.94 | 6.65 | 0.37 |
| 3| 861 | 17.84 | 7.56 | 0.39 |
| 5| 1124 | 19.64 | 9.35 | 0.43 |
| 10| 1779 | 24.16 | 13.86 | 0.52 |
| 50| 7020 | 62.48 | 50.39 | 1.33 |
| 75| 10295 | 85.62 | 73.00 | 1.82 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 593 | 15.49 | 10.37 | 0.38 |
| 2| 723 | 16.41 | 11.28 | 0.40 |
| 3| 855 | 17.27 | 12.17 | 0.42 |
| 5| 1117 | 19.11 | 13.99 | 0.45 |
| 50| 7013 | 61.72 | 55.07 | 1.35 |
| 75| 10288 | 85.40 | 77.89 | 1.85 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 18.79 | 13.48 | 0.43 |
| 2| 754 | 19.89 | 14.44 | 0.45 |
| 3| 885 | 20.96 | 15.39 | 0.47 |
| 5| 1145 | 23.14 | 17.31 | 0.51 |
| 10| 1804 | 28.40 | 22.05 | 0.62 |
| 50| 7045 | 73.62 | 60.73 | 1.49 |
| 73| 10053 | 99.27 | 82.87 | 1.99 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5699 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 570 | 5869 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1138 | 6208 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1138 | 6207 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 986 | 34.31 | 65.19 | 0.94 |
| 25 | 1309 | 1424 | 67.64 | 98.26 | 1.47 |
| 30 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 40 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 50 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 100 | 1311 | 1426 | 67.64 | 98.26 | 1.47 |
| 150 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1425 | 67.64 | 98.26 | 1.47 |
| 200 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1050 | 1532 | 41.38 | 67.71 | 1.04 |
| 25 | 2373 | 2628 | 75.97 | 98.08 | 1.59 |
| 30 | 2184 | 2434 | 75.99 | 98.03 | 1.58 |
| 40 | 2541 | 2808 | 75.97 | 98.13 | 1.60 |
| 50 | 2058 | 2303 | 75.99 | 97.98 | 1.57 |
| 100 | 2142 | 2387 | 75.99 | 98.03 | 1.58 |
| 150 | 2247 | 2501 | 75.99 | 98.03 | 1.58 |
| 200 | 2373 | 2629 | 75.99 | 98.08 | 1.59 |
| 200 | 2310 | 2567 | 75.99 | 98.08 | 1.58 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 105 | 5410 | 21.42 | 43.15 | 0.87 |
| 5 | 565 | 5785 | 35.02 | 54.69 | 1.08 |
| 10 | 1040 | 6155 | 53.35 | 69.50 | 1.35 |
| 10 | 1080 | 6196 | 53.35 | 69.50 | 1.36 |

