--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-12 14:58:52.656614692 UTC |
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
| 1| 5351 | 8.67 | 2.84 | 0.48 |
| 2| 5447 | 9.14 | 3.00 | 0.49 |
| 3| 5543 | 9.84 | 3.23 | 0.50 |
| 5| 5736 | 11.20 | 3.68 | 0.52 |
| 10| 6214 | 13.18 | 4.28 | 0.56 |
| 50| 10056 | 34.40 | 10.93 | 0.95 |
| 100| 14856 | 61.69 | 19.46 | 1.44 |
| 115| 16298 | 69.80 | 22.01 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2276 | 18.33 | 6.59 | 0.45 |
| 2| 2409 | 19.30 | 7.52 | 0.47 |
| 3| 2540 | 19.67 | 8.24 | 0.48 |
| 5| 2804 | 21.95 | 10.20 | 0.52 |
| 10| 3458 | 26.12 | 14.59 | 0.62 |
| 50| 8699 | 65.68 | 51.52 | 1.44 |
| 75| 11975 | 90.78 | 74.72 | 1.95 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 16.03 | 5.75 | 0.35 |
| 2| 729 | 16.92 | 6.65 | 0.37 |
| 3| 861 | 17.81 | 7.54 | 0.39 |
| 5| 1125 | 19.57 | 9.33 | 0.43 |
| 10| 1778 | 24.17 | 13.86 | 0.52 |
| 50| 7020 | 61.84 | 50.22 | 1.32 |
| 75| 10295 | 85.98 | 73.10 | 1.82 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 592 | 15.47 | 10.37 | 0.38 |
| 3| 854 | 17.29 | 12.18 | 0.42 |
| 5| 1117 | 19.14 | 14.00 | 0.46 |
| 10| 1773 | 23.68 | 18.52 | 0.55 |
| 75| 10289 | 86.26 | 78.13 | 1.86 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 620 | 18.78 | 13.47 | 0.43 |
| 2| 754 | 19.89 | 14.44 | 0.45 |
| 3| 886 | 20.97 | 15.40 | 0.47 |
| 5| 1149 | 23.10 | 17.30 | 0.51 |
| 10| 1799 | 28.56 | 22.10 | 0.62 |
| 50| 7045 | 73.64 | 60.74 | 1.49 |
| 73| 10057 | 99.83 | 83.03 | 2.00 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5564 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5699 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 570 | 5869 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1140 | 6210 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1137 | 6206 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 986 | 34.31 | 65.19 | 0.94 |
| 25 | 1310 | 1425 | 67.64 | 98.26 | 1.47 |
| 30 | 1307 | 1426 | 67.64 | 98.26 | 1.47 |
| 40 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 50 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 100 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 150 | 1307 | 1426 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 200 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1060 | 1543 | 41.38 | 67.71 | 1.04 |
| 25 | 2079 | 2320 | 75.97 | 97.98 | 1.57 |
| 30 | 2163 | 2412 | 75.97 | 98.02 | 1.58 |
| 40 | 2247 | 2500 | 75.99 | 98.03 | 1.58 |
| 50 | 2247 | 2501 | 75.99 | 98.03 | 1.58 |
| 100 | 2289 | 2545 | 75.99 | 98.03 | 1.58 |
| 150 | 1953 | 2193 | 75.99 | 97.98 | 1.57 |
| 200 | 1974 | 2215 | 75.99 | 97.98 | 1.57 |
| 200 | 2079 | 2325 | 75.97 | 97.98 | 1.57 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 106 | 5410 | 21.42 | 43.15 | 0.87 |
| 5 | 495 | 5715 | 35.02 | 54.66 | 1.08 |
| 10 | 1030 | 6145 | 53.23 | 69.47 | 1.35 |
| 10 | 1160 | 6276 | 53.23 | 69.49 | 1.36 |

