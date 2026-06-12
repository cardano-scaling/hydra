--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-12 14:39:52.05455131 UTC |
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
| 1| 5349 | 9.16 | 3.02 | 0.48 |
| 2| 5445 | 9.14 | 3.00 | 0.49 |
| 3| 5541 | 9.76 | 3.19 | 0.50 |
| 5| 5737 | 11.46 | 3.77 | 0.52 |
| 10| 6216 | 13.09 | 4.26 | 0.56 |
| 50| 10056 | 34.55 | 10.97 | 0.95 |
| 100| 14862 | 61.43 | 19.39 | 1.44 |
| 115| 16299 | 69.72 | 21.97 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2282 | 17.76 | 6.40 | 0.44 |
| 2| 2409 | 18.67 | 7.31 | 0.46 |
| 3| 2541 | 19.67 | 8.24 | 0.48 |
| 5| 2808 | 21.47 | 10.04 | 0.52 |
| 10| 3456 | 26.68 | 14.78 | 0.62 |
| 50| 8703 | 64.96 | 51.31 | 1.43 |
| 75| 11973 | 90.44 | 74.61 | 1.95 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 599 | 16.03 | 5.75 | 0.35 |
| 2| 730 | 16.92 | 6.65 | 0.37 |
| 3| 862 | 17.79 | 7.54 | 0.39 |
| 5| 1124 | 19.62 | 9.35 | 0.43 |
| 10| 1778 | 24.14 | 13.85 | 0.52 |
| 50| 7020 | 62.34 | 50.36 | 1.33 |
| 75| 10295 | 85.30 | 72.91 | 1.82 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 592 | 15.49 | 10.37 | 0.38 |
| 2| 724 | 16.39 | 11.27 | 0.40 |
| 3| 854 | 17.32 | 12.19 | 0.42 |
| 5| 1117 | 19.11 | 13.99 | 0.45 |
| 74| 10158 | 84.45 | 76.97 | 1.83 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 18.79 | 13.48 | 0.43 |
| 2| 750 | 19.87 | 14.44 | 0.45 |
| 3| 886 | 20.92 | 15.38 | 0.47 |
| 5| 1149 | 23.12 | 17.31 | 0.51 |
| 10| 1804 | 28.52 | 22.09 | 0.62 |
| 50| 7045 | 73.45 | 60.68 | 1.49 |
| 72| 9927 | 98.22 | 81.92 | 1.97 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5530 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 283 | 5697 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 570 | 5869 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1140 | 6210 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1137 | 6207 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 987 | 34.31 | 65.19 | 0.94 |
| 25 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 30 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 40 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 50 | 1307 | 1426 | 67.64 | 98.26 | 1.47 |
| 100 | 1312 | 1427 | 67.64 | 98.26 | 1.47 |
| 150 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 200 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1180 | 1675 | 41.40 | 67.74 | 1.05 |
| 25 | 2562 | 2830 | 75.99 | 98.13 | 1.60 |
| 30 | 2121 | 2368 | 75.97 | 97.98 | 1.57 |
| 40 | 2037 | 2280 | 75.99 | 97.98 | 1.57 |
| 50 | 2520 | 2787 | 75.99 | 98.13 | 1.59 |
| 100 | 2352 | 2607 | 75.99 | 98.08 | 1.59 |
| 150 | 2478 | 2743 | 75.99 | 98.08 | 1.59 |
| 200 | 2037 | 2281 | 75.99 | 97.98 | 1.57 |
| 200 | 2331 | 2589 | 75.97 | 98.07 | 1.58 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 118 | 5422 | 21.54 | 43.19 | 0.87 |
| 5 | 610 | 5831 | 35.15 | 54.73 | 1.09 |
| 10 | 1010 | 6126 | 53.23 | 69.45 | 1.35 |
| 10 | 1210 | 6326 | 53.35 | 69.55 | 1.36 |

