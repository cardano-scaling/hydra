--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-18 12:47:23.308189996 UTC |
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
| 1| 5349 | 8.66 | 2.84 | 0.48 |
| 2| 5448 | 9.48 | 3.12 | 0.49 |
| 3| 5541 | 10.27 | 3.38 | 0.50 |
| 5| 5736 | 11.19 | 3.67 | 0.52 |
| 10| 6216 | 13.17 | 4.28 | 0.56 |
| 50| 10056 | 35.11 | 11.15 | 0.96 |
| 100| 14858 | 61.79 | 19.51 | 1.44 |
| 115| 16296 | 69.64 | 21.96 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2282 | 18.33 | 6.59 | 0.45 |
| 2| 2410 | 19.21 | 7.49 | 0.47 |
| 3| 2540 | 20.28 | 8.45 | 0.49 |
| 5| 2803 | 21.43 | 10.03 | 0.52 |
| 10| 3462 | 26.12 | 14.59 | 0.62 |
| 50| 8699 | 65.24 | 51.40 | 1.43 |
| 75| 11977 | 90.39 | 74.61 | 1.95 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 603 | 16.03 | 5.75 | 0.35 |
| 2| 731 | 16.92 | 6.65 | 0.37 |
| 3| 861 | 17.83 | 7.55 | 0.39 |
| 5| 1123 | 19.62 | 9.35 | 0.43 |
| 10| 1779 | 24.09 | 13.84 | 0.52 |
| 50| 7020 | 61.98 | 50.26 | 1.32 |
| 75| 10294 | 86.28 | 73.19 | 1.83 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 3| 850 | 17.32 | 12.19 | 0.42 |
| 5| 1111 | 19.12 | 13.99 | 0.45 |
| 50| 7013 | 61.95 | 55.14 | 1.36 |
| 75| 10285 | 86.06 | 78.07 | 1.86 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 18.78 | 13.47 | 0.43 |
| 2| 751 | 19.87 | 14.44 | 0.45 |
| 3| 886 | 20.92 | 15.38 | 0.47 |
| 5| 1149 | 23.14 | 17.31 | 0.51 |
| 10| 1803 | 28.50 | 22.08 | 0.62 |
| 50| 7045 | 74.18 | 60.89 | 1.50 |
| 72| 9922 | 99.47 | 82.27 | 1.98 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5530 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 283 | 5698 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 569 | 5869 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1138 | 6207 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1141 | 6210 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 986 | 34.31 | 65.19 | 0.94 |
| 25 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 30 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 40 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 50 | 1307 | 1422 | 67.64 | 98.26 | 1.47 |
| 100 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 150 | 1311 | 1426 | 67.64 | 98.26 | 1.47 |
| 200 | 1307 | 1422 | 67.64 | 98.26 | 1.47 |
| 200 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1060 | 1543 | 41.40 | 67.72 | 1.04 |
| 25 | 2247 | 2500 | 75.97 | 98.03 | 1.58 |
| 30 | 2079 | 2324 | 75.99 | 97.98 | 1.57 |
| 40 | 2562 | 2830 | 75.99 | 98.13 | 1.60 |
| 50 | 2415 | 2673 | 75.99 | 98.08 | 1.59 |
| 100 | 2331 | 2589 | 75.97 | 98.07 | 1.58 |
| 150 | 2184 | 2431 | 75.99 | 98.03 | 1.58 |
| 200 | 2016 | 2255 | 75.99 | 97.98 | 1.57 |
| 200 | 2394 | 2655 | 75.99 | 98.08 | 1.59 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 106 | 5410 | 21.54 | 43.18 | 0.87 |
| 5 | 535 | 5755 | 35.02 | 54.68 | 1.08 |
| 10 | 990 | 6105 | 53.35 | 69.48 | 1.35 |
| 10 | 1220 | 6336 | 53.35 | 69.55 | 1.36 |

