--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-15 09:17:59.494266499 UTC |
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
| 1| 5349 | 8.71 | 2.86 | 0.48 |
| 2| 5445 | 9.16 | 3.00 | 0.49 |
| 3| 5543 | 9.68 | 3.17 | 0.50 |
| 5| 5737 | 10.63 | 3.47 | 0.51 |
| 10| 6216 | 13.15 | 4.27 | 0.56 |
| 50| 10059 | 34.53 | 10.96 | 0.95 |
| 100| 14858 | 61.43 | 19.38 | 1.44 |
| 115| 16299 | 69.77 | 22.00 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2282 | 17.84 | 6.43 | 0.44 |
| 2| 2409 | 18.65 | 7.30 | 0.46 |
| 3| 2540 | 19.67 | 8.24 | 0.48 |
| 5| 2804 | 21.55 | 10.07 | 0.52 |
| 10| 3458 | 26.67 | 14.77 | 0.62 |
| 50| 8703 | 65.76 | 51.57 | 1.44 |
| 75| 11971 | 89.87 | 74.47 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 16.01 | 5.75 | 0.35 |
| 2| 730 | 16.94 | 6.65 | 0.37 |
| 3| 860 | 17.83 | 7.55 | 0.39 |
| 5| 1129 | 19.57 | 9.33 | 0.43 |
| 10| 1783 | 24.19 | 13.87 | 0.52 |
| 50| 7024 | 61.70 | 50.18 | 1.32 |
| 75| 10295 | 85.46 | 72.96 | 1.82 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 588 | 15.49 | 10.37 | 0.38 |
| 2| 723 | 16.41 | 11.28 | 0.40 |
| 3| 854 | 17.29 | 12.18 | 0.42 |
| 10| 1772 | 23.71 | 18.53 | 0.55 |
| 75| 10288 | 85.88 | 78.02 | 1.86 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 18.79 | 13.48 | 0.43 |
| 2| 750 | 19.89 | 14.44 | 0.45 |
| 3| 886 | 20.97 | 15.40 | 0.47 |
| 5| 1149 | 23.09 | 17.30 | 0.51 |
| 10| 1804 | 28.49 | 22.08 | 0.62 |
| 50| 7045 | 73.90 | 60.81 | 1.50 |
| 72| 9926 | 99.13 | 82.18 | 1.98 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 284 | 5698 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 569 | 5868 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1139 | 6208 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1138 | 6208 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 987 | 34.31 | 65.19 | 0.94 |
| 25 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 30 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 40 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 50 | 1306 | 1421 | 67.64 | 98.26 | 1.47 |
| 100 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 150 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 200 | 1307 | 1426 | 67.64 | 98.26 | 1.47 |
| 200 | 1306 | 1425 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1020 | 1499 | 41.40 | 67.71 | 1.04 |
| 25 | 2205 | 2456 | 75.99 | 98.03 | 1.58 |
| 30 | 2499 | 2764 | 75.99 | 98.13 | 1.59 |
| 40 | 2289 | 2544 | 75.99 | 98.03 | 1.58 |
| 50 | 2226 | 2475 | 75.97 | 98.03 | 1.58 |
| 100 | 2205 | 2457 | 75.99 | 98.03 | 1.58 |
| 150 | 2247 | 2501 | 75.99 | 98.03 | 1.58 |
| 200 | 2310 | 2567 | 75.99 | 98.08 | 1.58 |
| 200 | 2331 | 2589 | 75.99 | 98.08 | 1.59 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 102 | 5406 | 21.54 | 43.18 | 0.87 |
| 5 | 525 | 5745 | 35.15 | 54.71 | 1.08 |
| 10 | 1190 | 6305 | 53.35 | 69.55 | 1.36 |
| 10 | 1040 | 6155 | 53.35 | 69.50 | 1.35 |

