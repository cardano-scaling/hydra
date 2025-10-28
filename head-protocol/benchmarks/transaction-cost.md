--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 14:33:02.823467709 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νInitial | c8a101a5c8ac4816b0dceb59ce31fc2258e387de828f02961d2f2045 | 2652 | 
| νCommit | 61458bc2f297fff3cc5df6ac7ab57cefd87763b0b7bd722146a1035c | 685 | 
| νHead | a1442faf26d4ec409e2f62a685c1d4893f8d6bcbaf7bcb59d6fa1340 | 14599 | 
| μHead | fd173b993e12103cd734ca6710d364e17120a5eb37a224c64ab2b188* | 5284 | 
| νDeposit | ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c | 1102 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5834 | 10.95 | 3.49 | 0.52 |
| 2| 6038 | 12.34 | 3.90 | 0.54 |
| 3| 6236 | 14.52 | 4.59 | 0.58 |
| 5| 6640 | 18.62 | 5.87 | 0.64 |
| 10| 7647 | 29.49 | 9.31 | 0.79 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2167 | 12.13 | 7.25 | 0.40 |
| 54| 10068 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 170 | 747 | 42.68 | 12.30 | 0.62 |
| 4 | 227 | 858 | 49.74 | 14.38 | 0.69 |
| 5 | 281 | 969 | 55.92 | 16.20 | 0.76 |
| 6 | 340 | 1085 | 73.07 | 20.69 | 0.94 |
| 7 | 396 | 1192 | 73.18 | 21.28 | 0.95 |
| 8 | 449 | 1303 | 82.51 | 23.78 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1809 | 23.92 | 7.60 | 0.48 |
| 2| 1924 | 25.84 | 8.78 | 0.51 |
| 3| 2179 | 29.30 | 10.42 | 0.56 |
| 5| 2329 | 29.93 | 11.94 | 0.58 |
| 10| 3153 | 40.85 | 18.31 | 0.75 |
| 40| 7637 | 98.54 | 54.36 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 657 | 22.81 | 7.38 | 0.42 |
| 2| 780 | 23.59 | 8.23 | 0.43 |
| 3| 973 | 26.14 | 9.61 | 0.47 |
| 5| 1134 | 28.07 | 11.49 | 0.51 |
| 10| 1957 | 37.95 | 17.60 | 0.67 |
| 41| 6557 | 95.16 | 54.18 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 29.17 | 8.91 | 0.48 |
| 2| 778 | 30.98 | 10.08 | 0.51 |
| 3| 1001 | 31.50 | 10.93 | 0.53 |
| 5| 1390 | 36.35 | 13.65 | 0.60 |
| 10| 1907 | 46.21 | 19.66 | 0.75 |
| 35| 5801 | 94.92 | 50.12 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 679 | 33.87 | 10.16 | 0.53 |
| 2| 875 | 36.52 | 11.59 | 0.57 |
| 3| 1058 | 39.38 | 13.06 | 0.61 |
| 5| 1251 | 42.56 | 15.26 | 0.66 |
| 10| 1956 | 53.49 | 21.63 | 0.82 |
| 29| 5009 | 99.89 | 47.29 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5780 | 27.16 | 9.10 | 0.69 |
| 2| 5874 | 32.49 | 10.86 | 0.75 |
| 3| 6147 | 45.92 | 15.49 | 0.90 |
| 4| 6312 | 52.33 | 17.66 | 0.98 |
| 5| 6388 | 62.78 | 21.08 | 1.09 |
| 6| 6368 | 64.80 | 21.64 | 1.11 |
| 7| 6504 | 75.56 | 25.40 | 1.23 |
| 8| 6846 | 91.20 | 30.72 | 1.41 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 284 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 569 | 6174 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1137 | 6511 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1707 | 6854 | 80.04 | 30.46 | 1.32 |
| 10 | 40 | 2278 | 7195 | 99.22 | 38.09 | 1.54 |
| 10 | 39 | 2219 | 7158 | 99.31 | 38.01 | 1.54 |

