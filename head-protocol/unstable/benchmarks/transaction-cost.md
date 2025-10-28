--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 15:43:48.272611752 UTC |
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
| 1| 5836 | 10.61 | 3.37 | 0.52 |
| 2| 6037 | 12.42 | 3.93 | 0.54 |
| 3| 6243 | 14.60 | 4.62 | 0.58 |
| 5| 6638 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 28.92 | 9.11 | 0.79 |
| 43| 14286 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10065 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 528 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.18 | 9.60 | 0.52 |
| 3 | 170 | 747 | 40.20 | 11.68 | 0.59 |
| 4 | 226 | 862 | 53.57 | 15.25 | 0.73 |
| 5 | 284 | 974 | 64.27 | 18.26 | 0.84 |
| 6 | 339 | 1081 | 66.77 | 19.34 | 0.88 |
| 7 | 394 | 1192 | 82.71 | 23.48 | 1.04 |
| 8 | 448 | 1303 | 87.75 | 25.14 | 1.10 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1824 | 24.00 | 7.62 | 0.48 |
| 2| 1934 | 25.92 | 8.80 | 0.51 |
| 3| 2130 | 27.94 | 10.05 | 0.54 |
| 5| 2403 | 32.36 | 12.61 | 0.61 |
| 10| 3085 | 40.01 | 18.09 | 0.74 |
| 39| 7552 | 98.09 | 53.53 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 662 | 22.54 | 7.31 | 0.41 |
| 2| 739 | 23.61 | 8.23 | 0.43 |
| 3| 946 | 26.95 | 9.86 | 0.48 |
| 5| 1285 | 30.07 | 12.04 | 0.54 |
| 10| 2061 | 39.70 | 18.07 | 0.69 |
| 40| 6506 | 96.25 | 53.81 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 27.50 | 8.46 | 0.46 |
| 2| 737 | 30.27 | 9.86 | 0.50 |
| 3| 959 | 30.98 | 10.76 | 0.52 |
| 5| 1283 | 37.54 | 13.95 | 0.61 |
| 10| 2062 | 48.30 | 20.29 | 0.78 |
| 35| 5827 | 99.14 | 51.19 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 696 | 33.87 | 10.16 | 0.53 |
| 2| 764 | 35.21 | 11.18 | 0.55 |
| 3| 996 | 38.55 | 12.81 | 0.60 |
| 5| 1335 | 43.25 | 15.47 | 0.67 |
| 10| 2153 | 55.43 | 22.24 | 0.85 |
| 29| 4879 | 97.93 | 46.67 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5801 | 27.13 | 9.10 | 0.69 |
| 2| 5954 | 35.96 | 12.08 | 0.79 |
| 3| 6117 | 45.73 | 15.44 | 0.90 |
| 4| 6195 | 50.14 | 16.81 | 0.95 |
| 5| 6448 | 62.94 | 21.24 | 1.10 |
| 6| 6525 | 70.05 | 23.57 | 1.17 |
| 7| 6643 | 81.43 | 27.35 | 1.30 |
| 8| 6762 | 88.43 | 29.70 | 1.38 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 285 | 6005 | 29.09 | 10.34 | 0.72 |
| 10 | 10 | 570 | 6175 | 38.18 | 14.00 | 0.83 |
| 10 | 39 | 2221 | 7160 | 97.61 | 37.43 | 1.52 |

