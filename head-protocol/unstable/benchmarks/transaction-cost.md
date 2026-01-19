--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-19 11:17:25.236803917 UTC |
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
| 1| 5836 | 10.93 | 3.49 | 0.52 |
| 2| 6035 | 12.23 | 3.86 | 0.54 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6645 | 18.62 | 5.87 | 0.64 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 99.14 | 30.99 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10054 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 43.77 | 12.52 | 0.63 |
| 4 | 226 | 862 | 51.04 | 14.67 | 0.71 |
| 5 | 281 | 969 | 62.31 | 17.73 | 0.82 |
| 6 | 338 | 1081 | 74.80 | 21.14 | 0.95 |
| 7 | 395 | 1192 | 72.51 | 21.03 | 0.94 |
| 9 | 505 | 1414 | 98.31 | 27.95 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 24.00 | 7.62 | 0.48 |
| 2| 1886 | 24.85 | 8.50 | 0.50 |
| 3| 2055 | 27.36 | 9.87 | 0.53 |
| 5| 2526 | 33.10 | 12.83 | 0.62 |
| 10| 3136 | 40.55 | 18.24 | 0.75 |
| 39| 7615 | 98.49 | 53.66 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 22.77 | 7.36 | 0.42 |
| 2| 754 | 23.61 | 8.25 | 0.43 |
| 3| 830 | 24.09 | 9.04 | 0.45 |
| 5| 1186 | 30.00 | 12.05 | 0.53 |
| 10| 2173 | 44.13 | 19.33 | 0.74 |
| 41| 6690 | 98.13 | 55.02 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 693 | 27.51 | 8.47 | 0.46 |
| 2| 869 | 29.90 | 9.82 | 0.50 |
| 3| 917 | 32.79 | 11.25 | 0.54 |
| 5| 1244 | 36.95 | 13.76 | 0.60 |
| 10| 2053 | 44.93 | 19.37 | 0.74 |
| 34| 5910 | 96.83 | 50.02 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 33.79 | 10.15 | 0.53 |
| 2| 803 | 35.92 | 11.40 | 0.56 |
| 3| 1002 | 38.55 | 12.81 | 0.60 |
| 5| 1289 | 42.61 | 15.27 | 0.66 |
| 10| 2007 | 53.83 | 21.75 | 0.83 |
| 29| 4783 | 97.00 | 46.38 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5819 | 27.09 | 9.10 | 0.69 |
| 2| 5845 | 31.40 | 10.44 | 0.74 |
| 3| 6030 | 41.24 | 13.80 | 0.85 |
| 4| 6284 | 55.18 | 18.56 | 1.01 |
| 5| 6464 | 66.13 | 22.29 | 1.13 |
| 6| 6550 | 70.27 | 23.69 | 1.18 |
| 7| 6762 | 84.72 | 28.59 | 1.34 |
| 8| 6864 | 94.19 | 31.82 | 1.44 |
| 9| 6899 | 94.01 | 31.56 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5869 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 282 | 6001 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 568 | 6172 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1137 | 6512 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1706 | 6852 | 79.15 | 30.16 | 1.31 |
| 10 | 39 | 2223 | 7163 | 99.38 | 38.04 | 1.54 |

