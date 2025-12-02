--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-02 16:46:10.601704486 UTC |
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
| 1| 5841 | 11.04 | 3.52 | 0.52 |
| 2| 6035 | 12.67 | 4.01 | 0.55 |
| 3| 6236 | 14.47 | 4.57 | 0.57 |
| 5| 6640 | 18.60 | 5.87 | 0.64 |
| 10| 7646 | 29.57 | 9.34 | 0.79 |
| 43| 14286 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2183 | 12.13 | 7.25 | 0.40 |
| 54| 10066 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 34.20 | 9.84 | 0.53 |
| 3 | 171 | 747 | 39.98 | 11.61 | 0.59 |
| 4 | 227 | 858 | 53.37 | 15.22 | 0.73 |
| 5 | 282 | 969 | 61.31 | 17.58 | 0.81 |
| 6 | 339 | 1081 | 67.67 | 19.43 | 0.88 |
| 7 | 394 | 1192 | 81.36 | 23.20 | 1.03 |
| 8 | 448 | 1303 | 93.71 | 26.46 | 1.16 |
| 9 | 504 | 1414 | 90.81 | 26.15 | 1.13 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 22.92 | 7.32 | 0.47 |
| 2| 1971 | 26.76 | 9.04 | 0.52 |
| 3| 2141 | 29.50 | 10.47 | 0.56 |
| 5| 2275 | 29.19 | 11.72 | 0.57 |
| 10| 3077 | 40.23 | 18.13 | 0.74 |
| 41| 7576 | 95.31 | 54.15 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 636 | 22.81 | 7.37 | 0.42 |
| 2| 739 | 24.31 | 8.47 | 0.44 |
| 3| 1010 | 27.73 | 10.08 | 0.49 |
| 5| 1209 | 29.14 | 11.80 | 0.52 |
| 10| 1897 | 38.40 | 17.72 | 0.67 |
| 39| 6415 | 96.39 | 53.15 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 643 | 29.13 | 8.90 | 0.48 |
| 2| 733 | 30.27 | 9.86 | 0.50 |
| 3| 864 | 32.04 | 11.02 | 0.53 |
| 5| 1391 | 36.99 | 13.85 | 0.61 |
| 10| 2119 | 48.71 | 20.43 | 0.78 |
| 34| 5815 | 93.84 | 49.14 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 633 | 33.15 | 9.95 | 0.52 |
| 2| 815 | 35.89 | 11.39 | 0.56 |
| 3| 942 | 37.84 | 12.60 | 0.59 |
| 5| 1245 | 42.61 | 15.27 | 0.66 |
| 10| 2098 | 54.63 | 21.99 | 0.84 |
| 29| 4900 | 97.96 | 46.70 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5801 | 27.13 | 9.10 | 0.69 |
| 2| 6010 | 37.01 | 12.48 | 0.80 |
| 3| 6205 | 45.70 | 15.41 | 0.90 |
| 4| 6386 | 56.99 | 19.29 | 1.03 |
| 5| 6426 | 64.61 | 21.75 | 1.11 |
| 6| 6530 | 70.83 | 23.79 | 1.18 |
| 7| 6644 | 79.12 | 26.62 | 1.27 |
| 8| 6903 | 90.03 | 30.32 | 1.40 |
| 9| 6911 | 93.71 | 31.48 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 1 | 57 | 5869 | 21.66 | 7.37 | 0.64 |
| 10 | 10 | 570 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6854 | 80.92 | 30.76 | 1.33 |
| 10 | 39 | 2222 | 7161 | 98.05 | 37.58 | 1.53 |

