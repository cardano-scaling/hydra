--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-10 04:52:26.346930255 UTC |
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
| 2| 6035 | 12.54 | 3.97 | 0.55 |
| 3| 6243 | 14.52 | 4.59 | 0.58 |
| 5| 6641 | 18.93 | 5.98 | 0.64 |
| 10| 7644 | 29.09 | 9.17 | 0.79 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10050 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.19 | 9.36 | 0.51 |
| 3 | 171 | 747 | 39.86 | 11.58 | 0.59 |
| 4 | 226 | 858 | 52.33 | 14.95 | 0.72 |
| 5 | 283 | 974 | 56.45 | 16.36 | 0.77 |
| 6 | 339 | 1081 | 75.53 | 21.32 | 0.96 |
| 7 | 393 | 1192 | 86.31 | 24.33 | 1.08 |
| 8 | 453 | 1307 | 91.60 | 25.95 | 1.13 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1746 | 23.30 | 7.41 | 0.47 |
| 2| 1991 | 26.87 | 9.07 | 0.52 |
| 3| 2167 | 29.54 | 10.48 | 0.56 |
| 5| 2275 | 28.97 | 11.67 | 0.57 |
| 10| 3197 | 41.66 | 18.55 | 0.76 |
| 43| 7841 | 98.03 | 56.21 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 602 | 22.57 | 7.32 | 0.41 |
| 2| 757 | 23.55 | 8.22 | 0.43 |
| 3| 931 | 26.08 | 9.61 | 0.47 |
| 5| 1092 | 26.96 | 11.19 | 0.50 |
| 10| 1958 | 37.70 | 17.51 | 0.67 |
| 43| 6908 | 99.98 | 56.86 | 1.67 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 27.50 | 8.46 | 0.46 |
| 2| 797 | 30.98 | 10.08 | 0.51 |
| 3| 914 | 32.72 | 11.23 | 0.54 |
| 5| 1224 | 37.06 | 13.78 | 0.60 |
| 10| 2095 | 45.60 | 19.58 | 0.75 |
| 35| 5591 | 99.11 | 51.15 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 697 | 33.83 | 10.15 | 0.53 |
| 2| 863 | 36.56 | 11.60 | 0.57 |
| 3| 1033 | 38.59 | 12.82 | 0.60 |
| 5| 1391 | 43.84 | 15.65 | 0.68 |
| 10| 1952 | 53.34 | 21.59 | 0.82 |
| 29| 4723 | 95.93 | 46.04 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5832 | 26.92 | 9.05 | 0.69 |
| 2| 6011 | 36.89 | 12.42 | 0.80 |
| 3| 6040 | 42.54 | 14.26 | 0.86 |
| 4| 6142 | 50.10 | 16.80 | 0.95 |
| 5| 6383 | 63.97 | 21.53 | 1.10 |
| 6| 6667 | 75.10 | 25.38 | 1.23 |
| 7| 6851 | 85.26 | 28.88 | 1.35 |
| 8| 7048 | 93.75 | 31.68 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 20 | 1139 | 6514 | 58.66 | 22.07 | 1.07 |
| 10 | 40 | 2277 | 7193 | 99.22 | 38.09 | 1.54 |

