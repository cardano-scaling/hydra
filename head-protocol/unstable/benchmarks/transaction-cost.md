--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-24 04:42:31.198804436 UTC |
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
| 1| 5834 | 10.38 | 3.29 | 0.51 |
| 2| 6041 | 12.34 | 3.90 | 0.54 |
| 3| 6239 | 14.40 | 4.55 | 0.57 |
| 5| 6638 | 18.50 | 5.83 | 0.63 |
| 10| 7647 | 28.88 | 9.10 | 0.79 |
| 43| 14282 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10075 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 171 | 747 | 42.25 | 12.15 | 0.61 |
| 4 | 225 | 858 | 52.57 | 15.06 | 0.72 |
| 5 | 281 | 969 | 63.08 | 17.98 | 0.83 |
| 6 | 340 | 1085 | 67.90 | 19.49 | 0.89 |
| 7 | 395 | 1192 | 83.20 | 23.64 | 1.05 |
| 8 | 450 | 1303 | 90.22 | 25.78 | 1.12 |
| 9 | 504 | 1414 | 93.14 | 26.71 | 1.16 |
| 10 | 560 | 1529 | 97.23 | 28.21 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 22.92 | 7.32 | 0.47 |
| 2| 1925 | 25.76 | 8.76 | 0.51 |
| 3| 2056 | 27.32 | 9.86 | 0.53 |
| 5| 2327 | 30.26 | 12.02 | 0.58 |
| 10| 3256 | 43.20 | 18.96 | 0.78 |
| 39| 7580 | 96.79 | 53.24 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.53 | 7.31 | 0.41 |
| 2| 834 | 25.16 | 8.71 | 0.45 |
| 3| 935 | 26.05 | 9.60 | 0.47 |
| 5| 1225 | 28.97 | 11.74 | 0.52 |
| 10| 1942 | 39.23 | 17.96 | 0.68 |
| 40| 6494 | 95.28 | 53.57 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.13 | 8.90 | 0.48 |
| 2| 875 | 31.69 | 10.29 | 0.52 |
| 3| 944 | 30.87 | 10.74 | 0.52 |
| 5| 1127 | 35.52 | 13.32 | 0.58 |
| 10| 1931 | 46.54 | 19.78 | 0.75 |
| 36| 5950 | 98.04 | 51.63 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 33.12 | 9.94 | 0.52 |
| 2| 810 | 35.89 | 11.39 | 0.56 |
| 3| 1006 | 38.55 | 12.81 | 0.60 |
| 5| 1258 | 42.53 | 15.25 | 0.66 |
| 10| 1948 | 52.82 | 21.42 | 0.82 |
| 29| 4877 | 98.69 | 46.88 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5784 | 26.97 | 9.05 | 0.69 |
| 2| 5822 | 31.60 | 10.52 | 0.74 |
| 3| 6076 | 44.97 | 15.08 | 0.89 |
| 4| 6270 | 54.92 | 18.53 | 1.00 |
| 5| 6558 | 66.59 | 22.50 | 1.14 |
| 6| 6431 | 65.68 | 22.05 | 1.12 |
| 7| 6771 | 81.86 | 27.67 | 1.31 |
| 8| 6724 | 90.91 | 30.63 | 1.40 |
| 9| 7001 | 99.49 | 33.59 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6005 | 29.35 | 10.43 | 0.73 |
| 10 | 20 | 1138 | 6512 | 60.42 | 22.68 | 1.09 |
| 10 | 38 | 2162 | 7124 | 97.33 | 37.23 | 1.52 |

