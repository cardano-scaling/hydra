--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-16 04:40:56.881047725 UTC |
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
| 1| 5837 | 10.66 | 3.39 | 0.52 |
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6238 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7644 | 28.92 | 9.11 | 0.79 |
| 43| 14286 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 737 | 3.38 | 1.73 | 0.22 |
| 3| 916 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10074 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 32.31 | 9.40 | 0.51 |
| 3 | 171 | 751 | 41.29 | 11.96 | 0.60 |
| 4 | 227 | 858 | 52.41 | 15.02 | 0.72 |
| 5 | 282 | 969 | 56.75 | 16.49 | 0.77 |
| 6 | 338 | 1081 | 74.19 | 21.12 | 0.95 |
| 7 | 392 | 1192 | 80.72 | 22.96 | 1.02 |
| 8 | 449 | 1307 | 96.81 | 27.30 | 1.19 |
| 9 | 504 | 1414 | 93.82 | 27.05 | 1.17 |
| 10 | 560 | 1525 | 97.37 | 28.25 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1799 | 24.37 | 7.71 | 0.48 |
| 2| 1992 | 27.00 | 9.10 | 0.52 |
| 3| 2129 | 27.94 | 10.05 | 0.54 |
| 5| 2434 | 32.60 | 12.67 | 0.61 |
| 10| 3097 | 40.08 | 18.09 | 0.74 |
| 40| 7655 | 99.84 | 54.73 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 646 | 22.54 | 7.31 | 0.41 |
| 2| 828 | 25.14 | 8.69 | 0.45 |
| 3| 883 | 25.16 | 9.35 | 0.46 |
| 5| 1293 | 30.98 | 12.32 | 0.55 |
| 10| 1749 | 34.59 | 16.65 | 0.63 |
| 41| 6563 | 97.41 | 54.82 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 653 | 29.13 | 8.90 | 0.48 |
| 2| 766 | 28.55 | 9.40 | 0.48 |
| 3| 1047 | 34.03 | 11.63 | 0.56 |
| 5| 1290 | 35.65 | 13.44 | 0.59 |
| 10| 2143 | 46.52 | 19.85 | 0.76 |
| 35| 5967 | 96.59 | 50.61 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 33.83 | 10.15 | 0.53 |
| 2| 832 | 35.85 | 11.38 | 0.56 |
| 3| 952 | 37.84 | 12.60 | 0.59 |
| 5| 1327 | 43.36 | 15.49 | 0.67 |
| 10| 2054 | 54.14 | 21.83 | 0.84 |
| 29| 4916 | 98.43 | 46.83 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5795 | 27.13 | 9.10 | 0.69 |
| 2| 5868 | 34.91 | 11.69 | 0.77 |
| 3| 6154 | 46.16 | 15.55 | 0.91 |
| 4| 6184 | 53.63 | 18.08 | 0.99 |
| 5| 6398 | 63.65 | 21.37 | 1.10 |
| 6| 6451 | 69.08 | 23.19 | 1.16 |
| 7| 6702 | 79.85 | 26.83 | 1.28 |
| 8| 6917 | 96.44 | 32.61 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6003 | 29.98 | 10.65 | 0.73 |
| 10 | 10 | 569 | 6174 | 39.95 | 14.60 | 0.85 |
| 10 | 30 | 1709 | 6856 | 79.78 | 30.37 | 1.32 |
| 10 | 36 | 2051 | 7060 | 93.85 | 35.83 | 1.48 |

