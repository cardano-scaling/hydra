--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-09 09:01:16.321665722 UTC |
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
| 1| 5836 | 10.59 | 3.36 | 0.52 |
| 2| 6037 | 12.67 | 4.01 | 0.55 |
| 3| 6239 | 14.76 | 4.67 | 0.58 |
| 5| 6640 | 18.84 | 5.95 | 0.64 |
| 10| 7644 | 28.80 | 9.07 | 0.78 |
| 43| 14282 | 98.95 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10072 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 168 | 751 | 43.99 | 12.61 | 0.63 |
| 4 | 227 | 858 | 48.15 | 13.97 | 0.68 |
| 5 | 282 | 974 | 60.68 | 17.33 | 0.81 |
| 6 | 338 | 1085 | 75.66 | 21.39 | 0.96 |
| 7 | 396 | 1192 | 72.82 | 21.15 | 0.94 |
| 8 | 449 | 1303 | 96.27 | 27.07 | 1.18 |
| 9 | 507 | 1414 | 94.09 | 27.00 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1783 | 23.92 | 7.60 | 0.48 |
| 2| 1880 | 24.47 | 8.41 | 0.49 |
| 3| 2015 | 26.32 | 9.58 | 0.52 |
| 5| 2367 | 31.42 | 12.34 | 0.60 |
| 10| 3223 | 42.34 | 18.72 | 0.77 |
| 38| 7377 | 94.15 | 51.81 | 1.61 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 603 | 22.84 | 7.38 | 0.41 |
| 2| 755 | 24.31 | 8.45 | 0.44 |
| 3| 954 | 26.71 | 9.79 | 0.48 |
| 5| 1233 | 29.89 | 12.01 | 0.53 |
| 10| 2051 | 40.67 | 18.36 | 0.70 |
| 42| 6801 | 99.14 | 55.97 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.13 | 8.90 | 0.48 |
| 2| 871 | 29.90 | 9.82 | 0.50 |
| 3| 914 | 32.68 | 11.22 | 0.54 |
| 5| 1281 | 35.01 | 13.24 | 0.58 |
| 10| 2090 | 48.91 | 20.49 | 0.79 |
| 35| 6025 | 97.71 | 50.93 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 33.87 | 10.16 | 0.53 |
| 2| 824 | 35.88 | 11.39 | 0.56 |
| 3| 895 | 37.20 | 12.40 | 0.58 |
| 5| 1196 | 41.86 | 15.04 | 0.65 |
| 10| 1971 | 53.54 | 21.64 | 0.83 |
| 29| 4753 | 96.53 | 46.28 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 27.00 | 9.07 | 0.69 |
| 2| 5988 | 35.96 | 12.08 | 0.79 |
| 3| 6136 | 45.92 | 15.50 | 0.90 |
| 4| 6214 | 54.17 | 18.19 | 0.99 |
| 5| 6407 | 63.63 | 21.45 | 1.10 |
| 6| 6548 | 73.53 | 24.72 | 1.21 |
| 7| 6742 | 79.83 | 26.92 | 1.29 |
| 8| 6939 | 90.43 | 30.52 | 1.41 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6003 | 29.53 | 10.50 | 0.73 |
| 10 | 10 | 568 | 6173 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6512 | 59.28 | 22.29 | 1.08 |
| 10 | 30 | 1706 | 6853 | 78.90 | 30.07 | 1.31 |
| 10 | 39 | 2215 | 7154 | 98.93 | 37.88 | 1.54 |

