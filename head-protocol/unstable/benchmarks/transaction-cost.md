--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-19 14:27:47.132907533 UTC |
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
| 2| 6035 | 12.46 | 3.94 | 0.54 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6638 | 18.62 | 5.87 | 0.64 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14282 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10043 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.31 | 9.40 | 0.51 |
| 3 | 170 | 747 | 39.85 | 11.58 | 0.59 |
| 4 | 227 | 858 | 52.44 | 15.00 | 0.72 |
| 5 | 281 | 969 | 58.23 | 16.82 | 0.78 |
| 6 | 338 | 1081 | 75.41 | 21.29 | 0.96 |
| 7 | 395 | 1192 | 72.36 | 21.04 | 0.94 |
| 8 | 451 | 1303 | 82.82 | 23.90 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 22.93 | 7.32 | 0.47 |
| 2| 1952 | 25.92 | 8.80 | 0.51 |
| 3| 2203 | 29.47 | 10.46 | 0.56 |
| 5| 2422 | 31.80 | 12.47 | 0.60 |
| 10| 3115 | 40.75 | 18.29 | 0.75 |
| 39| 7487 | 96.07 | 53.00 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 616 | 22.53 | 7.32 | 0.41 |
| 2| 744 | 24.35 | 8.48 | 0.44 |
| 3| 872 | 25.47 | 9.45 | 0.46 |
| 5| 1137 | 28.60 | 11.67 | 0.52 |
| 10| 2037 | 41.39 | 18.57 | 0.71 |
| 42| 6572 | 95.78 | 54.99 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 28.42 | 8.68 | 0.47 |
| 2| 831 | 29.15 | 9.59 | 0.49 |
| 3| 868 | 32.01 | 11.01 | 0.53 |
| 5| 1244 | 35.12 | 13.27 | 0.58 |
| 10| 2003 | 47.14 | 19.97 | 0.76 |
| 35| 5750 | 99.43 | 51.23 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.16 | 0.53 |
| 2| 765 | 35.14 | 11.16 | 0.55 |
| 3| 936 | 37.91 | 12.62 | 0.59 |
| 5| 1315 | 43.25 | 15.47 | 0.67 |
| 10| 2058 | 53.83 | 21.75 | 0.83 |
| 29| 4919 | 98.00 | 46.71 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.93 | 7.56 | 0.64 |
| 2| 5881 | 32.48 | 10.86 | 0.75 |
| 3| 5897 | 36.89 | 12.23 | 0.80 |
| 4| 6144 | 50.63 | 16.95 | 0.95 |
| 5| 6334 | 59.64 | 20.01 | 1.06 |
| 6| 6449 | 68.43 | 22.93 | 1.15 |
| 7| 6600 | 76.78 | 25.80 | 1.25 |
| 8| 6911 | 93.91 | 31.64 | 1.44 |
| 9| 6969 | 99.04 | 33.33 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 10 | 569 | 6173 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1136 | 6510 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1706 | 6852 | 80.92 | 30.76 | 1.33 |
| 10 | 39 | 2218 | 7157 | 98.93 | 37.88 | 1.54 |

