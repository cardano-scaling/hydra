--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-06 04:51:12.922025347 UTC |
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
| 1| 5834 | 10.40 | 3.30 | 0.51 |
| 2| 6041 | 12.25 | 3.87 | 0.54 |
| 3| 6236 | 14.72 | 4.66 | 0.58 |
| 5| 6638 | 18.72 | 5.91 | 0.64 |
| 10| 7646 | 29.38 | 9.27 | 0.79 |
| 43| 14282 | 99.23 | 31.02 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10053 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 32.23 | 9.37 | 0.51 |
| 3 | 170 | 747 | 41.07 | 11.87 | 0.60 |
| 4 | 226 | 858 | 47.62 | 13.85 | 0.67 |
| 5 | 283 | 969 | 61.37 | 17.57 | 0.82 |
| 6 | 340 | 1081 | 65.96 | 19.10 | 0.87 |
| 7 | 396 | 1192 | 84.72 | 24.00 | 1.06 |
| 8 | 449 | 1303 | 93.25 | 26.60 | 1.15 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 24.00 | 7.62 | 0.48 |
| 2| 1882 | 24.40 | 8.39 | 0.49 |
| 3| 2058 | 27.02 | 9.79 | 0.53 |
| 5| 2341 | 30.34 | 12.04 | 0.59 |
| 10| 3120 | 41.01 | 18.35 | 0.75 |
| 38| 7299 | 93.80 | 51.67 | 1.60 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 620 | 22.84 | 7.39 | 0.42 |
| 2| 775 | 24.32 | 8.46 | 0.44 |
| 3| 915 | 25.72 | 9.52 | 0.47 |
| 5| 1336 | 32.27 | 12.67 | 0.56 |
| 10| 2123 | 40.75 | 18.36 | 0.71 |
| 42| 6625 | 96.31 | 55.15 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 27.50 | 8.46 | 0.46 |
| 2| 802 | 30.87 | 10.05 | 0.51 |
| 3| 948 | 30.82 | 10.73 | 0.52 |
| 5| 1312 | 37.67 | 13.98 | 0.61 |
| 10| 2095 | 45.73 | 19.61 | 0.75 |
| 37| 6004 | 97.88 | 52.20 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 33.87 | 10.16 | 0.53 |
| 2| 826 | 35.85 | 11.38 | 0.56 |
| 3| 1000 | 38.66 | 12.84 | 0.60 |
| 5| 1326 | 44.03 | 15.70 | 0.68 |
| 10| 2053 | 54.47 | 21.93 | 0.84 |
| 29| 4851 | 97.47 | 46.56 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5833 | 26.96 | 9.06 | 0.69 |
| 2| 5897 | 32.57 | 10.87 | 0.75 |
| 3| 6075 | 44.97 | 15.09 | 0.89 |
| 4| 6135 | 50.49 | 16.89 | 0.95 |
| 5| 6427 | 60.93 | 20.60 | 1.07 |
| 6| 6465 | 72.38 | 24.38 | 1.20 |
| 7| 6787 | 84.13 | 28.47 | 1.33 |
| 8| 7006 | 97.66 | 33.16 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.82 | 6.63 | 0.61 |
| 10 | 5 | 283 | 6002 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1136 | 6510 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1706 | 6852 | 80.48 | 30.61 | 1.32 |
| 10 | 38 | 2157 | 7119 | 95.56 | 36.62 | 1.50 |

