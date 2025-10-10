--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-10 09:54:53.1854539 UTC |
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
| 1| 5837 | 10.17 | 3.22 | 0.51 |
| 2| 6037 | 12.67 | 4.01 | 0.55 |
| 3| 6236 | 14.67 | 4.64 | 0.58 |
| 5| 6638 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 28.88 | 9.10 | 0.79 |
| 43| 14282 | 99.23 | 31.02 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10073 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 640 | 33.25 | 9.62 | 0.52 |
| 3 | 170 | 747 | 40.31 | 11.73 | 0.59 |
| 4 | 228 | 858 | 49.67 | 14.34 | 0.69 |
| 5 | 283 | 969 | 58.19 | 16.81 | 0.78 |
| 6 | 338 | 1081 | 72.54 | 20.56 | 0.93 |
| 7 | 395 | 1192 | 72.97 | 21.19 | 0.95 |
| 8 | 451 | 1303 | 83.11 | 24.02 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1801 | 23.92 | 7.60 | 0.48 |
| 2| 1944 | 25.92 | 8.80 | 0.51 |
| 3| 2066 | 26.90 | 9.76 | 0.53 |
| 5| 2504 | 33.64 | 12.96 | 0.63 |
| 10| 3095 | 39.49 | 17.94 | 0.73 |
| 41| 7705 | 99.45 | 55.27 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 636 | 22.77 | 7.36 | 0.42 |
| 2| 739 | 23.58 | 8.22 | 0.43 |
| 3| 938 | 26.14 | 9.61 | 0.47 |
| 5| 1217 | 29.12 | 11.78 | 0.52 |
| 10| 1985 | 37.81 | 17.55 | 0.67 |
| 40| 6587 | 98.08 | 54.30 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 692 | 27.47 | 8.46 | 0.46 |
| 2| 812 | 29.22 | 9.61 | 0.49 |
| 3| 1001 | 31.69 | 10.98 | 0.53 |
| 5| 1208 | 34.26 | 13.01 | 0.57 |
| 10| 2011 | 48.04 | 20.23 | 0.77 |
| 37| 6127 | 99.28 | 52.66 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 678 | 33.79 | 10.15 | 0.53 |
| 2| 819 | 35.85 | 11.38 | 0.56 |
| 3| 1001 | 38.66 | 12.84 | 0.60 |
| 5| 1200 | 41.86 | 15.04 | 0.65 |
| 10| 2079 | 54.70 | 22.01 | 0.84 |
| 29| 4757 | 97.33 | 46.48 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 26.96 | 9.05 | 0.69 |
| 2| 5845 | 31.52 | 10.48 | 0.74 |
| 3| 6061 | 45.13 | 15.18 | 0.89 |
| 4| 6296 | 54.80 | 18.47 | 1.00 |
| 5| 6286 | 59.82 | 20.03 | 1.05 |
| 6| 6625 | 70.07 | 23.58 | 1.18 |
| 7| 6740 | 83.64 | 28.21 | 1.33 |
| 8| 6923 | 94.02 | 31.72 | 1.44 |
| 9| 6950 | 98.05 | 32.93 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 58 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 284 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 30 | 1707 | 6853 | 81.55 | 30.98 | 1.33 |
| 10 | 38 | 2163 | 7125 | 96.44 | 36.92 | 1.51 |

