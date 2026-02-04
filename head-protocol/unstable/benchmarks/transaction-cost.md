--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-04 15:55:19.037336489 UTC |
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
| 2| 6037 | 12.54 | 3.97 | 0.55 |
| 3| 6238 | 14.52 | 4.59 | 0.58 |
| 5| 6641 | 18.84 | 5.95 | 0.64 |
| 10| 7647 | 29.14 | 9.19 | 0.79 |
| 43| 14279 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2169 | 12.13 | 7.25 | 0.40 |
| 54| 10052 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 32.23 | 9.37 | 0.51 |
| 3 | 170 | 747 | 42.34 | 12.19 | 0.61 |
| 4 | 227 | 858 | 50.36 | 14.48 | 0.70 |
| 5 | 282 | 974 | 64.78 | 18.39 | 0.85 |
| 6 | 338 | 1081 | 64.10 | 18.62 | 0.85 |
| 7 | 394 | 1192 | 72.22 | 20.88 | 0.94 |
| 8 | 451 | 1303 | 87.78 | 25.14 | 1.10 |
| 9 | 504 | 1414 | 95.83 | 27.42 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1799 | 24.00 | 7.62 | 0.48 |
| 2| 1949 | 25.55 | 8.71 | 0.51 |
| 3| 2013 | 25.95 | 9.49 | 0.52 |
| 5| 2401 | 32.60 | 12.67 | 0.61 |
| 10| 3154 | 40.94 | 18.34 | 0.75 |
| 39| 7572 | 98.28 | 53.58 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 609 | 22.84 | 7.37 | 0.41 |
| 2| 798 | 25.16 | 8.72 | 0.45 |
| 3| 873 | 25.05 | 9.31 | 0.46 |
| 5| 1204 | 29.82 | 11.99 | 0.53 |
| 10| 2180 | 43.26 | 19.08 | 0.73 |
| 43| 6615 | 96.29 | 55.81 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.13 | 8.90 | 0.48 |
| 2| 874 | 29.94 | 9.83 | 0.50 |
| 3| 911 | 32.75 | 11.24 | 0.54 |
| 5| 1332 | 38.52 | 14.23 | 0.62 |
| 10| 2029 | 44.94 | 19.38 | 0.74 |
| 36| 6079 | 98.29 | 51.72 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 704 | 33.83 | 10.15 | 0.53 |
| 2| 835 | 35.89 | 11.39 | 0.56 |
| 3| 1009 | 38.63 | 12.83 | 0.60 |
| 5| 1217 | 41.97 | 15.07 | 0.65 |
| 10| 2061 | 54.35 | 21.90 | 0.84 |
| 28| 4984 | 99.35 | 46.48 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5791 | 27.00 | 9.08 | 0.69 |
| 2| 6030 | 36.85 | 12.44 | 0.80 |
| 3| 6146 | 45.72 | 15.42 | 0.90 |
| 4| 6184 | 51.38 | 17.24 | 0.96 |
| 5| 6394 | 60.64 | 20.39 | 1.07 |
| 6| 6435 | 69.05 | 23.13 | 1.16 |
| 7| 6599 | 74.22 | 24.89 | 1.22 |
| 8| 6991 | 95.12 | 32.15 | 1.46 |
| 9| 6762 | 86.89 | 29.17 | 1.36 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.05 | 6.02 | 0.60 |
| 10 | 10 | 570 | 6174 | 39.44 | 14.43 | 0.84 |
| 10 | 20 | 1140 | 6515 | 58.21 | 21.92 | 1.07 |
| 10 | 30 | 1706 | 6852 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2219 | 7158 | 97.61 | 37.43 | 1.52 |

