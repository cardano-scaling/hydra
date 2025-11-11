--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-11 04:50:18.228050607 UTC |
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
| 1| 5837 | 10.38 | 3.29 | 0.51 |
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6239 | 14.29 | 4.51 | 0.57 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.66 | 30.82 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2170 | 12.13 | 7.25 | 0.40 |
| 54| 10069 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 170 | 747 | 42.45 | 12.24 | 0.61 |
| 4 | 225 | 858 | 52.34 | 14.96 | 0.72 |
| 5 | 282 | 974 | 64.28 | 18.23 | 0.84 |
| 6 | 341 | 1081 | 64.26 | 18.62 | 0.85 |
| 7 | 396 | 1192 | 76.81 | 22.15 | 0.98 |
| 8 | 450 | 1307 | 98.61 | 27.69 | 1.20 |
| 9 | 505 | 1414 | 98.50 | 28.16 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1811 | 24.37 | 7.71 | 0.48 |
| 2| 1988 | 26.79 | 9.05 | 0.52 |
| 3| 2097 | 27.86 | 10.03 | 0.54 |
| 5| 2400 | 30.85 | 12.20 | 0.59 |
| 10| 3155 | 40.77 | 18.29 | 0.75 |
| 38| 7118 | 91.86 | 51.16 | 1.57 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 600 | 22.57 | 7.31 | 0.41 |
| 2| 826 | 25.14 | 8.70 | 0.45 |
| 3| 830 | 24.06 | 9.04 | 0.45 |
| 5| 1194 | 30.12 | 12.06 | 0.53 |
| 10| 1973 | 39.78 | 18.12 | 0.69 |
| 39| 6386 | 96.82 | 53.24 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 652 | 29.17 | 8.91 | 0.48 |
| 2| 832 | 29.22 | 9.61 | 0.49 |
| 3| 980 | 33.44 | 11.45 | 0.55 |
| 5| 1323 | 35.65 | 13.44 | 0.59 |
| 10| 1981 | 47.48 | 20.05 | 0.77 |
| 36| 5845 | 96.68 | 51.20 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.15 | 0.53 |
| 2| 807 | 35.88 | 11.39 | 0.56 |
| 3| 981 | 38.59 | 12.82 | 0.60 |
| 5| 1328 | 43.40 | 15.50 | 0.67 |
| 10| 2061 | 54.66 | 22.00 | 0.84 |
| 29| 4786 | 96.92 | 46.36 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5789 | 27.13 | 9.09 | 0.69 |
| 2| 6002 | 37.05 | 12.50 | 0.80 |
| 3| 6062 | 44.96 | 15.13 | 0.89 |
| 4| 6302 | 52.02 | 17.54 | 0.97 |
| 5| 6316 | 59.32 | 19.88 | 1.05 |
| 6| 6724 | 77.21 | 26.15 | 1.26 |
| 7| 6673 | 80.56 | 27.13 | 1.29 |
| 8| 6799 | 92.01 | 30.89 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5869 | 20.96 | 7.13 | 0.63 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1140 | 6515 | 59.98 | 22.53 | 1.08 |
| 10 | 39 | 2222 | 7161 | 98.49 | 37.73 | 1.53 |

