--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-28 04:50:50.359959571 UTC |
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
| 1| 5840 | 11.02 | 3.52 | 0.52 |
| 2| 6041 | 12.41 | 3.92 | 0.54 |
| 3| 6239 | 15.07 | 4.78 | 0.58 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7647 | 28.94 | 9.11 | 0.79 |
| 43| 14286 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 34.19 | 9.84 | 0.53 |
| 3 | 171 | 747 | 42.53 | 12.22 | 0.61 |
| 4 | 225 | 858 | 52.37 | 14.96 | 0.72 |
| 5 | 282 | 969 | 61.55 | 17.61 | 0.82 |
| 6 | 340 | 1081 | 73.19 | 20.79 | 0.94 |
| 7 | 395 | 1196 | 79.90 | 22.80 | 1.01 |
| 8 | 451 | 1303 | 96.13 | 27.14 | 1.18 |
| 9 | 506 | 1414 | 90.90 | 26.18 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1809 | 24.37 | 7.71 | 0.48 |
| 2| 1976 | 26.54 | 9.00 | 0.52 |
| 3| 2080 | 27.39 | 9.88 | 0.53 |
| 5| 2388 | 31.42 | 12.34 | 0.60 |
| 10| 3116 | 40.55 | 18.24 | 0.75 |
| 38| 7512 | 96.89 | 52.59 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 603 | 22.84 | 7.37 | 0.41 |
| 2| 830 | 25.06 | 8.67 | 0.45 |
| 3| 1030 | 28.04 | 10.17 | 0.50 |
| 5| 1228 | 30.13 | 12.08 | 0.53 |
| 10| 1961 | 38.58 | 17.75 | 0.68 |
| 43| 6769 | 99.42 | 56.68 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 651 | 29.17 | 8.91 | 0.48 |
| 2| 790 | 30.87 | 10.05 | 0.51 |
| 3| 944 | 30.87 | 10.74 | 0.52 |
| 5| 1422 | 36.51 | 13.69 | 0.61 |
| 10| 1891 | 46.01 | 19.61 | 0.75 |
| 36| 6055 | 98.67 | 51.83 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 33.87 | 10.16 | 0.53 |
| 2| 833 | 35.88 | 11.39 | 0.56 |
| 3| 988 | 38.47 | 12.79 | 0.60 |
| 5| 1257 | 42.49 | 15.24 | 0.66 |
| 10| 2017 | 54.54 | 21.95 | 0.84 |
| 29| 4923 | 98.53 | 46.86 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5800 | 27.09 | 9.09 | 0.69 |
| 2| 6011 | 37.01 | 12.46 | 0.80 |
| 3| 6204 | 45.41 | 15.34 | 0.90 |
| 4| 6186 | 50.06 | 16.77 | 0.95 |
| 5| 6423 | 60.45 | 20.34 | 1.07 |
| 6| 6462 | 68.96 | 23.11 | 1.16 |
| 7| 6649 | 79.50 | 26.78 | 1.28 |
| 8| 6928 | 90.87 | 30.60 | 1.41 |
| 9| 6835 | 89.70 | 30.00 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 30 | 1705 | 6851 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2219 | 7158 | 99.38 | 38.04 | 1.54 |

