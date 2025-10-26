--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-26 04:40:58.863071335 UTC |
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
| 1| 5837 | 10.36 | 3.28 | 0.51 |
| 2| 6037 | 12.46 | 3.94 | 0.55 |
| 3| 6238 | 15.14 | 4.81 | 0.58 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7644 | 29.11 | 9.17 | 0.79 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2172 | 12.13 | 7.25 | 0.40 |
| 54| 10066 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 41.18 | 11.91 | 0.60 |
| 4 | 227 | 858 | 50.69 | 14.56 | 0.70 |
| 5 | 283 | 969 | 64.28 | 18.26 | 0.84 |
| 6 | 339 | 1081 | 75.15 | 21.26 | 0.96 |
| 7 | 395 | 1192 | 82.76 | 23.45 | 1.04 |
| 8 | 448 | 1303 | 98.26 | 27.50 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1790 | 24.37 | 7.71 | 0.48 |
| 2| 1924 | 25.47 | 8.69 | 0.50 |
| 3| 2059 | 27.02 | 9.79 | 0.53 |
| 5| 2347 | 29.89 | 11.93 | 0.58 |
| 10| 3229 | 41.57 | 18.53 | 0.76 |
| 39| 7570 | 96.99 | 53.24 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 646 | 22.54 | 7.30 | 0.41 |
| 2| 780 | 24.25 | 8.44 | 0.44 |
| 3| 896 | 25.83 | 9.54 | 0.47 |
| 5| 1330 | 32.12 | 12.63 | 0.56 |
| 10| 2044 | 42.74 | 18.91 | 0.72 |
| 41| 6507 | 94.79 | 54.06 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 644 | 29.17 | 8.91 | 0.48 |
| 2| 774 | 30.94 | 10.07 | 0.51 |
| 3| 1043 | 31.57 | 10.95 | 0.53 |
| 5| 1330 | 35.61 | 13.43 | 0.59 |
| 10| 1958 | 44.04 | 19.11 | 0.73 |
| 37| 6055 | 98.02 | 52.29 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 33.83 | 10.15 | 0.53 |
| 2| 764 | 35.14 | 11.16 | 0.55 |
| 3| 938 | 37.91 | 12.62 | 0.59 |
| 5| 1297 | 42.61 | 15.27 | 0.66 |
| 10| 2083 | 55.33 | 22.20 | 0.85 |
| 29| 4707 | 95.58 | 45.95 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5816 | 27.05 | 9.07 | 0.69 |
| 2| 5928 | 35.96 | 12.07 | 0.79 |
| 3| 6063 | 41.44 | 13.87 | 0.85 |
| 4| 6251 | 55.02 | 18.48 | 1.00 |
| 5| 6418 | 61.56 | 20.77 | 1.08 |
| 6| 6577 | 73.81 | 24.93 | 1.22 |
| 7| 6554 | 74.98 | 25.11 | 1.23 |
| 8| 6894 | 91.90 | 30.92 | 1.42 |
| 9| 6853 | 94.02 | 31.55 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 56 | 5867 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6005 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 569 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1138 | 6512 | 60.61 | 22.74 | 1.09 |
| 10 | 30 | 1705 | 6851 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2217 | 7156 | 98.86 | 37.86 | 1.54 |

