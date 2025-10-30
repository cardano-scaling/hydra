--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-30 04:48:54.331340445 UTC |
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
| 1| 5838 | 10.38 | 3.29 | 0.51 |
| 2| 6037 | 12.65 | 4.01 | 0.55 |
| 3| 6238 | 14.40 | 4.55 | 0.57 |
| 5| 6641 | 18.50 | 5.83 | 0.63 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10046 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 43.81 | 12.53 | 0.63 |
| 4 | 228 | 858 | 52.40 | 15.04 | 0.72 |
| 5 | 283 | 969 | 55.18 | 16.02 | 0.75 |
| 6 | 338 | 1081 | 75.09 | 21.25 | 0.96 |
| 7 | 394 | 1196 | 82.72 | 23.48 | 1.04 |
| 8 | 449 | 1303 | 88.10 | 25.26 | 1.10 |
| 9 | 504 | 1418 | 94.99 | 27.39 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1746 | 23.30 | 7.41 | 0.47 |
| 2| 1992 | 26.46 | 8.97 | 0.52 |
| 3| 2115 | 28.02 | 10.07 | 0.54 |
| 5| 2408 | 32.33 | 12.60 | 0.61 |
| 10| 3225 | 42.81 | 18.87 | 0.77 |
| 39| 7580 | 99.76 | 54.03 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.81 | 7.37 | 0.42 |
| 2| 762 | 23.61 | 8.24 | 0.43 |
| 3| 874 | 25.58 | 9.50 | 0.46 |
| 5| 1265 | 30.85 | 12.29 | 0.54 |
| 10| 2102 | 41.65 | 18.64 | 0.71 |
| 41| 6602 | 95.26 | 54.23 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 657 | 29.13 | 8.90 | 0.48 |
| 2| 875 | 29.90 | 9.82 | 0.50 |
| 3| 984 | 33.47 | 11.45 | 0.55 |
| 5| 1265 | 37.36 | 13.88 | 0.61 |
| 10| 1932 | 47.17 | 19.95 | 0.76 |
| 38| 6152 | 99.08 | 53.21 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 33.87 | 10.16 | 0.53 |
| 2| 807 | 35.89 | 11.39 | 0.56 |
| 3| 1007 | 38.59 | 12.82 | 0.60 |
| 5| 1278 | 42.72 | 15.30 | 0.66 |
| 10| 2065 | 54.46 | 21.94 | 0.84 |
| 30| 4841 | 97.92 | 47.27 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5811 | 27.12 | 9.10 | 0.69 |
| 2| 5976 | 35.99 | 12.09 | 0.79 |
| 3| 6116 | 44.75 | 15.04 | 0.89 |
| 4| 6218 | 53.67 | 18.09 | 0.99 |
| 5| 6411 | 63.60 | 21.39 | 1.10 |
| 6| 6569 | 73.32 | 24.69 | 1.21 |
| 7| 6805 | 84.50 | 28.52 | 1.34 |
| 8| 6873 | 92.03 | 31.02 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 1 | 57 | 5869 | 19.89 | 6.76 | 0.62 |
| 10 | 10 | 570 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 30 | 1707 | 6853 | 80.48 | 30.61 | 1.32 |
| 10 | 38 | 2164 | 7126 | 96.88 | 37.08 | 1.51 |

