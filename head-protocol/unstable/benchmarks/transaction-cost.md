--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-26 11:17:27.555315331 UTC |
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
| 1| 5836 | 10.69 | 3.40 | 0.52 |
| 2| 6037 | 12.72 | 4.03 | 0.55 |
| 3| 6239 | 14.29 | 4.51 | 0.57 |
| 5| 6638 | 18.96 | 6.00 | 0.64 |
| 10| 7646 | 29.21 | 9.21 | 0.79 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10042 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 34.30 | 9.88 | 0.53 |
| 3 | 170 | 747 | 40.02 | 11.62 | 0.59 |
| 4 | 225 | 858 | 49.41 | 14.28 | 0.69 |
| 5 | 282 | 969 | 60.68 | 17.34 | 0.81 |
| 6 | 337 | 1081 | 64.75 | 18.78 | 0.86 |
| 7 | 397 | 1192 | 84.72 | 23.92 | 1.06 |
| 8 | 449 | 1303 | 85.36 | 24.51 | 1.07 |
| 9 | 505 | 1414 | 91.37 | 26.35 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1814 | 24.00 | 7.62 | 0.48 |
| 2| 1938 | 25.80 | 8.77 | 0.51 |
| 3| 2118 | 28.31 | 10.14 | 0.55 |
| 5| 2430 | 31.84 | 12.48 | 0.60 |
| 10| 3181 | 41.56 | 18.51 | 0.76 |
| 38| 7293 | 94.04 | 51.77 | 1.60 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 661 | 22.77 | 7.37 | 0.42 |
| 2| 803 | 25.52 | 8.79 | 0.46 |
| 3| 966 | 26.06 | 9.59 | 0.47 |
| 5| 1194 | 30.01 | 12.05 | 0.53 |
| 10| 2019 | 41.14 | 18.47 | 0.70 |
| 40| 6523 | 98.19 | 54.35 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 29.09 | 8.89 | 0.48 |
| 2| 841 | 29.18 | 9.60 | 0.49 |
| 3| 956 | 33.43 | 11.44 | 0.54 |
| 5| 1196 | 36.27 | 13.55 | 0.59 |
| 10| 1988 | 44.11 | 19.13 | 0.73 |
| 37| 6153 | 99.44 | 52.70 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 33.83 | 10.16 | 0.53 |
| 2| 837 | 35.85 | 11.38 | 0.56 |
| 3| 942 | 37.84 | 12.60 | 0.59 |
| 5| 1268 | 42.61 | 15.27 | 0.66 |
| 10| 2029 | 54.13 | 21.83 | 0.83 |
| 29| 4731 | 95.92 | 46.07 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 27.12 | 9.10 | 0.69 |
| 2| 6039 | 36.83 | 12.43 | 0.80 |
| 3| 5989 | 41.38 | 13.83 | 0.85 |
| 4| 6419 | 55.78 | 18.85 | 1.02 |
| 5| 6340 | 63.63 | 21.43 | 1.10 |
| 6| 6505 | 69.69 | 23.42 | 1.17 |
| 7| 6704 | 83.47 | 28.09 | 1.32 |
| 8| 6921 | 93.15 | 31.46 | 1.43 |
| 9| 6930 | 98.83 | 33.21 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 56 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 570 | 6174 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1139 | 6514 | 60.17 | 22.59 | 1.09 |
| 10 | 30 | 1708 | 6854 | 81.37 | 30.91 | 1.33 |
| 10 | 39 | 2218 | 7157 | 99.38 | 38.04 | 1.54 |

