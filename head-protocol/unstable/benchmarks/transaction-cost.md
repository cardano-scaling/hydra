--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-27 17:22:28.239701475 UTC |
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
| 1| 5836 | 10.85 | 3.45 | 0.52 |
| 2| 6038 | 13.01 | 4.14 | 0.55 |
| 3| 6242 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 19.00 | 6.01 | 0.64 |
| 10| 7647 | 28.88 | 9.10 | 0.79 |
| 43| 14281 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2172 | 12.13 | 7.25 | 0.40 |
| 54| 10071 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.30 | 9.88 | 0.53 |
| 3 | 169 | 747 | 40.08 | 11.67 | 0.59 |
| 4 | 226 | 858 | 48.03 | 13.95 | 0.68 |
| 5 | 282 | 969 | 55.61 | 16.21 | 0.76 |
| 6 | 338 | 1081 | 74.19 | 21.11 | 0.95 |
| 7 | 394 | 1196 | 82.22 | 23.27 | 1.03 |
| 8 | 450 | 1307 | 87.41 | 25.05 | 1.09 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1814 | 24.37 | 7.71 | 0.48 |
| 2| 1984 | 26.92 | 9.09 | 0.52 |
| 3| 2013 | 25.87 | 9.47 | 0.52 |
| 5| 2405 | 31.98 | 12.52 | 0.60 |
| 10| 3222 | 42.53 | 18.78 | 0.77 |
| 39| 7504 | 97.09 | 53.29 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 22.81 | 7.37 | 0.42 |
| 2| 775 | 24.05 | 8.39 | 0.44 |
| 3| 939 | 26.90 | 9.85 | 0.48 |
| 5| 1157 | 27.93 | 11.45 | 0.51 |
| 10| 1949 | 39.19 | 17.97 | 0.68 |
| 42| 6695 | 99.93 | 56.16 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 643 | 29.17 | 8.91 | 0.48 |
| 2| 839 | 29.22 | 9.61 | 0.49 |
| 3| 873 | 32.05 | 11.02 | 0.53 |
| 5| 1215 | 34.41 | 13.05 | 0.58 |
| 10| 1954 | 47.25 | 20.00 | 0.76 |
| 37| 6163 | 99.92 | 52.82 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 703 | 33.83 | 10.15 | 0.53 |
| 2| 903 | 36.60 | 11.61 | 0.57 |
| 3| 1009 | 38.59 | 12.82 | 0.60 |
| 5| 1251 | 42.65 | 15.28 | 0.66 |
| 10| 1945 | 53.49 | 21.63 | 0.82 |
| 29| 4913 | 98.82 | 46.93 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.93 | 7.56 | 0.64 |
| 2| 5972 | 37.16 | 12.51 | 0.80 |
| 3| 6180 | 45.71 | 15.44 | 0.90 |
| 4| 6237 | 55.01 | 18.55 | 1.00 |
| 5| 6145 | 52.78 | 17.61 | 0.97 |
| 6| 6511 | 70.12 | 23.60 | 1.17 |
| 7| 6787 | 79.62 | 26.87 | 1.29 |
| 8| 6966 | 95.22 | 32.16 | 1.46 |
| 10| 7027 | 99.65 | 33.46 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 19.45 | 6.61 | 0.61 |
| 10 | 5 | 285 | 6004 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 569 | 6174 | 37.74 | 13.85 | 0.83 |
| 10 | 20 | 1137 | 6511 | 58.66 | 22.07 | 1.07 |
| 10 | 37 | 2107 | 7092 | 95.28 | 36.42 | 1.49 |

