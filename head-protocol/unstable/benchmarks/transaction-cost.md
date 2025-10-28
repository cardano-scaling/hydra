--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 04:43:10.571612768 UTC |
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
| 1| 5836 | 10.86 | 3.46 | 0.52 |
| 2| 6037 | 12.32 | 3.89 | 0.54 |
| 3| 6243 | 14.50 | 4.58 | 0.58 |
| 5| 6641 | 18.50 | 5.83 | 0.63 |
| 10| 7648 | 28.94 | 9.11 | 0.79 |
| 43| 14279 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2172 | 12.13 | 7.25 | 0.40 |
| 54| 10053 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 32.19 | 9.36 | 0.51 |
| 3 | 170 | 747 | 43.73 | 12.51 | 0.63 |
| 4 | 226 | 862 | 48.10 | 13.99 | 0.68 |
| 5 | 283 | 969 | 59.20 | 17.01 | 0.79 |
| 6 | 337 | 1085 | 67.75 | 19.45 | 0.89 |
| 7 | 394 | 1192 | 82.87 | 23.52 | 1.04 |
| 8 | 450 | 1307 | 88.78 | 25.27 | 1.11 |
| 9 | 505 | 1414 | 94.43 | 27.14 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 24.00 | 7.62 | 0.48 |
| 2| 1938 | 25.88 | 8.79 | 0.51 |
| 3| 2115 | 28.51 | 10.19 | 0.55 |
| 5| 2275 | 29.27 | 11.74 | 0.57 |
| 10| 3198 | 41.54 | 18.52 | 0.76 |
| 40| 7526 | 96.32 | 53.73 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.81 | 7.37 | 0.42 |
| 2| 766 | 24.25 | 8.44 | 0.44 |
| 3| 924 | 24.99 | 9.29 | 0.46 |
| 5| 1267 | 30.71 | 12.25 | 0.54 |
| 10| 1829 | 35.56 | 16.91 | 0.64 |
| 39| 6228 | 93.74 | 52.42 | 1.56 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 29.17 | 8.91 | 0.48 |
| 2| 812 | 29.26 | 9.62 | 0.49 |
| 3| 998 | 31.50 | 10.93 | 0.53 |
| 5| 1212 | 37.02 | 13.77 | 0.60 |
| 10| 2178 | 46.36 | 19.81 | 0.76 |
| 37| 6024 | 99.23 | 52.59 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 33.83 | 10.15 | 0.53 |
| 2| 893 | 36.56 | 11.60 | 0.57 |
| 3| 1034 | 39.34 | 13.05 | 0.61 |
| 5| 1158 | 41.11 | 14.82 | 0.64 |
| 10| 1901 | 52.67 | 21.38 | 0.81 |
| 29| 4846 | 97.89 | 46.68 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5813 | 27.05 | 9.07 | 0.69 |
| 2| 5937 | 35.88 | 12.06 | 0.79 |
| 3| 6160 | 45.91 | 15.46 | 0.90 |
| 4| 6318 | 55.17 | 18.59 | 1.01 |
| 5| 6479 | 66.46 | 22.44 | 1.13 |
| 6| 6603 | 72.84 | 24.52 | 1.21 |
| 7| 6700 | 80.02 | 26.94 | 1.29 |
| 8| 6849 | 86.01 | 28.91 | 1.35 |
| 9| 6816 | 93.25 | 31.35 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 284 | 6003 | 29.09 | 10.34 | 0.72 |
| 10 | 10 | 569 | 6173 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1136 | 6511 | 59.54 | 22.38 | 1.08 |
| 10 | 39 | 2217 | 7156 | 98.05 | 37.58 | 1.53 |

