--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-01 04:46:13.559108996 UTC |
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
| 1| 5837 | 10.55 | 3.35 | 0.52 |
| 2| 6035 | 12.42 | 3.93 | 0.54 |
| 3| 6239 | 14.50 | 4.58 | 0.58 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7646 | 28.94 | 9.11 | 0.79 |
| 43| 14282 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 736 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10069 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 34.35 | 9.90 | 0.53 |
| 3 | 171 | 747 | 41.57 | 12.03 | 0.61 |
| 4 | 226 | 858 | 51.04 | 14.69 | 0.71 |
| 5 | 282 | 969 | 55.87 | 16.21 | 0.76 |
| 6 | 339 | 1081 | 67.66 | 19.39 | 0.88 |
| 7 | 393 | 1192 | 81.01 | 23.15 | 1.02 |
| 8 | 448 | 1307 | 93.85 | 26.49 | 1.16 |
| 9 | 507 | 1414 | 98.80 | 28.18 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1784 | 23.92 | 7.60 | 0.48 |
| 2| 1969 | 25.93 | 8.83 | 0.51 |
| 3| 2082 | 26.94 | 9.77 | 0.53 |
| 5| 2395 | 31.49 | 12.36 | 0.60 |
| 10| 3133 | 40.45 | 18.22 | 0.75 |
| 41| 7682 | 98.06 | 54.91 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 650 | 22.77 | 7.37 | 0.42 |
| 2| 789 | 23.51 | 8.21 | 0.43 |
| 3| 900 | 25.09 | 9.31 | 0.46 |
| 5| 1269 | 31.22 | 12.38 | 0.55 |
| 10| 2092 | 41.96 | 18.71 | 0.72 |
| 40| 6236 | 94.90 | 53.42 | 1.58 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 711 | 27.51 | 8.47 | 0.46 |
| 2| 900 | 29.89 | 9.82 | 0.50 |
| 3| 868 | 32.04 | 11.02 | 0.53 |
| 5| 1241 | 37.05 | 13.78 | 0.60 |
| 10| 1934 | 43.47 | 18.93 | 0.72 |
| 36| 5974 | 97.90 | 51.59 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 33.83 | 10.16 | 0.53 |
| 2| 807 | 35.88 | 11.39 | 0.56 |
| 3| 964 | 37.95 | 12.63 | 0.59 |
| 5| 1399 | 44.03 | 15.70 | 0.68 |
| 10| 1945 | 53.16 | 21.54 | 0.82 |
| 28| 4839 | 97.25 | 45.87 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5831 | 27.00 | 9.07 | 0.69 |
| 2| 5864 | 32.68 | 10.91 | 0.75 |
| 3| 6050 | 45.18 | 15.19 | 0.89 |
| 4| 6283 | 55.20 | 18.59 | 1.01 |
| 5| 6374 | 63.92 | 21.54 | 1.10 |
| 6| 6564 | 74.24 | 24.97 | 1.22 |
| 7| 6814 | 82.21 | 27.80 | 1.31 |
| 8| 6734 | 84.35 | 28.24 | 1.33 |
| 9| 6688 | 86.33 | 28.92 | 1.35 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.85 | 7.43 | 0.64 |
| 10 | 20 | 1138 | 6513 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1708 | 6854 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2220 | 7159 | 99.38 | 38.04 | 1.54 |

