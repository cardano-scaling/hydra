--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-24 11:12:25.55075601 UTC |
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
| 1| 5837 | 10.19 | 3.22 | 0.51 |
| 2| 6037 | 12.65 | 4.01 | 0.55 |
| 3| 6239 | 15.07 | 4.78 | 0.58 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7647 | 28.94 | 9.11 | 0.79 |
| 43| 14282 | 98.73 | 30.85 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10054 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 170 | 747 | 42.33 | 12.19 | 0.61 |
| 4 | 227 | 862 | 50.85 | 14.62 | 0.70 |
| 5 | 281 | 969 | 55.74 | 16.15 | 0.76 |
| 6 | 339 | 1081 | 67.80 | 19.50 | 0.89 |
| 7 | 394 | 1192 | 84.41 | 23.96 | 1.06 |
| 8 | 448 | 1303 | 80.69 | 23.39 | 1.03 |
| 9 | 504 | 1414 | 90.94 | 26.24 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1786 | 23.92 | 7.60 | 0.48 |
| 2| 1984 | 26.92 | 9.09 | 0.52 |
| 3| 2017 | 26.31 | 9.58 | 0.52 |
| 5| 2436 | 32.53 | 12.65 | 0.61 |
| 10| 3168 | 42.08 | 18.65 | 0.76 |
| 39| 7565 | 97.07 | 53.28 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 22.80 | 7.36 | 0.42 |
| 2| 788 | 24.32 | 8.46 | 0.44 |
| 3| 881 | 25.16 | 9.34 | 0.46 |
| 5| 1210 | 29.01 | 11.75 | 0.52 |
| 10| 2032 | 41.29 | 18.52 | 0.71 |
| 41| 6614 | 95.03 | 54.15 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 665 | 29.13 | 8.90 | 0.48 |
| 2| 845 | 29.19 | 9.60 | 0.49 |
| 3| 917 | 32.69 | 11.22 | 0.54 |
| 5| 1268 | 34.97 | 13.23 | 0.58 |
| 10| 2007 | 47.43 | 20.05 | 0.77 |
| 38| 6145 | 99.81 | 53.41 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 709 | 33.83 | 10.16 | 0.53 |
| 2| 803 | 35.85 | 11.38 | 0.56 |
| 3| 934 | 37.80 | 12.59 | 0.59 |
| 5| 1349 | 43.43 | 15.51 | 0.67 |
| 10| 2024 | 54.66 | 22.00 | 0.84 |
| 28| 4865 | 97.89 | 46.05 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5785 | 27.16 | 9.10 | 0.69 |
| 2| 5874 | 34.99 | 11.69 | 0.78 |
| 3| 6121 | 45.68 | 15.43 | 0.90 |
| 4| 6239 | 53.95 | 18.11 | 0.99 |
| 5| 6429 | 63.57 | 21.41 | 1.10 |
| 6| 6520 | 69.62 | 23.40 | 1.17 |
| 7| 6741 | 84.28 | 28.42 | 1.33 |
| 8| 6880 | 94.09 | 31.70 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 17.86 | 5.96 | 0.59 |
| 10 | 1 | 57 | 5868 | 19.89 | 6.76 | 0.62 |
| 10 | 5 | 284 | 6003 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1137 | 6512 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1708 | 6854 | 80.92 | 30.76 | 1.33 |
| 10 | 37 | 2106 | 7091 | 95.46 | 36.48 | 1.50 |

