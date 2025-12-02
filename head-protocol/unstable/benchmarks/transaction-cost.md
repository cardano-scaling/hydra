--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-02 11:15:08.722997511 UTC |
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
| 1| 5838 | 10.35 | 3.28 | 0.51 |
| 2| 6038 | 12.78 | 4.06 | 0.55 |
| 3| 6239 | 14.78 | 4.68 | 0.58 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7644 | 29.12 | 9.18 | 0.79 |
| 43| 14282 | 99.02 | 30.95 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10068 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.62 | 0.52 |
| 3 | 170 | 751 | 42.34 | 12.17 | 0.61 |
| 4 | 227 | 858 | 51.13 | 14.72 | 0.71 |
| 5 | 283 | 969 | 62.92 | 17.94 | 0.83 |
| 6 | 337 | 1081 | 69.29 | 19.78 | 0.90 |
| 7 | 394 | 1196 | 78.98 | 22.59 | 1.00 |
| 8 | 449 | 1303 | 91.47 | 25.97 | 1.13 |
| 9 | 505 | 1414 | 89.43 | 25.89 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 24.00 | 7.62 | 0.48 |
| 2| 1983 | 26.59 | 9.01 | 0.52 |
| 3| 2067 | 27.44 | 9.89 | 0.53 |
| 5| 2459 | 32.48 | 12.64 | 0.61 |
| 10| 3227 | 41.64 | 18.56 | 0.76 |
| 41| 7636 | 98.84 | 55.08 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 645 | 22.81 | 7.37 | 0.42 |
| 2| 718 | 22.52 | 7.93 | 0.42 |
| 3| 994 | 27.81 | 10.10 | 0.49 |
| 5| 1191 | 29.14 | 11.80 | 0.52 |
| 10| 1913 | 37.69 | 17.51 | 0.67 |
| 42| 6679 | 98.57 | 55.79 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 27.54 | 8.47 | 0.46 |
| 2| 778 | 30.94 | 10.07 | 0.51 |
| 3| 868 | 32.08 | 11.03 | 0.53 |
| 5| 1230 | 36.90 | 13.75 | 0.60 |
| 10| 1975 | 44.36 | 19.21 | 0.73 |
| 37| 6061 | 98.94 | 52.54 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 671 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.17 | 11.17 | 0.55 |
| 3| 980 | 38.66 | 12.84 | 0.60 |
| 5| 1284 | 42.60 | 15.27 | 0.66 |
| 10| 2023 | 54.12 | 21.84 | 0.83 |
| 29| 4925 | 99.86 | 47.27 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.97 | 7.58 | 0.64 |
| 2| 5939 | 35.96 | 12.08 | 0.79 |
| 3| 6155 | 45.90 | 15.46 | 0.90 |
| 4| 6206 | 54.20 | 18.21 | 0.99 |
| 5| 6471 | 65.29 | 22.02 | 1.12 |
| 6| 6518 | 69.16 | 23.22 | 1.16 |
| 7| 6744 | 84.37 | 28.48 | 1.33 |
| 8| 6776 | 88.06 | 29.58 | 1.37 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 20 | 1139 | 6513 | 59.10 | 22.22 | 1.07 |
| 10 | 38 | 2164 | 7126 | 96.88 | 37.08 | 1.51 |

