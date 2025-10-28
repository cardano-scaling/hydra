--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 15:18:51.61143624 UTC |
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
| 1| 5837 | 10.40 | 3.30 | 0.51 |
| 2| 6037 | 12.46 | 3.94 | 0.55 |
| 3| 6236 | 14.90 | 4.72 | 0.58 |
| 5| 6643 | 18.41 | 5.80 | 0.63 |
| 10| 7644 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 32.24 | 9.37 | 0.51 |
| 3 | 171 | 747 | 42.66 | 12.27 | 0.62 |
| 4 | 227 | 858 | 48.15 | 13.98 | 0.68 |
| 5 | 282 | 969 | 64.39 | 18.26 | 0.84 |
| 6 | 339 | 1081 | 65.08 | 18.93 | 0.86 |
| 7 | 394 | 1192 | 87.21 | 24.60 | 1.08 |
| 8 | 453 | 1303 | 82.38 | 23.74 | 1.04 |
| 9 | 506 | 1418 | 88.78 | 25.73 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1746 | 23.30 | 7.41 | 0.47 |
| 2| 1996 | 26.50 | 8.99 | 0.52 |
| 3| 2055 | 26.94 | 9.77 | 0.53 |
| 5| 2486 | 32.44 | 12.63 | 0.61 |
| 10| 3199 | 42.20 | 18.70 | 0.77 |
| 39| 7393 | 95.20 | 52.78 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.81 | 7.37 | 0.42 |
| 2| 791 | 24.28 | 8.45 | 0.44 |
| 3| 948 | 27.07 | 9.89 | 0.48 |
| 5| 1190 | 29.14 | 11.78 | 0.52 |
| 10| 2010 | 39.90 | 18.16 | 0.69 |
| 43| 6704 | 97.30 | 56.10 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 658 | 29.13 | 8.90 | 0.48 |
| 2| 771 | 28.55 | 9.40 | 0.48 |
| 3| 991 | 31.61 | 10.96 | 0.53 |
| 5| 1287 | 35.00 | 13.24 | 0.59 |
| 10| 1946 | 46.80 | 19.84 | 0.76 |
| 37| 6052 | 99.00 | 52.53 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 671 | 33.87 | 10.16 | 0.53 |
| 2| 764 | 35.17 | 11.17 | 0.55 |
| 3| 980 | 38.59 | 12.82 | 0.60 |
| 5| 1338 | 43.31 | 15.48 | 0.67 |
| 10| 2009 | 53.34 | 21.59 | 0.83 |
| 29| 4900 | 97.52 | 46.55 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5844 | 27.08 | 9.10 | 0.69 |
| 2| 5917 | 34.76 | 11.63 | 0.78 |
| 3| 6090 | 44.72 | 15.05 | 0.89 |
| 4| 6210 | 54.88 | 18.51 | 1.00 |
| 5| 6403 | 63.76 | 21.48 | 1.10 |
| 6| 6448 | 70.33 | 23.65 | 1.17 |
| 7| 6600 | 78.98 | 26.56 | 1.27 |
| 8| 6799 | 91.23 | 30.69 | 1.41 |
| 9| 6870 | 91.92 | 30.86 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 283 | 6002 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 569 | 6174 | 40.58 | 14.82 | 0.86 |
| 10 | 20 | 1138 | 6513 | 60.87 | 22.83 | 1.09 |
| 10 | 39 | 2219 | 7158 | 98.05 | 37.58 | 1.53 |

