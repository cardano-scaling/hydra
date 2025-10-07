--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-07 04:40:23.784774602 UTC |
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
| 1| 5840 | 10.61 | 3.37 | 0.52 |
| 2| 6038 | 12.65 | 4.01 | 0.55 |
| 3| 6243 | 14.52 | 4.59 | 0.58 |
| 5| 6638 | 18.43 | 5.81 | 0.63 |
| 10| 7647 | 28.73 | 9.04 | 0.78 |
| 43| 14279 | 99.49 | 31.12 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 42.94 | 12.38 | 0.62 |
| 4 | 226 | 858 | 49.28 | 14.27 | 0.69 |
| 5 | 281 | 974 | 56.05 | 16.26 | 0.76 |
| 6 | 339 | 1081 | 73.60 | 20.90 | 0.94 |
| 7 | 395 | 1192 | 84.73 | 23.96 | 1.06 |
| 8 | 449 | 1303 | 94.24 | 26.68 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1794 | 23.92 | 7.60 | 0.48 |
| 2| 1966 | 26.58 | 9.01 | 0.52 |
| 3| 2056 | 27.31 | 9.86 | 0.53 |
| 5| 2431 | 32.00 | 12.52 | 0.61 |
| 10| 3368 | 45.39 | 19.61 | 0.81 |
| 39| 7540 | 97.45 | 53.37 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.77 | 7.36 | 0.41 |
| 2| 695 | 22.58 | 7.96 | 0.42 |
| 3| 886 | 25.51 | 9.47 | 0.46 |
| 5| 1201 | 29.87 | 12.01 | 0.53 |
| 10| 1988 | 38.75 | 17.80 | 0.68 |
| 42| 6607 | 95.66 | 54.98 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 29.13 | 8.90 | 0.48 |
| 2| 736 | 30.27 | 9.86 | 0.50 |
| 3| 987 | 33.43 | 11.45 | 0.55 |
| 5| 1258 | 35.08 | 13.26 | 0.58 |
| 10| 1976 | 46.84 | 19.85 | 0.76 |
| 36| 6095 | 98.57 | 51.84 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 33.83 | 10.16 | 0.53 |
| 2| 889 | 36.56 | 11.60 | 0.57 |
| 3| 942 | 37.91 | 12.62 | 0.59 |
| 5| 1316 | 43.40 | 15.50 | 0.67 |
| 10| 2112 | 55.49 | 22.24 | 0.85 |
| 29| 4952 | 97.59 | 46.61 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5793 | 27.04 | 9.07 | 0.69 |
| 2| 5978 | 35.95 | 12.08 | 0.79 |
| 3| 6080 | 45.23 | 15.22 | 0.89 |
| 4| 6384 | 56.85 | 19.28 | 1.03 |
| 5| 6213 | 56.12 | 18.72 | 1.01 |
| 6| 6601 | 70.70 | 23.80 | 1.18 |
| 7| 6852 | 85.69 | 28.97 | 1.35 |
| 8| 6961 | 91.01 | 30.76 | 1.41 |
| 9| 6911 | 95.51 | 32.04 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1137 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1710 | 6856 | 79.60 | 30.31 | 1.31 |
| 10 | 38 | 2159 | 7122 | 96.81 | 37.05 | 1.51 |

