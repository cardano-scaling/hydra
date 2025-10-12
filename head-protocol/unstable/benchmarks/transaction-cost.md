--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-12 04:40:02.544879691 UTC |
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
| 1| 5837 | 10.93 | 3.49 | 0.52 |
| 2| 6038 | 12.42 | 3.93 | 0.54 |
| 3| 6239 | 14.76 | 4.67 | 0.58 |
| 5| 6640 | 18.79 | 5.94 | 0.64 |
| 10| 7647 | 29.02 | 9.14 | 0.79 |
| 43| 14283 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10082 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 640 | 33.25 | 9.61 | 0.52 |
| 3 | 169 | 751 | 43.95 | 12.60 | 0.63 |
| 4 | 226 | 858 | 51.15 | 14.70 | 0.71 |
| 5 | 283 | 969 | 61.46 | 17.56 | 0.82 |
| 6 | 337 | 1081 | 68.25 | 19.61 | 0.89 |
| 7 | 395 | 1192 | 76.55 | 21.96 | 0.98 |
| 8 | 450 | 1303 | 99.28 | 27.89 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 24.00 | 7.62 | 0.48 |
| 2| 1993 | 26.54 | 9.00 | 0.52 |
| 3| 2122 | 27.94 | 10.05 | 0.54 |
| 5| 2369 | 31.41 | 12.34 | 0.60 |
| 10| 3132 | 40.70 | 18.28 | 0.75 |
| 40| 7684 | 98.43 | 54.35 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 649 | 22.54 | 7.31 | 0.41 |
| 2| 814 | 25.10 | 8.68 | 0.45 |
| 3| 893 | 25.01 | 9.30 | 0.46 |
| 5| 1267 | 31.31 | 12.40 | 0.55 |
| 10| 1942 | 36.86 | 17.28 | 0.66 |
| 42| 6683 | 98.40 | 55.74 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 654 | 29.17 | 8.91 | 0.48 |
| 2| 884 | 29.97 | 9.84 | 0.50 |
| 3| 944 | 30.94 | 10.75 | 0.52 |
| 5| 1175 | 36.20 | 13.53 | 0.59 |
| 10| 2169 | 46.70 | 19.91 | 0.77 |
| 37| 5840 | 96.37 | 51.75 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 693 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.14 | 11.16 | 0.55 |
| 3| 991 | 38.59 | 12.82 | 0.60 |
| 5| 1298 | 43.24 | 15.47 | 0.67 |
| 10| 2168 | 55.64 | 22.28 | 0.86 |
| 29| 5014 | 99.51 | 47.14 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.97 | 7.58 | 0.64 |
| 2| 5982 | 35.91 | 12.06 | 0.79 |
| 3| 5989 | 41.60 | 13.93 | 0.85 |
| 4| 6248 | 53.95 | 18.11 | 0.99 |
| 5| 6259 | 59.57 | 19.97 | 1.05 |
| 6| 6588 | 73.36 | 24.72 | 1.21 |
| 7| 6785 | 84.61 | 28.54 | 1.34 |
| 8| 7029 | 96.22 | 32.58 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 5 | 285 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 20 | 1139 | 6513 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1707 | 6853 | 80.04 | 30.46 | 1.32 |
| 10 | 38 | 2164 | 7127 | 97.77 | 37.38 | 1.52 |

