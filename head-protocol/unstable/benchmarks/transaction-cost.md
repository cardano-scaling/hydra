--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-30 11:01:41.109961223 UTC |
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
| 1| 5841 | 10.55 | 3.35 | 0.52 |
| 2| 6038 | 12.91 | 4.10 | 0.55 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 18.90 | 5.97 | 0.64 |
| 10| 7646 | 29.28 | 9.24 | 0.79 |
| 43| 14281 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10077 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 33.33 | 9.66 | 0.52 |
| 3 | 171 | 751 | 41.24 | 11.93 | 0.60 |
| 4 | 226 | 858 | 53.38 | 15.20 | 0.73 |
| 5 | 283 | 969 | 57.55 | 16.59 | 0.78 |
| 6 | 338 | 1081 | 71.15 | 20.27 | 0.92 |
| 7 | 395 | 1192 | 87.36 | 24.68 | 1.09 |
| 8 | 448 | 1303 | 89.64 | 25.58 | 1.12 |
| 9 | 508 | 1414 | 90.65 | 26.12 | 1.13 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1818 | 24.00 | 7.62 | 0.48 |
| 2| 1926 | 25.51 | 8.70 | 0.50 |
| 3| 2054 | 27.43 | 9.89 | 0.53 |
| 5| 2326 | 30.27 | 12.04 | 0.58 |
| 10| 3075 | 39.78 | 18.02 | 0.74 |
| 38| 7493 | 97.41 | 52.75 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 612 | 22.84 | 7.39 | 0.41 |
| 2| 786 | 25.16 | 8.70 | 0.45 |
| 3| 914 | 25.14 | 9.33 | 0.46 |
| 5| 1160 | 28.16 | 11.51 | 0.51 |
| 10| 1965 | 38.35 | 17.70 | 0.67 |
| 42| 6731 | 99.66 | 56.06 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 651 | 29.17 | 8.91 | 0.48 |
| 2| 812 | 29.18 | 9.60 | 0.49 |
| 3| 952 | 33.05 | 11.33 | 0.54 |
| 5| 1267 | 37.85 | 14.02 | 0.61 |
| 10| 2035 | 44.71 | 19.32 | 0.74 |
| 35| 6123 | 99.40 | 51.41 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 33.87 | 10.16 | 0.53 |
| 2| 871 | 36.64 | 11.62 | 0.57 |
| 3| 945 | 37.91 | 12.62 | 0.59 |
| 5| 1229 | 41.93 | 15.06 | 0.65 |
| 10| 1940 | 52.60 | 21.37 | 0.81 |
| 30| 4916 | 99.18 | 47.68 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5789 | 27.13 | 9.10 | 0.69 |
| 2| 5891 | 34.94 | 11.70 | 0.78 |
| 3| 6019 | 41.36 | 13.85 | 0.85 |
| 4| 6336 | 56.00 | 18.90 | 1.02 |
| 5| 6392 | 57.71 | 19.37 | 1.04 |
| 6| 6712 | 74.99 | 25.35 | 1.23 |
| 7| 6812 | 85.01 | 28.74 | 1.34 |
| 8| 6820 | 89.30 | 30.09 | 1.39 |
| 10| 6996 | 99.27 | 33.22 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 285 | 6004 | 29.98 | 10.65 | 0.73 |
| 10 | 10 | 570 | 6174 | 39.25 | 14.36 | 0.84 |
| 10 | 20 | 1138 | 6513 | 59.54 | 22.38 | 1.08 |
| 10 | 37 | 2102 | 7087 | 94.39 | 36.12 | 1.49 |

