--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-08 10:38:07.989805381 UTC |
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
| 1| 5837 | 10.38 | 3.29 | 0.51 |
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6236 | 14.97 | 4.75 | 0.58 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10066 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 33.18 | 9.60 | 0.52 |
| 3 | 170 | 747 | 43.75 | 12.55 | 0.63 |
| 4 | 226 | 858 | 48.30 | 14.01 | 0.68 |
| 5 | 281 | 974 | 57.51 | 16.58 | 0.78 |
| 6 | 340 | 1081 | 67.91 | 19.50 | 0.89 |
| 7 | 394 | 1192 | 83.00 | 23.55 | 1.04 |
| 8 | 451 | 1303 | 87.60 | 25.14 | 1.10 |
| 9 | 505 | 1414 | 91.15 | 26.24 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 24.29 | 7.69 | 0.48 |
| 2| 1946 | 25.55 | 8.72 | 0.51 |
| 3| 2055 | 26.99 | 9.78 | 0.53 |
| 5| 2344 | 30.08 | 11.98 | 0.58 |
| 10| 3293 | 42.97 | 18.91 | 0.78 |
| 39| 7604 | 96.73 | 53.23 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 600 | 22.53 | 7.31 | 0.41 |
| 2| 755 | 24.08 | 8.41 | 0.44 |
| 3| 895 | 25.06 | 9.31 | 0.46 |
| 5| 1238 | 29.08 | 11.77 | 0.52 |
| 10| 1943 | 38.73 | 17.81 | 0.68 |
| 41| 6595 | 96.86 | 54.64 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 645 | 29.13 | 8.90 | 0.48 |
| 2| 856 | 29.89 | 9.82 | 0.50 |
| 3| 1021 | 34.07 | 11.64 | 0.55 |
| 5| 1310 | 37.62 | 13.96 | 0.61 |
| 10| 2014 | 47.33 | 20.01 | 0.77 |
| 35| 5878 | 96.39 | 50.54 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 33.83 | 10.16 | 0.53 |
| 2| 818 | 35.92 | 11.40 | 0.56 |
| 3| 895 | 37.20 | 12.40 | 0.58 |
| 5| 1281 | 42.68 | 15.29 | 0.66 |
| 10| 2039 | 54.21 | 21.85 | 0.84 |
| 29| 4947 | 99.41 | 47.14 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5781 | 27.13 | 9.10 | 0.69 |
| 2| 5937 | 35.81 | 12.03 | 0.79 |
| 3| 6136 | 46.12 | 15.53 | 0.90 |
| 4| 6256 | 54.79 | 18.46 | 1.00 |
| 5| 6449 | 63.59 | 21.40 | 1.10 |
| 6| 6602 | 74.15 | 24.97 | 1.22 |
| 7| 6639 | 78.89 | 26.49 | 1.27 |
| 8| 6816 | 89.63 | 30.16 | 1.39 |
| 9| 6994 | 96.53 | 32.50 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6004 | 28.02 | 9.98 | 0.71 |
| 10 | 10 | 569 | 6173 | 39.69 | 14.52 | 0.85 |
| 10 | 20 | 1138 | 6513 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1709 | 6856 | 81.37 | 30.91 | 1.33 |
| 10 | 39 | 2216 | 7155 | 99.82 | 38.19 | 1.55 |

