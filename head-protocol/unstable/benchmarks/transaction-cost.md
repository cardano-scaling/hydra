--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-08 18:53:35.813963267 UTC |
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
| 1| 5836 | 10.17 | 3.22 | 0.51 |
| 2| 6038 | 12.46 | 3.94 | 0.55 |
| 3| 6238 | 14.76 | 4.67 | 0.58 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7650 | 28.81 | 9.07 | 0.79 |
| 43| 14279 | 99.13 | 30.99 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10055 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 528 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.20 | 9.36 | 0.51 |
| 3 | 168 | 747 | 42.57 | 12.25 | 0.62 |
| 4 | 225 | 858 | 50.57 | 14.55 | 0.70 |
| 5 | 280 | 969 | 59.69 | 17.17 | 0.80 |
| 6 | 338 | 1081 | 73.35 | 20.87 | 0.94 |
| 7 | 395 | 1192 | 78.61 | 22.45 | 1.00 |
| 8 | 452 | 1303 | 82.13 | 23.78 | 1.04 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.00 | 7.62 | 0.48 |
| 2| 2005 | 26.84 | 9.06 | 0.52 |
| 3| 2066 | 27.32 | 9.86 | 0.53 |
| 5| 2479 | 33.19 | 12.85 | 0.62 |
| 10| 3157 | 40.78 | 18.30 | 0.75 |
| 40| 7633 | 96.44 | 53.82 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 604 | 22.84 | 7.38 | 0.41 |
| 2| 764 | 23.61 | 8.24 | 0.43 |
| 3| 892 | 25.16 | 9.33 | 0.46 |
| 5| 1256 | 31.33 | 12.40 | 0.55 |
| 10| 1846 | 36.51 | 17.17 | 0.65 |
| 40| 6352 | 94.33 | 53.28 | 1.58 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 27.54 | 8.47 | 0.46 |
| 2| 828 | 29.26 | 9.62 | 0.49 |
| 3| 944 | 30.86 | 10.73 | 0.52 |
| 5| 1250 | 37.02 | 13.77 | 0.60 |
| 10| 1990 | 44.22 | 19.16 | 0.73 |
| 35| 5959 | 97.84 | 50.93 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.15 | 0.53 |
| 2| 765 | 35.17 | 11.17 | 0.55 |
| 3| 934 | 37.95 | 12.63 | 0.59 |
| 5| 1205 | 41.86 | 15.04 | 0.65 |
| 10| 2098 | 54.62 | 21.99 | 0.84 |
| 30| 4971 | 99.84 | 47.88 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5790 | 27.00 | 9.08 | 0.69 |
| 2| 5968 | 35.88 | 12.05 | 0.79 |
| 3| 6152 | 46.11 | 15.53 | 0.91 |
| 4| 6274 | 54.73 | 18.46 | 1.00 |
| 5| 6384 | 60.63 | 20.37 | 1.07 |
| 6| 6624 | 74.14 | 25.04 | 1.22 |
| 7| 6787 | 84.22 | 28.41 | 1.33 |
| 8| 6962 | 90.07 | 30.39 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5869 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 284 | 6003 | 29.53 | 10.50 | 0.73 |
| 10 | 30 | 1704 | 6851 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2222 | 7162 | 98.49 | 37.73 | 1.53 |

