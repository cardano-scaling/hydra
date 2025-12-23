--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-23 12:07:06.202088929 UTC |
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
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6238 | 14.31 | 4.52 | 0.57 |
| 5| 6638 | 18.79 | 5.94 | 0.64 |
| 10| 7644 | 29.02 | 9.14 | 0.79 |
| 43| 14285 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.18 | 9.60 | 0.52 |
| 3 | 171 | 747 | 43.86 | 12.56 | 0.63 |
| 4 | 226 | 862 | 52.52 | 15.02 | 0.72 |
| 5 | 282 | 974 | 56.11 | 16.30 | 0.76 |
| 6 | 340 | 1081 | 75.00 | 21.19 | 0.96 |
| 7 | 393 | 1192 | 80.43 | 22.88 | 1.02 |
| 8 | 450 | 1303 | 93.97 | 26.52 | 1.16 |
| 9 | 504 | 1414 | 95.27 | 27.46 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1802 | 23.92 | 7.60 | 0.48 |
| 2| 1942 | 25.80 | 8.77 | 0.51 |
| 3| 2053 | 26.95 | 9.77 | 0.53 |
| 5| 2416 | 32.02 | 12.53 | 0.61 |
| 10| 3155 | 41.01 | 18.37 | 0.75 |
| 40| 7587 | 97.50 | 54.05 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.54 | 7.30 | 0.41 |
| 2| 746 | 23.61 | 8.23 | 0.43 |
| 3| 974 | 26.09 | 9.59 | 0.47 |
| 5| 1134 | 28.11 | 11.50 | 0.51 |
| 10| 2068 | 41.45 | 18.59 | 0.71 |
| 41| 6704 | 98.59 | 55.16 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 703 | 27.54 | 8.47 | 0.46 |
| 2| 812 | 29.26 | 9.62 | 0.49 |
| 3| 983 | 31.50 | 10.93 | 0.53 |
| 5| 1343 | 36.35 | 13.65 | 0.60 |
| 10| 2075 | 48.00 | 20.22 | 0.78 |
| 34| 5707 | 93.34 | 48.99 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 698 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.17 | 11.17 | 0.55 |
| 3| 937 | 37.95 | 12.63 | 0.59 |
| 5| 1324 | 43.31 | 15.48 | 0.67 |
| 10| 2122 | 55.74 | 22.32 | 0.85 |
| 30| 4870 | 98.05 | 47.34 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5782 | 27.00 | 9.07 | 0.69 |
| 2| 5950 | 35.96 | 12.06 | 0.79 |
| 3| 6058 | 42.46 | 14.26 | 0.86 |
| 4| 6233 | 53.74 | 18.07 | 0.99 |
| 5| 6345 | 63.32 | 21.26 | 1.09 |
| 6| 6600 | 71.50 | 24.11 | 1.19 |
| 7| 6731 | 79.27 | 26.67 | 1.28 |
| 8| 6750 | 90.97 | 30.61 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 20.70 | 6.93 | 0.62 |
| 10 | 1 | 57 | 5869 | 19.89 | 6.76 | 0.62 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 30 | 1707 | 6853 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2220 | 7160 | 99.38 | 38.04 | 1.54 |

