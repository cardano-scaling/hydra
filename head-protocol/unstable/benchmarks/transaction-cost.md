--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-30 04:42:45.854650246 UTC |
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
| 2| 6041 | 12.67 | 4.01 | 0.55 |
| 3| 6238 | 14.67 | 4.64 | 0.58 |
| 5| 6641 | 18.71 | 5.91 | 0.64 |
| 10| 7647 | 29.14 | 9.19 | 0.79 |
| 43| 14279 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10067 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 32.39 | 9.43 | 0.51 |
| 3 | 171 | 747 | 42.61 | 12.28 | 0.62 |
| 4 | 228 | 862 | 48.27 | 14.01 | 0.68 |
| 5 | 284 | 969 | 62.59 | 17.86 | 0.83 |
| 6 | 339 | 1081 | 67.70 | 19.48 | 0.89 |
| 7 | 395 | 1192 | 78.79 | 22.62 | 1.00 |
| 8 | 451 | 1307 | 96.71 | 27.28 | 1.19 |
| 9 | 505 | 1414 | 90.42 | 26.01 | 1.13 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1801 | 24.00 | 7.62 | 0.48 |
| 2| 1879 | 24.77 | 8.48 | 0.49 |
| 3| 2059 | 27.40 | 9.88 | 0.53 |
| 5| 2363 | 31.34 | 12.32 | 0.60 |
| 10| 3020 | 38.90 | 17.76 | 0.73 |
| 39| 7468 | 97.64 | 53.40 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 634 | 22.77 | 7.36 | 0.42 |
| 2| 745 | 24.00 | 8.37 | 0.44 |
| 3| 942 | 26.91 | 9.86 | 0.48 |
| 5| 1265 | 29.19 | 11.80 | 0.53 |
| 10| 2096 | 41.74 | 18.65 | 0.71 |
| 40| 6576 | 96.78 | 53.96 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 660 | 29.09 | 8.89 | 0.48 |
| 2| 779 | 28.47 | 9.38 | 0.48 |
| 3| 1008 | 31.53 | 10.94 | 0.53 |
| 5| 1189 | 36.34 | 13.57 | 0.59 |
| 10| 2151 | 48.82 | 20.46 | 0.79 |
| 36| 5761 | 99.40 | 51.86 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 33.83 | 10.16 | 0.53 |
| 2| 837 | 35.88 | 11.39 | 0.56 |
| 3| 1040 | 39.22 | 13.02 | 0.61 |
| 5| 1271 | 42.49 | 15.24 | 0.66 |
| 10| 1968 | 53.42 | 21.61 | 0.82 |
| 29| 4774 | 97.00 | 46.42 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5833 | 27.08 | 9.09 | 0.69 |
| 2| 5870 | 35.03 | 11.71 | 0.78 |
| 3| 6191 | 45.83 | 15.48 | 0.90 |
| 4| 6198 | 54.03 | 18.16 | 0.99 |
| 5| 6345 | 60.76 | 20.42 | 1.07 |
| 6| 6630 | 74.19 | 25.04 | 1.22 |
| 7| 6804 | 83.95 | 28.35 | 1.33 |
| 8| 6798 | 89.30 | 30.08 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 30.23 | 10.73 | 0.74 |
| 10 | 30 | 1703 | 6849 | 78.71 | 30.00 | 1.30 |
| 10 | 38 | 2165 | 7127 | 96.19 | 36.84 | 1.51 |

