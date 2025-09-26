--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-26 09:33:02.646630792 UTC |
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
| 2| 6035 | 12.25 | 3.87 | 0.54 |
| 3| 6238 | 14.86 | 4.71 | 0.58 |
| 5| 6641 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 29.09 | 9.17 | 0.79 |
| 43| 14281 | 99.23 | 31.02 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10068 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 32.24 | 9.37 | 0.51 |
| 3 | 171 | 747 | 43.73 | 12.51 | 0.63 |
| 4 | 227 | 858 | 48.21 | 14.04 | 0.68 |
| 5 | 282 | 969 | 64.22 | 18.22 | 0.84 |
| 6 | 338 | 1081 | 66.10 | 19.06 | 0.87 |
| 7 | 395 | 1192 | 86.87 | 24.47 | 1.08 |
| 8 | 449 | 1303 | 98.67 | 27.70 | 1.20 |
| 9 | 504 | 1414 | 94.80 | 27.34 | 1.18 |
| 10 | 560 | 1525 | 95.51 | 27.67 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 24.37 | 7.71 | 0.48 |
| 2| 1922 | 25.76 | 8.76 | 0.51 |
| 3| 2089 | 27.23 | 9.84 | 0.53 |
| 5| 2344 | 30.34 | 12.04 | 0.59 |
| 10| 3165 | 42.19 | 18.68 | 0.77 |
| 42| 7813 | 99.11 | 55.85 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 633 | 22.80 | 7.38 | 0.42 |
| 2| 865 | 25.37 | 8.76 | 0.46 |
| 3| 861 | 24.11 | 9.04 | 0.45 |
| 5| 1119 | 26.94 | 11.17 | 0.50 |
| 10| 2014 | 40.62 | 18.34 | 0.70 |
| 40| 6505 | 97.15 | 54.03 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 664 | 29.13 | 8.90 | 0.48 |
| 2| 850 | 31.69 | 10.29 | 0.52 |
| 3| 1014 | 31.65 | 10.97 | 0.53 |
| 5| 1238 | 37.02 | 13.78 | 0.60 |
| 10| 2131 | 45.26 | 19.50 | 0.75 |
| 36| 6095 | 98.88 | 51.90 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 706 | 33.79 | 10.15 | 0.53 |
| 2| 858 | 36.48 | 11.58 | 0.57 |
| 3| 1038 | 38.59 | 12.82 | 0.60 |
| 5| 1230 | 41.97 | 15.07 | 0.65 |
| 10| 2049 | 53.91 | 21.77 | 0.83 |
| 29| 4859 | 97.89 | 46.66 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5843 | 27.08 | 9.09 | 0.69 |
| 2| 5824 | 31.48 | 10.46 | 0.74 |
| 3| 6095 | 44.85 | 15.05 | 0.89 |
| 4| 6221 | 51.67 | 17.38 | 0.97 |
| 5| 6435 | 61.78 | 20.79 | 1.08 |
| 6| 6492 | 69.37 | 23.31 | 1.16 |
| 7| 6828 | 83.43 | 28.12 | 1.33 |
| 8| 6880 | 92.68 | 31.19 | 1.43 |
| 9| 6890 | 95.51 | 32.17 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 22.55 | 7.67 | 0.65 |
| 10 | 5 | 284 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1137 | 6512 | 60.42 | 22.68 | 1.09 |
| 10 | 38 | 2161 | 7123 | 95.56 | 36.62 | 1.50 |

