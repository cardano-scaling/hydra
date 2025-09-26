--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-26 04:42:09.771324204 UTC |
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
| 1| 5834 | 10.78 | 3.43 | 0.52 |
| 2| 6037 | 12.25 | 3.87 | 0.54 |
| 3| 6236 | 14.79 | 4.69 | 0.58 |
| 5| 6643 | 18.43 | 5.81 | 0.63 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 98.64 | 30.82 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 913 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2164 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 171 | 747 | 39.93 | 11.62 | 0.59 |
| 4 | 225 | 858 | 50.24 | 14.45 | 0.70 |
| 5 | 284 | 969 | 56.35 | 16.36 | 0.77 |
| 6 | 339 | 1081 | 69.82 | 19.91 | 0.91 |
| 7 | 392 | 1192 | 86.94 | 24.53 | 1.08 |
| 8 | 452 | 1303 | 98.96 | 27.87 | 1.21 |
| 9 | 508 | 1414 | 94.22 | 27.14 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1804 | 24.37 | 7.71 | 0.48 |
| 2| 1925 | 25.76 | 8.76 | 0.51 |
| 3| 2082 | 26.98 | 9.78 | 0.53 |
| 5| 2325 | 30.35 | 12.04 | 0.58 |
| 10| 3038 | 38.56 | 17.68 | 0.72 |
| 41| 7769 | 99.74 | 55.33 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 616 | 22.84 | 7.39 | 0.42 |
| 2| 726 | 22.52 | 7.93 | 0.42 |
| 3| 962 | 26.65 | 9.79 | 0.48 |
| 5| 1102 | 27.11 | 11.22 | 0.50 |
| 10| 2028 | 39.72 | 18.07 | 0.69 |
| 39| 6552 | 97.51 | 53.47 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 27.54 | 8.47 | 0.46 |
| 2| 829 | 29.15 | 9.59 | 0.49 |
| 3| 872 | 32.01 | 11.01 | 0.53 |
| 5| 1223 | 37.13 | 13.80 | 0.60 |
| 10| 1988 | 47.44 | 20.04 | 0.77 |
| 35| 6052 | 98.43 | 51.13 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 33.83 | 10.15 | 0.53 |
| 2| 805 | 35.85 | 11.38 | 0.56 |
| 3| 899 | 37.24 | 12.41 | 0.58 |
| 5| 1301 | 43.40 | 15.50 | 0.67 |
| 10| 2117 | 54.81 | 22.03 | 0.85 |
| 29| 4787 | 96.68 | 46.30 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5824 | 27.09 | 9.10 | 0.69 |
| 2| 5924 | 36.04 | 12.12 | 0.79 |
| 3| 6127 | 45.78 | 15.44 | 0.90 |
| 4| 6334 | 54.70 | 18.42 | 1.00 |
| 5| 6517 | 65.09 | 21.99 | 1.12 |
| 6| 6513 | 70.24 | 23.62 | 1.17 |
| 7| 6794 | 84.81 | 28.67 | 1.34 |
| 8| 7009 | 93.31 | 31.60 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5869 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 284 | 6004 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 569 | 6173 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6513 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1709 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 38 | 2161 | 7123 | 95.56 | 36.62 | 1.50 |

