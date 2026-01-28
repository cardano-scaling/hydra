--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-28 13:55:29.987687942 UTC |
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
| 1| 5834 | 10.64 | 3.38 | 0.52 |
| 2| 6035 | 12.46 | 3.94 | 0.54 |
| 3| 6239 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7648 | 29.14 | 9.19 | 0.79 |
| 43| 14282 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1278 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 170 | 747 | 41.28 | 11.92 | 0.60 |
| 4 | 226 | 858 | 53.34 | 15.19 | 0.73 |
| 5 | 282 | 969 | 56.40 | 16.35 | 0.77 |
| 6 | 338 | 1081 | 69.84 | 19.92 | 0.91 |
| 7 | 394 | 1196 | 80.61 | 22.93 | 1.02 |
| 8 | 449 | 1303 | 80.64 | 23.38 | 1.03 |
| 9 | 504 | 1414 | 94.44 | 27.19 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1784 | 24.37 | 7.71 | 0.48 |
| 2| 1940 | 25.51 | 8.70 | 0.50 |
| 3| 2055 | 26.98 | 9.78 | 0.53 |
| 5| 2462 | 32.19 | 12.57 | 0.61 |
| 10| 3180 | 42.22 | 18.69 | 0.77 |
| 39| 7495 | 97.20 | 53.31 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.84 | 7.38 | 0.42 |
| 2| 742 | 23.58 | 8.23 | 0.43 |
| 3| 854 | 24.07 | 9.03 | 0.45 |
| 5| 1223 | 29.19 | 11.80 | 0.53 |
| 10| 1911 | 38.54 | 17.76 | 0.67 |
| 39| 6399 | 97.95 | 53.58 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.17 | 8.91 | 0.48 |
| 2| 780 | 30.91 | 10.06 | 0.51 |
| 3| 944 | 30.82 | 10.73 | 0.52 |
| 5| 1471 | 39.16 | 14.43 | 0.64 |
| 10| 1932 | 43.29 | 18.89 | 0.72 |
| 35| 5674 | 93.52 | 49.64 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 695 | 33.87 | 10.16 | 0.53 |
| 2| 908 | 36.52 | 11.59 | 0.57 |
| 3| 1025 | 38.63 | 12.83 | 0.60 |
| 5| 1293 | 42.83 | 15.34 | 0.66 |
| 10| 2076 | 55.00 | 22.08 | 0.85 |
| 29| 4918 | 98.42 | 46.83 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5813 | 27.08 | 9.09 | 0.69 |
| 2| 5982 | 36.92 | 12.44 | 0.80 |
| 3| 6170 | 47.19 | 15.93 | 0.92 |
| 4| 6162 | 50.08 | 16.78 | 0.95 |
| 5| 6278 | 60.50 | 20.28 | 1.06 |
| 6| 6489 | 70.64 | 23.74 | 1.18 |
| 7| 6809 | 83.93 | 28.34 | 1.33 |
| 8| 6956 | 95.08 | 32.10 | 1.46 |
| 9| 6982 | 97.89 | 32.95 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 20.07 | 6.71 | 0.62 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6003 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 569 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6513 | 60.61 | 22.74 | 1.09 |
| 10 | 40 | 2280 | 7196 | 99.22 | 38.09 | 1.54 |
| 10 | 39 | 2221 | 7161 | 98.49 | 37.73 | 1.53 |

