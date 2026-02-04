--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-04 09:52:00.251846177 UTC |
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
| 2| 6035 | 12.91 | 4.10 | 0.55 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 99.08 | 30.97 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1281 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10065 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.31 | 9.88 | 0.53 |
| 3 | 169 | 747 | 41.46 | 11.98 | 0.60 |
| 4 | 225 | 858 | 47.67 | 13.86 | 0.67 |
| 5 | 284 | 969 | 60.88 | 17.45 | 0.81 |
| 6 | 338 | 1081 | 72.28 | 20.57 | 0.93 |
| 7 | 393 | 1192 | 84.85 | 24.03 | 1.06 |
| 8 | 453 | 1303 | 83.67 | 24.30 | 1.06 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 24.37 | 7.71 | 0.48 |
| 2| 1946 | 25.84 | 8.78 | 0.51 |
| 3| 2013 | 26.24 | 9.56 | 0.52 |
| 5| 2381 | 30.96 | 12.23 | 0.59 |
| 10| 3208 | 42.42 | 18.74 | 0.77 |
| 42| 7676 | 97.01 | 55.25 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.54 | 7.30 | 0.41 |
| 2| 695 | 22.58 | 7.96 | 0.42 |
| 3| 881 | 25.85 | 9.56 | 0.47 |
| 5| 1173 | 28.01 | 11.47 | 0.51 |
| 10| 1949 | 38.65 | 17.79 | 0.68 |
| 40| 6449 | 96.64 | 53.92 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 669 | 29.17 | 8.91 | 0.48 |
| 2| 861 | 29.94 | 9.83 | 0.50 |
| 3| 946 | 30.90 | 10.74 | 0.52 |
| 5| 1276 | 36.99 | 13.77 | 0.60 |
| 10| 2072 | 45.49 | 19.55 | 0.75 |
| 37| 6161 | 99.98 | 52.83 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.87 | 10.16 | 0.53 |
| 2| 812 | 35.89 | 11.39 | 0.56 |
| 3| 895 | 37.13 | 12.38 | 0.58 |
| 5| 1284 | 43.25 | 15.47 | 0.67 |
| 10| 1994 | 53.90 | 21.77 | 0.83 |
| 30| 4924 | 98.31 | 47.43 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 27.08 | 9.08 | 0.69 |
| 2| 5916 | 34.87 | 11.67 | 0.78 |
| 3| 6064 | 42.45 | 14.27 | 0.86 |
| 4| 6255 | 54.76 | 18.46 | 1.00 |
| 5| 6468 | 65.25 | 22.01 | 1.12 |
| 6| 6551 | 72.99 | 24.60 | 1.21 |
| 7| 6588 | 77.82 | 26.14 | 1.26 |
| 8| 6660 | 85.89 | 28.85 | 1.35 |
| 9| 6886 | 98.61 | 33.14 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.93 | 6.32 | 0.61 |
| 10 | 1 | 57 | 5869 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6005 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 569 | 6173 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1138 | 6512 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1709 | 6855 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2220 | 7160 | 98.05 | 37.58 | 1.53 |

