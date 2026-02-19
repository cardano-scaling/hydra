--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-19 15:12:22.279401413 UTC |
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
| 1| 5837 | 10.85 | 3.45 | 0.52 |
| 2| 6037 | 12.92 | 4.11 | 0.55 |
| 3| 6239 | 14.72 | 4.66 | 0.58 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7644 | 28.81 | 9.07 | 0.78 |
| 43| 14285 | 98.94 | 30.92 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 169 | 747 | 41.40 | 11.95 | 0.60 |
| 4 | 227 | 858 | 52.43 | 14.98 | 0.72 |
| 5 | 282 | 969 | 59.64 | 17.12 | 0.80 |
| 6 | 339 | 1081 | 71.79 | 20.50 | 0.93 |
| 7 | 394 | 1192 | 74.33 | 21.38 | 0.96 |
| 8 | 450 | 1303 | 80.81 | 23.42 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1811 | 24.00 | 7.62 | 0.48 |
| 2| 1933 | 25.55 | 8.71 | 0.50 |
| 3| 2013 | 25.87 | 9.47 | 0.52 |
| 5| 2411 | 31.30 | 12.31 | 0.60 |
| 10| 3107 | 40.15 | 18.12 | 0.74 |
| 39| 7600 | 98.08 | 53.56 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.84 | 7.38 | 0.42 |
| 2| 789 | 23.59 | 8.23 | 0.43 |
| 3| 871 | 25.09 | 9.31 | 0.46 |
| 5| 1264 | 30.02 | 12.04 | 0.54 |
| 10| 1951 | 38.46 | 17.73 | 0.67 |
| 41| 6609 | 97.70 | 54.90 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 661 | 29.17 | 8.91 | 0.48 |
| 2| 819 | 29.26 | 9.62 | 0.49 |
| 3| 1036 | 31.69 | 10.98 | 0.53 |
| 5| 1393 | 36.43 | 13.67 | 0.60 |
| 10| 2019 | 48.23 | 20.28 | 0.77 |
| 35| 5675 | 93.02 | 49.51 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 693 | 33.87 | 10.16 | 0.53 |
| 2| 760 | 35.17 | 11.17 | 0.55 |
| 3| 938 | 37.88 | 12.61 | 0.59 |
| 5| 1207 | 41.93 | 15.06 | 0.65 |
| 10| 2079 | 54.68 | 22.00 | 0.84 |
| 30| 4902 | 98.70 | 47.54 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5823 | 27.08 | 9.09 | 0.69 |
| 2| 5916 | 34.75 | 11.63 | 0.78 |
| 3| 6012 | 41.25 | 13.79 | 0.85 |
| 4| 6310 | 54.90 | 18.48 | 1.00 |
| 5| 6432 | 61.26 | 20.63 | 1.08 |
| 6| 6398 | 68.95 | 23.17 | 1.16 |
| 7| 6799 | 82.65 | 27.86 | 1.32 |
| 8| 6912 | 92.50 | 31.18 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 570 | 6174 | 40.39 | 14.75 | 0.85 |
| 10 | 39 | 2221 | 7161 | 98.93 | 37.88 | 1.54 |

