--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-10 09:14:26.013994264 UTC |
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
| 1| 5841 | 10.57 | 3.36 | 0.52 |
| 2| 6035 | 12.42 | 3.93 | 0.54 |
| 3| 6242 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 18.84 | 5.95 | 0.64 |
| 10| 7647 | 28.73 | 9.04 | 0.78 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.24 | 9.37 | 0.51 |
| 3 | 171 | 747 | 43.88 | 12.56 | 0.63 |
| 4 | 228 | 858 | 53.84 | 15.34 | 0.73 |
| 5 | 281 | 969 | 57.54 | 16.59 | 0.78 |
| 6 | 341 | 1081 | 67.76 | 19.46 | 0.89 |
| 7 | 395 | 1192 | 78.55 | 22.44 | 1.00 |
| 8 | 449 | 1303 | 98.46 | 27.65 | 1.20 |
| 10 | 560 | 1529 | 96.83 | 28.05 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1801 | 23.92 | 7.60 | 0.48 |
| 2| 1978 | 26.46 | 8.97 | 0.52 |
| 3| 2013 | 25.87 | 9.47 | 0.52 |
| 5| 2276 | 29.18 | 11.72 | 0.57 |
| 10| 3101 | 39.46 | 17.94 | 0.73 |
| 40| 7698 | 99.78 | 54.71 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 603 | 22.84 | 7.37 | 0.41 |
| 2| 795 | 25.45 | 8.77 | 0.45 |
| 3| 942 | 26.67 | 9.79 | 0.48 |
| 5| 1092 | 27.15 | 11.23 | 0.50 |
| 10| 2091 | 41.09 | 18.46 | 0.71 |
| 40| 6713 | 99.89 | 54.84 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 678 | 29.13 | 8.90 | 0.48 |
| 2| 822 | 29.19 | 9.60 | 0.49 |
| 3| 932 | 32.72 | 11.23 | 0.54 |
| 5| 1272 | 37.74 | 13.99 | 0.61 |
| 10| 2048 | 45.16 | 19.45 | 0.75 |
| 34| 5380 | 89.12 | 47.73 | 1.45 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 690 | 33.83 | 10.15 | 0.53 |
| 2| 869 | 36.52 | 11.59 | 0.57 |
| 3| 950 | 37.91 | 12.62 | 0.59 |
| 5| 1312 | 43.25 | 15.47 | 0.67 |
| 10| 2067 | 54.25 | 21.86 | 0.84 |
| 28| 4719 | 95.36 | 45.33 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5811 | 27.08 | 9.08 | 0.69 |
| 2| 5908 | 32.44 | 10.84 | 0.75 |
| 3| 6117 | 45.82 | 15.45 | 0.90 |
| 4| 6294 | 56.03 | 18.92 | 1.02 |
| 5| 6354 | 60.44 | 20.28 | 1.06 |
| 6| 6521 | 73.59 | 24.72 | 1.21 |
| 7| 6736 | 84.06 | 28.34 | 1.33 |
| 8| 6745 | 91.09 | 30.55 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5869 | 19.89 | 6.76 | 0.62 |
| 10 | 5 | 284 | 6004 | 28.65 | 10.19 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1138 | 6512 | 58.21 | 21.92 | 1.07 |
| 10 | 30 | 1706 | 6852 | 79.15 | 30.16 | 1.31 |
| 10 | 37 | 2107 | 7092 | 93.95 | 35.96 | 1.48 |

