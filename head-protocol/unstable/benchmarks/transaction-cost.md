--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-16 14:05:18.084899751 UTC |
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
| 1| 5836 | 10.40 | 3.30 | 0.51 |
| 2| 6042 | 12.25 | 3.87 | 0.54 |
| 3| 6239 | 15.07 | 4.78 | 0.58 |
| 5| 6638 | 18.58 | 5.86 | 0.63 |
| 10| 7644 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 99.42 | 31.09 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 918 | 4.36 | 2.33 | 0.24 |
| 5| 1275 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 170 | 747 | 41.08 | 11.87 | 0.60 |
| 4 | 227 | 858 | 49.66 | 14.34 | 0.69 |
| 5 | 282 | 974 | 56.59 | 16.43 | 0.77 |
| 6 | 340 | 1081 | 73.79 | 20.94 | 0.94 |
| 7 | 394 | 1192 | 82.11 | 23.33 | 1.03 |
| 8 | 452 | 1303 | 96.80 | 27.30 | 1.19 |
| 9 | 506 | 1414 | 97.62 | 28.02 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1811 | 24.00 | 7.62 | 0.48 |
| 2| 1883 | 24.40 | 8.40 | 0.49 |
| 3| 2062 | 26.98 | 9.78 | 0.53 |
| 5| 2466 | 32.07 | 12.54 | 0.61 |
| 10| 3131 | 41.16 | 18.39 | 0.75 |
| 40| 7658 | 99.75 | 54.71 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 606 | 22.80 | 7.36 | 0.41 |
| 2| 834 | 25.53 | 8.79 | 0.46 |
| 3| 941 | 27.03 | 9.88 | 0.48 |
| 5| 1115 | 26.94 | 11.17 | 0.50 |
| 10| 1988 | 38.47 | 17.73 | 0.68 |
| 43| 6784 | 98.52 | 56.46 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 668 | 29.13 | 8.90 | 0.48 |
| 2| 782 | 30.94 | 10.07 | 0.51 |
| 3| 913 | 32.76 | 11.24 | 0.54 |
| 5| 1227 | 36.98 | 13.76 | 0.60 |
| 10| 1908 | 45.87 | 19.57 | 0.75 |
| 33| 5291 | 87.70 | 46.64 | 1.43 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.79 | 10.15 | 0.53 |
| 2| 815 | 35.89 | 11.39 | 0.56 |
| 3| 896 | 37.16 | 12.39 | 0.58 |
| 5| 1220 | 41.89 | 15.05 | 0.65 |
| 10| 2190 | 56.09 | 22.43 | 0.86 |
| 29| 4999 | 98.87 | 47.00 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5818 | 27.08 | 9.08 | 0.69 |
| 2| 5936 | 35.89 | 12.04 | 0.79 |
| 3| 6065 | 43.48 | 14.58 | 0.87 |
| 4| 6253 | 51.45 | 17.30 | 0.97 |
| 5| 6412 | 64.20 | 21.61 | 1.11 |
| 6| 6554 | 74.49 | 25.16 | 1.22 |
| 7| 6577 | 73.01 | 24.46 | 1.21 |
| 8| 6745 | 84.22 | 28.31 | 1.33 |
| 9| 6991 | 99.99 | 33.78 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 285 | 6005 | 28.65 | 10.19 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1140 | 6514 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1707 | 6853 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2220 | 7159 | 98.93 | 37.88 | 1.54 |

