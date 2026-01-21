--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-21 14:20:11.717369212 UTC |
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
| 1| 5840 | 10.61 | 3.37 | 0.52 |
| 2| 6041 | 12.70 | 4.03 | 0.55 |
| 3| 6238 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 18.60 | 5.87 | 0.64 |
| 10| 7647 | 28.94 | 9.11 | 0.79 |
| 43| 14285 | 99.32 | 31.06 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 640 | 34.19 | 9.84 | 0.53 |
| 3 | 170 | 747 | 42.61 | 12.28 | 0.62 |
| 4 | 226 | 858 | 48.19 | 13.99 | 0.68 |
| 5 | 283 | 969 | 57.35 | 16.63 | 0.78 |
| 6 | 338 | 1081 | 73.87 | 20.96 | 0.95 |
| 7 | 392 | 1192 | 77.40 | 22.29 | 0.99 |
| 8 | 449 | 1303 | 99.67 | 28.04 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1814 | 24.37 | 7.71 | 0.48 |
| 2| 1969 | 26.79 | 9.05 | 0.52 |
| 3| 2112 | 28.47 | 10.18 | 0.55 |
| 5| 2378 | 31.22 | 12.29 | 0.60 |
| 10| 3283 | 44.45 | 19.31 | 0.79 |
| 41| 7818 | 99.54 | 55.33 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.54 | 7.30 | 0.41 |
| 2| 741 | 24.04 | 8.38 | 0.44 |
| 3| 1009 | 28.22 | 10.20 | 0.50 |
| 5| 1157 | 28.01 | 11.47 | 0.51 |
| 10| 2068 | 42.72 | 18.91 | 0.72 |
| 41| 6583 | 99.04 | 55.25 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 695 | 27.54 | 8.47 | 0.46 |
| 2| 852 | 31.58 | 10.26 | 0.52 |
| 3| 869 | 31.97 | 11.01 | 0.53 |
| 5| 1278 | 37.73 | 13.99 | 0.61 |
| 10| 2072 | 45.08 | 19.43 | 0.75 |
| 37| 6102 | 99.27 | 52.62 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 33.83 | 10.15 | 0.53 |
| 2| 823 | 35.85 | 11.38 | 0.56 |
| 3| 948 | 37.91 | 12.62 | 0.59 |
| 5| 1202 | 41.97 | 15.07 | 0.65 |
| 10| 1981 | 53.76 | 21.71 | 0.83 |
| 30| 4842 | 98.59 | 47.48 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5807 | 27.05 | 9.07 | 0.69 |
| 2| 5960 | 35.84 | 12.03 | 0.79 |
| 3| 6108 | 45.92 | 15.51 | 0.90 |
| 4| 6171 | 50.28 | 16.89 | 0.95 |
| 5| 6414 | 63.65 | 21.39 | 1.10 |
| 6| 6667 | 73.79 | 24.94 | 1.22 |
| 7| 6470 | 71.97 | 24.15 | 1.19 |
| 8| 6919 | 90.66 | 30.56 | 1.41 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 284 | 6003 | 28.90 | 10.28 | 0.72 |
| 10 | 20 | 1140 | 6515 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1705 | 6851 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2216 | 7155 | 99.38 | 38.03 | 1.54 |

