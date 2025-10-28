--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 13:48:24.586494813 UTC |
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
| 1| 5834 | 10.19 | 3.22 | 0.51 |
| 2| 6038 | 12.82 | 4.07 | 0.55 |
| 3| 6238 | 14.72 | 4.66 | 0.58 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7644 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.73 | 30.85 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10063 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 32.30 | 9.40 | 0.51 |
| 3 | 171 | 747 | 42.66 | 12.27 | 0.62 |
| 4 | 228 | 862 | 54.28 | 15.49 | 0.74 |
| 5 | 283 | 974 | 59.70 | 17.14 | 0.80 |
| 6 | 339 | 1081 | 69.81 | 19.95 | 0.91 |
| 7 | 395 | 1192 | 80.18 | 22.78 | 1.01 |
| 8 | 450 | 1303 | 82.57 | 23.79 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 23.30 | 7.41 | 0.47 |
| 2| 1886 | 24.77 | 8.48 | 0.49 |
| 3| 2097 | 28.39 | 10.16 | 0.55 |
| 5| 2477 | 32.28 | 12.59 | 0.61 |
| 10| 3142 | 41.06 | 18.37 | 0.75 |
| 39| 7430 | 94.51 | 52.59 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 652 | 22.81 | 7.37 | 0.42 |
| 2| 750 | 24.31 | 8.47 | 0.44 |
| 3| 904 | 25.10 | 9.32 | 0.46 |
| 5| 1205 | 29.15 | 11.79 | 0.52 |
| 10| 1949 | 38.16 | 17.64 | 0.67 |
| 40| 6629 | 99.27 | 54.65 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 650 | 29.17 | 8.91 | 0.48 |
| 2| 737 | 30.27 | 9.86 | 0.50 |
| 3| 1007 | 31.65 | 10.97 | 0.53 |
| 5| 1126 | 35.60 | 13.34 | 0.58 |
| 10| 1993 | 44.15 | 19.14 | 0.73 |
| 36| 5915 | 95.97 | 51.04 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 690 | 33.83 | 10.15 | 0.53 |
| 2| 803 | 35.85 | 11.38 | 0.56 |
| 3| 1001 | 38.51 | 12.80 | 0.60 |
| 5| 1349 | 43.43 | 15.51 | 0.67 |
| 10| 1900 | 52.67 | 21.38 | 0.81 |
| 29| 4935 | 98.56 | 46.87 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5792 | 27.00 | 9.08 | 0.69 |
| 2| 5822 | 31.60 | 10.52 | 0.74 |
| 3| 6148 | 46.04 | 15.52 | 0.90 |
| 4| 6253 | 55.31 | 18.65 | 1.01 |
| 5| 6281 | 57.78 | 19.44 | 1.03 |
| 6| 6690 | 75.38 | 25.45 | 1.24 |
| 7| 6537 | 77.77 | 26.03 | 1.25 |
| 8| 6870 | 88.87 | 29.85 | 1.39 |
| 9| 6791 | 92.24 | 30.89 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6004 | 28.02 | 9.98 | 0.71 |
| 10 | 20 | 1136 | 6510 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1706 | 6853 | 79.15 | 30.16 | 1.31 |
| 10 | 39 | 2222 | 7161 | 98.86 | 37.86 | 1.54 |

