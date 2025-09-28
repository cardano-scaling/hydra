--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-28 04:40:24.404283082 UTC |
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
| 1| 5836 | 10.26 | 3.25 | 0.51 |
| 2| 6038 | 12.44 | 3.94 | 0.54 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6646 | 19.00 | 6.01 | 0.64 |
| 10| 7647 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 170 | 747 | 42.46 | 12.22 | 0.61 |
| 4 | 228 | 858 | 48.25 | 14.02 | 0.68 |
| 5 | 283 | 969 | 59.52 | 17.09 | 0.80 |
| 6 | 340 | 1081 | 68.79 | 19.82 | 0.90 |
| 7 | 395 | 1192 | 76.93 | 22.10 | 0.98 |
| 8 | 449 | 1303 | 82.78 | 23.94 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1815 | 23.92 | 7.60 | 0.48 |
| 2| 1953 | 25.76 | 8.76 | 0.51 |
| 3| 2167 | 29.51 | 10.47 | 0.56 |
| 5| 2279 | 29.26 | 11.74 | 0.57 |
| 10| 3043 | 38.82 | 17.74 | 0.73 |
| 40| 7678 | 98.12 | 54.28 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 22.81 | 7.38 | 0.42 |
| 2| 707 | 22.62 | 7.96 | 0.42 |
| 3| 938 | 26.12 | 9.60 | 0.47 |
| 5| 1238 | 29.81 | 12.01 | 0.53 |
| 10| 2018 | 40.88 | 18.41 | 0.70 |
| 42| 6505 | 93.05 | 54.24 | 1.58 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 658 | 29.13 | 8.90 | 0.48 |
| 2| 740 | 30.23 | 9.85 | 0.50 |
| 3| 1015 | 31.69 | 10.98 | 0.53 |
| 5| 1330 | 35.72 | 13.46 | 0.59 |
| 10| 2105 | 45.57 | 19.57 | 0.75 |
| 36| 5919 | 97.14 | 51.33 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 33.83 | 10.15 | 0.53 |
| 2| 838 | 35.89 | 11.39 | 0.56 |
| 3| 1000 | 38.63 | 12.83 | 0.60 |
| 5| 1292 | 43.40 | 15.50 | 0.67 |
| 10| 2134 | 55.32 | 22.20 | 0.85 |
| 28| 4764 | 95.50 | 45.37 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5833 | 27.05 | 9.08 | 0.69 |
| 2| 5959 | 36.03 | 12.11 | 0.79 |
| 3| 6129 | 45.66 | 15.41 | 0.90 |
| 4| 6137 | 50.64 | 16.97 | 0.95 |
| 5| 6478 | 65.72 | 22.25 | 1.13 |
| 6| 6590 | 72.34 | 24.40 | 1.20 |
| 7| 6726 | 83.44 | 28.15 | 1.32 |
| 8| 6788 | 88.25 | 29.79 | 1.38 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.49 | 6.17 | 0.60 |
| 10 | 5 | 284 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1139 | 6513 | 60.35 | 22.66 | 1.09 |
| 10 | 30 | 1709 | 6856 | 80.04 | 30.46 | 1.32 |
| 10 | 37 | 2105 | 7090 | 95.02 | 36.33 | 1.49 |

