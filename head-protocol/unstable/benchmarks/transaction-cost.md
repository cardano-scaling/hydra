--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-13 13:43:29.730563676 UTC |
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
| 1| 5837 | 10.19 | 3.22 | 0.51 |
| 2| 6038 | 12.54 | 3.97 | 0.55 |
| 3| 6238 | 14.60 | 4.62 | 0.58 |
| 5| 6641 | 18.43 | 5.81 | 0.63 |
| 10| 7644 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.85 | 30.89 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2183 | 12.13 | 7.25 | 0.40 |
| 54| 10053 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 42.83 | 12.33 | 0.62 |
| 4 | 224 | 862 | 49.87 | 14.41 | 0.70 |
| 5 | 281 | 969 | 59.05 | 16.95 | 0.79 |
| 6 | 337 | 1081 | 73.53 | 20.84 | 0.94 |
| 7 | 395 | 1192 | 84.81 | 23.94 | 1.06 |
| 8 | 451 | 1303 | 89.42 | 25.48 | 1.11 |
| 9 | 504 | 1414 | 93.77 | 26.93 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1799 | 24.00 | 7.62 | 0.48 |
| 2| 1881 | 24.43 | 8.40 | 0.49 |
| 3| 2101 | 28.39 | 10.16 | 0.55 |
| 5| 2335 | 30.42 | 12.06 | 0.59 |
| 10| 3144 | 40.74 | 18.29 | 0.75 |
| 41| 7690 | 99.75 | 55.37 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.84 | 7.39 | 0.41 |
| 2| 700 | 22.55 | 7.95 | 0.42 |
| 3| 882 | 25.13 | 9.33 | 0.46 |
| 5| 1239 | 29.07 | 11.77 | 0.52 |
| 10| 2138 | 42.82 | 18.95 | 0.73 |
| 43| 6772 | 99.44 | 56.70 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.13 | 8.90 | 0.48 |
| 2| 813 | 29.26 | 9.62 | 0.49 |
| 3| 1004 | 31.69 | 10.98 | 0.53 |
| 5| 1303 | 37.81 | 14.01 | 0.61 |
| 10| 2028 | 44.79 | 19.34 | 0.74 |
| 36| 5873 | 96.18 | 51.08 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.15 | 0.53 |
| 2| 849 | 36.56 | 11.60 | 0.57 |
| 3| 949 | 38.25 | 12.72 | 0.59 |
| 5| 1244 | 42.72 | 15.30 | 0.66 |
| 10| 2057 | 54.20 | 21.86 | 0.84 |
| 29| 4971 | 99.71 | 47.19 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.97 | 7.57 | 0.64 |
| 2| 5943 | 35.84 | 12.04 | 0.79 |
| 3| 6120 | 46.27 | 15.59 | 0.91 |
| 4| 6140 | 50.63 | 16.99 | 0.95 |
| 5| 6488 | 66.03 | 22.30 | 1.13 |
| 6| 6457 | 69.40 | 23.35 | 1.16 |
| 7| 6605 | 78.85 | 26.46 | 1.27 |
| 8| 6839 | 90.62 | 30.49 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.49 | 6.17 | 0.60 |
| 10 | 1 | 56 | 5867 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 569 | 6173 | 40.39 | 14.75 | 0.85 |
| 10 | 30 | 1708 | 6854 | 80.22 | 30.52 | 1.32 |
| 10 | 40 | 2277 | 7193 | 99.66 | 38.24 | 1.55 |

