--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-02 14:25:03.129418232 UTC |
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
| 1| 5838 | 10.17 | 3.22 | 0.51 |
| 2| 6038 | 13.08 | 4.16 | 0.55 |
| 3| 6239 | 14.48 | 4.58 | 0.57 |
| 5| 6640 | 19.17 | 6.07 | 0.64 |
| 10| 7644 | 28.73 | 9.04 | 0.78 |
| 43| 14282 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10066 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 640 | 32.35 | 9.42 | 0.51 |
| 3 | 171 | 747 | 42.35 | 12.18 | 0.61 |
| 4 | 226 | 858 | 48.08 | 13.93 | 0.68 |
| 5 | 281 | 969 | 60.91 | 17.46 | 0.81 |
| 6 | 338 | 1081 | 66.60 | 19.22 | 0.87 |
| 7 | 394 | 1192 | 80.70 | 22.99 | 1.02 |
| 8 | 452 | 1303 | 79.99 | 23.22 | 1.02 |
| 9 | 505 | 1414 | 98.25 | 27.99 | 1.21 |
| 10 | 560 | 1525 | 98.48 | 28.38 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1810 | 24.00 | 7.62 | 0.48 |
| 2| 1943 | 25.43 | 8.68 | 0.50 |
| 3| 2131 | 27.94 | 10.05 | 0.54 |
| 5| 2377 | 31.33 | 12.32 | 0.60 |
| 10| 3110 | 40.55 | 18.24 | 0.75 |
| 40| 7495 | 94.86 | 53.34 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.54 | 7.30 | 0.41 |
| 2| 770 | 23.56 | 8.22 | 0.43 |
| 3| 874 | 25.58 | 9.50 | 0.46 |
| 5| 1165 | 28.66 | 11.67 | 0.52 |
| 10| 2110 | 43.36 | 19.09 | 0.73 |
| 42| 6790 | 98.62 | 55.83 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 29.13 | 8.90 | 0.48 |
| 2| 819 | 29.15 | 9.59 | 0.49 |
| 3| 952 | 33.43 | 11.44 | 0.54 |
| 5| 1301 | 37.62 | 13.97 | 0.61 |
| 10| 2110 | 45.42 | 19.54 | 0.75 |
| 34| 5747 | 93.58 | 49.09 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 696 | 33.87 | 10.16 | 0.53 |
| 2| 851 | 36.48 | 11.58 | 0.57 |
| 3| 1009 | 38.51 | 12.80 | 0.60 |
| 5| 1200 | 41.97 | 15.07 | 0.65 |
| 10| 2016 | 54.02 | 21.80 | 0.83 |
| 29| 4724 | 95.39 | 45.94 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.93 | 7.56 | 0.64 |
| 2| 5962 | 37.08 | 12.50 | 0.80 |
| 3| 6126 | 45.85 | 15.45 | 0.90 |
| 4| 6212 | 51.02 | 17.18 | 0.96 |
| 5| 6450 | 64.07 | 21.60 | 1.11 |
| 6| 6634 | 75.41 | 25.48 | 1.24 |
| 7| 6736 | 79.21 | 26.73 | 1.28 |
| 8| 6914 | 94.89 | 32.01 | 1.45 |
| 9| 6976 | 98.98 | 33.39 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 569 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1139 | 6514 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1706 | 6852 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2220 | 7159 | 99.38 | 38.04 | 1.54 |

