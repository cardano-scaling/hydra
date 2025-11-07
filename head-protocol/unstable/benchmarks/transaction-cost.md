--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-07 10:09:06.752865415 UTC |
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
| 1| 5837 | 10.66 | 3.39 | 0.52 |
| 2| 6039 | 12.41 | 3.92 | 0.54 |
| 3| 6243 | 14.50 | 4.58 | 0.58 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7651 | 28.73 | 9.04 | 0.78 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10052 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 43.75 | 12.51 | 0.63 |
| 4 | 226 | 858 | 49.68 | 14.34 | 0.69 |
| 5 | 283 | 969 | 61.13 | 17.54 | 0.81 |
| 6 | 336 | 1081 | 75.59 | 21.34 | 0.96 |
| 7 | 396 | 1192 | 74.83 | 21.63 | 0.96 |
| 8 | 450 | 1303 | 98.78 | 27.77 | 1.21 |
| 9 | 506 | 1414 | 98.54 | 28.06 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1796 | 24.00 | 7.62 | 0.48 |
| 2| 1930 | 25.47 | 8.69 | 0.50 |
| 3| 2119 | 28.02 | 10.07 | 0.54 |
| 5| 2381 | 31.41 | 12.34 | 0.60 |
| 10| 3062 | 40.08 | 18.09 | 0.74 |
| 41| 7729 | 99.17 | 55.19 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.57 | 7.31 | 0.41 |
| 2| 777 | 24.32 | 8.46 | 0.44 |
| 3| 1015 | 28.10 | 10.17 | 0.50 |
| 5| 1249 | 31.01 | 12.33 | 0.54 |
| 10| 1914 | 38.82 | 17.83 | 0.68 |
| 43| 6553 | 95.47 | 55.57 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 27.54 | 8.47 | 0.46 |
| 2| 775 | 28.55 | 9.40 | 0.48 |
| 3| 906 | 30.23 | 10.54 | 0.51 |
| 5| 1269 | 35.04 | 13.25 | 0.58 |
| 10| 2089 | 45.61 | 19.58 | 0.75 |
| 35| 5719 | 93.40 | 49.59 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 669 | 33.87 | 10.16 | 0.53 |
| 2| 827 | 35.85 | 11.38 | 0.56 |
| 3| 947 | 37.91 | 12.62 | 0.59 |
| 5| 1201 | 42.01 | 15.08 | 0.65 |
| 10| 2112 | 54.92 | 22.08 | 0.85 |
| 29| 4683 | 95.99 | 46.07 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.97 | 7.57 | 0.64 |
| 2| 5845 | 31.44 | 10.45 | 0.74 |
| 3| 6108 | 44.81 | 15.04 | 0.89 |
| 4| 6159 | 48.00 | 16.07 | 0.92 |
| 5| 6347 | 60.34 | 20.34 | 1.06 |
| 6| 6667 | 75.33 | 25.40 | 1.24 |
| 7| 6865 | 86.10 | 29.12 | 1.36 |
| 8| 6962 | 94.73 | 32.05 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5869 | 19.64 | 6.67 | 0.62 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 20 | 1137 | 6511 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1706 | 6852 | 81.99 | 31.13 | 1.34 |
| 10 | 39 | 2222 | 7162 | 98.49 | 37.73 | 1.53 |

