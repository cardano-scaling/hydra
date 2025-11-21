--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-21 04:50:44.69520135 UTC |
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
| 1| 5836 | 10.19 | 3.22 | 0.51 |
| 2| 6038 | 12.46 | 3.94 | 0.55 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7648 | 28.94 | 9.11 | 0.79 |
| 43| 14282 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2182 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 640 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 43.94 | 12.60 | 0.63 |
| 4 | 226 | 858 | 49.36 | 14.29 | 0.69 |
| 5 | 281 | 969 | 62.62 | 17.81 | 0.83 |
| 6 | 339 | 1081 | 64.20 | 18.61 | 0.85 |
| 7 | 395 | 1196 | 72.59 | 21.09 | 0.94 |
| 8 | 450 | 1303 | 95.50 | 26.88 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1819 | 24.37 | 7.71 | 0.48 |
| 2| 1971 | 26.55 | 9.00 | 0.52 |
| 3| 2219 | 29.42 | 10.45 | 0.56 |
| 5| 2317 | 29.89 | 11.93 | 0.58 |
| 10| 3278 | 43.14 | 18.95 | 0.78 |
| 39| 7381 | 96.44 | 53.07 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 622 | 22.57 | 7.31 | 0.41 |
| 2| 697 | 22.62 | 7.95 | 0.42 |
| 3| 899 | 25.85 | 9.55 | 0.47 |
| 5| 1330 | 32.39 | 12.70 | 0.56 |
| 10| 2070 | 39.87 | 18.13 | 0.69 |
| 40| 6564 | 98.15 | 54.30 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 676 | 27.50 | 8.46 | 0.46 |
| 2| 806 | 30.95 | 10.07 | 0.51 |
| 3| 1008 | 34.11 | 11.65 | 0.55 |
| 5| 1188 | 36.31 | 13.56 | 0.59 |
| 10| 2179 | 49.43 | 20.65 | 0.79 |
| 37| 6034 | 97.98 | 52.26 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 701 | 33.87 | 10.16 | 0.53 |
| 2| 827 | 35.88 | 11.39 | 0.56 |
| 3| 895 | 37.13 | 12.38 | 0.58 |
| 5| 1374 | 43.80 | 15.64 | 0.68 |
| 10| 2105 | 54.62 | 21.99 | 0.84 |
| 29| 4958 | 99.65 | 47.16 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5798 | 27.00 | 9.06 | 0.69 |
| 2| 5894 | 34.87 | 11.67 | 0.78 |
| 3| 6094 | 42.52 | 14.31 | 0.87 |
| 4| 6269 | 53.48 | 18.02 | 0.99 |
| 5| 6442 | 63.70 | 21.42 | 1.10 |
| 6| 6435 | 68.10 | 22.87 | 1.15 |
| 7| 6727 | 81.34 | 27.44 | 1.30 |
| 8| 6736 | 84.34 | 28.23 | 1.33 |
| 9| 6924 | 97.92 | 32.98 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.66 | 7.37 | 0.64 |
| 10 | 20 | 1136 | 6510 | 59.10 | 22.22 | 1.07 |
| 10 | 39 | 2217 | 7156 | 98.93 | 37.88 | 1.54 |

