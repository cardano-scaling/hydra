--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-11 10:31:47.786251573 UTC |
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
| 1| 5838 | 10.61 | 3.37 | 0.52 |
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6238 | 14.76 | 4.67 | 0.58 |
| 5| 6641 | 18.81 | 5.94 | 0.64 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14279 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 918 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10065 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 640 | 32.19 | 9.36 | 0.51 |
| 3 | 170 | 747 | 43.72 | 12.52 | 0.63 |
| 4 | 227 | 858 | 48.30 | 14.01 | 0.68 |
| 5 | 282 | 969 | 64.32 | 18.24 | 0.84 |
| 6 | 339 | 1085 | 70.33 | 20.15 | 0.91 |
| 7 | 394 | 1192 | 73.37 | 21.14 | 0.95 |
| 8 | 452 | 1303 | 93.58 | 26.47 | 1.15 |
| 9 | 504 | 1414 | 93.05 | 26.74 | 1.16 |
| 10 | 560 | 1525 | 97.81 | 28.29 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1809 | 23.92 | 7.60 | 0.48 |
| 2| 1881 | 24.47 | 8.41 | 0.49 |
| 3| 2083 | 27.10 | 9.81 | 0.53 |
| 5| 2280 | 28.89 | 11.65 | 0.57 |
| 10| 3188 | 42.26 | 18.70 | 0.77 |
| 38| 7589 | 99.76 | 53.37 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 638 | 22.84 | 7.40 | 0.42 |
| 2| 843 | 25.41 | 8.76 | 0.46 |
| 3| 915 | 26.71 | 9.79 | 0.48 |
| 5| 1226 | 29.94 | 12.03 | 0.53 |
| 10| 1941 | 37.62 | 17.49 | 0.67 |
| 40| 6336 | 94.21 | 53.21 | 1.57 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 652 | 29.17 | 8.91 | 0.48 |
| 2| 842 | 29.22 | 9.61 | 0.49 |
| 3| 973 | 30.87 | 10.74 | 0.52 |
| 5| 1309 | 37.77 | 14.00 | 0.61 |
| 10| 2046 | 48.22 | 20.28 | 0.78 |
| 35| 5713 | 94.51 | 49.94 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 700 | 33.83 | 10.15 | 0.53 |
| 2| 833 | 35.89 | 11.39 | 0.56 |
| 3| 1015 | 38.59 | 12.82 | 0.60 |
| 5| 1200 | 41.82 | 15.03 | 0.65 |
| 10| 2029 | 54.14 | 21.83 | 0.83 |
| 29| 4965 | 99.41 | 47.12 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5808 | 27.13 | 9.10 | 0.69 |
| 2| 5964 | 35.75 | 12.01 | 0.79 |
| 3| 6099 | 44.65 | 15.02 | 0.89 |
| 4| 6249 | 54.00 | 18.14 | 0.99 |
| 5| 6382 | 63.98 | 21.55 | 1.10 |
| 6| 6532 | 73.40 | 24.73 | 1.21 |
| 7| 6647 | 75.57 | 25.35 | 1.24 |
| 8| 6729 | 88.60 | 29.73 | 1.38 |
| 9| 6858 | 97.18 | 32.64 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.63 | 6.56 | 0.61 |
| 10 | 5 | 284 | 6003 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 569 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1137 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1707 | 6853 | 79.34 | 30.22 | 1.31 |
| 10 | 40 | 2275 | 7191 | 99.22 | 38.09 | 1.54 |
| 10 | 39 | 2218 | 7157 | 99.82 | 38.19 | 1.55 |

