--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-26 10:22:22.718119094 UTC |
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
| 1| 5837 | 10.93 | 3.49 | 0.52 |
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 18.43 | 5.81 | 0.63 |
| 10| 7647 | 29.19 | 9.21 | 0.79 |
| 43| 14282 | 98.94 | 30.92 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 744 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2170 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 171 | 747 | 42.66 | 12.27 | 0.62 |
| 4 | 227 | 858 | 51.45 | 14.82 | 0.71 |
| 5 | 283 | 969 | 62.33 | 17.76 | 0.82 |
| 6 | 340 | 1081 | 75.17 | 21.27 | 0.96 |
| 7 | 395 | 1192 | 84.28 | 23.76 | 1.05 |
| 8 | 449 | 1303 | 85.33 | 24.55 | 1.07 |
| 9 | 507 | 1414 | 97.23 | 27.87 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1820 | 24.37 | 7.71 | 0.48 |
| 2| 1956 | 25.92 | 8.80 | 0.51 |
| 3| 2056 | 27.02 | 9.79 | 0.53 |
| 5| 2395 | 31.08 | 12.26 | 0.60 |
| 10| 3289 | 44.46 | 19.31 | 0.79 |
| 39| 7444 | 94.94 | 52.70 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 612 | 22.84 | 7.38 | 0.41 |
| 2| 810 | 25.52 | 8.81 | 0.46 |
| 3| 877 | 25.16 | 9.34 | 0.46 |
| 5| 1142 | 27.96 | 11.47 | 0.51 |
| 10| 1933 | 37.69 | 17.52 | 0.67 |
| 40| 6614 | 97.06 | 54.03 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 29.17 | 8.91 | 0.48 |
| 2| 817 | 29.18 | 9.60 | 0.49 |
| 3| 922 | 32.76 | 11.24 | 0.54 |
| 5| 1233 | 37.06 | 13.78 | 0.60 |
| 10| 1957 | 46.72 | 19.83 | 0.76 |
| 35| 5770 | 94.59 | 49.96 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 700 | 33.83 | 10.15 | 0.53 |
| 2| 803 | 35.85 | 11.38 | 0.56 |
| 3| 1026 | 38.59 | 12.82 | 0.60 |
| 5| 1342 | 43.17 | 15.45 | 0.67 |
| 10| 2029 | 53.87 | 21.76 | 0.83 |
| 29| 4836 | 97.21 | 46.47 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5783 | 27.16 | 9.11 | 0.69 |
| 2| 5982 | 37.02 | 12.49 | 0.80 |
| 3| 6062 | 44.80 | 15.06 | 0.89 |
| 4| 6220 | 54.33 | 18.25 | 0.99 |
| 5| 6395 | 62.54 | 21.03 | 1.09 |
| 6| 6492 | 72.01 | 24.25 | 1.19 |
| 7| 6698 | 80.22 | 27.05 | 1.29 |
| 8| 7061 | 96.50 | 32.65 | 1.48 |
| 9| 7086 | 99.81 | 33.65 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.68 | 6.24 | 0.60 |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 284 | 6003 | 29.35 | 10.43 | 0.73 |
| 10 | 20 | 1139 | 6513 | 59.28 | 22.29 | 1.08 |
| 10 | 30 | 1707 | 6853 | 80.04 | 30.46 | 1.32 |
| 10 | 38 | 2162 | 7125 | 96.19 | 36.84 | 1.51 |

