--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-02 04:39:57.430097753 UTC |
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
| 1| 5840 | 10.67 | 3.39 | 0.52 |
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6239 | 14.31 | 4.52 | 0.57 |
| 5| 6645 | 18.84 | 5.95 | 0.64 |
| 10| 7647 | 29.02 | 9.14 | 0.79 |
| 43| 14281 | 98.87 | 30.90 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10051 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 171 | 747 | 42.54 | 12.22 | 0.61 |
| 4 | 227 | 862 | 47.92 | 13.90 | 0.68 |
| 5 | 282 | 969 | 58.06 | 16.74 | 0.78 |
| 6 | 338 | 1081 | 68.37 | 19.64 | 0.89 |
| 7 | 393 | 1196 | 78.48 | 22.42 | 1.00 |
| 8 | 451 | 1303 | 80.52 | 23.30 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 24.00 | 7.62 | 0.48 |
| 2| 1996 | 26.47 | 8.98 | 0.52 |
| 3| 2073 | 26.90 | 9.76 | 0.53 |
| 5| 2326 | 30.22 | 12.01 | 0.58 |
| 10| 3221 | 41.73 | 18.57 | 0.76 |
| 40| 7662 | 97.24 | 54.03 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 22.84 | 7.40 | 0.42 |
| 2| 825 | 25.20 | 8.73 | 0.45 |
| 3| 878 | 25.12 | 9.32 | 0.46 |
| 5| 1160 | 28.07 | 11.50 | 0.51 |
| 10| 1936 | 37.76 | 17.54 | 0.67 |
| 41| 6639 | 96.56 | 54.58 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 27.54 | 8.47 | 0.46 |
| 2| 818 | 29.22 | 9.61 | 0.49 |
| 3| 1043 | 34.14 | 11.66 | 0.56 |
| 5| 1317 | 35.64 | 13.44 | 0.59 |
| 10| 1891 | 42.87 | 18.75 | 0.72 |
| 37| 6218 | 99.97 | 52.81 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 708 | 33.87 | 10.16 | 0.53 |
| 2| 810 | 35.89 | 11.39 | 0.56 |
| 3| 950 | 37.91 | 12.62 | 0.59 |
| 5| 1214 | 41.90 | 15.05 | 0.65 |
| 10| 2029 | 54.06 | 21.81 | 0.83 |
| 28| 4957 | 97.38 | 45.93 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5827 | 27.08 | 9.09 | 0.69 |
| 2| 5983 | 36.93 | 12.44 | 0.80 |
| 3| 6139 | 45.86 | 15.44 | 0.90 |
| 4| 6141 | 50.00 | 16.76 | 0.95 |
| 5| 6419 | 62.80 | 21.12 | 1.09 |
| 6| 6646 | 75.46 | 25.48 | 1.24 |
| 7| 6764 | 84.15 | 28.41 | 1.33 |
| 8| 6694 | 83.84 | 28.12 | 1.32 |
| 9| 7031 | 98.90 | 33.31 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 1 | 57 | 5869 | 20.34 | 6.91 | 0.62 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1139 | 6514 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1706 | 6852 | 80.92 | 30.76 | 1.33 |
| 10 | 37 | 2104 | 7089 | 94.58 | 36.18 | 1.49 |

