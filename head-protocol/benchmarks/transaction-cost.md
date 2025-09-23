--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-23 11:43:08.671541133 UTC |
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
| 1| 5837 | 10.36 | 3.28 | 0.51 |
| 2| 6038 | 12.41 | 3.92 | 0.54 |
| 3| 6239 | 14.78 | 4.68 | 0.58 |
| 5| 6638 | 18.41 | 5.80 | 0.63 |
| 10| 7647 | 28.73 | 9.04 | 0.78 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.38 | 9.91 | 0.53 |
| 3 | 170 | 747 | 43.79 | 12.52 | 0.63 |
| 4 | 226 | 862 | 51.19 | 14.76 | 0.71 |
| 5 | 282 | 969 | 59.61 | 17.17 | 0.80 |
| 6 | 340 | 1081 | 66.27 | 19.17 | 0.87 |
| 7 | 395 | 1192 | 73.21 | 21.29 | 0.95 |
| 8 | 450 | 1303 | 92.35 | 26.19 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1818 | 24.37 | 7.71 | 0.48 |
| 2| 1938 | 25.39 | 8.68 | 0.50 |
| 3| 2129 | 28.48 | 10.18 | 0.55 |
| 5| 2484 | 32.15 | 12.56 | 0.61 |
| 10| 3023 | 39.04 | 17.80 | 0.73 |
| 41| 7671 | 98.16 | 54.91 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 634 | 22.80 | 7.37 | 0.42 |
| 2| 817 | 25.47 | 8.79 | 0.46 |
| 3| 952 | 26.99 | 9.87 | 0.48 |
| 5| 1302 | 32.34 | 12.69 | 0.56 |
| 10| 1911 | 37.45 | 17.44 | 0.66 |
| 40| 6409 | 94.09 | 53.22 | 1.58 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 27.51 | 8.47 | 0.46 |
| 2| 866 | 29.97 | 9.84 | 0.50 |
| 3| 1007 | 33.36 | 11.43 | 0.55 |
| 5| 1260 | 35.01 | 13.24 | 0.58 |
| 10| 1943 | 47.14 | 19.95 | 0.76 |
| 35| 6044 | 97.84 | 50.98 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.83 | 10.16 | 0.53 |
| 2| 768 | 35.17 | 11.17 | 0.55 |
| 3| 989 | 38.51 | 12.80 | 0.60 |
| 5| 1271 | 42.72 | 15.30 | 0.66 |
| 10| 2006 | 54.25 | 21.86 | 0.83 |
| 29| 4872 | 98.54 | 46.84 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5799 | 27.13 | 9.11 | 0.69 |
| 2| 5941 | 35.98 | 12.11 | 0.79 |
| 3| 6135 | 45.61 | 15.40 | 0.90 |
| 4| 6364 | 57.54 | 19.44 | 1.04 |
| 5| 6432 | 64.82 | 21.96 | 1.12 |
| 6| 6697 | 75.83 | 25.61 | 1.24 |
| 7| 6763 | 80.34 | 27.07 | 1.29 |
| 8| 6866 | 89.94 | 30.30 | 1.40 |
| 9| 6973 | 94.76 | 31.86 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 56 | 5867 | 20.96 | 7.13 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.25 | 14.36 | 0.84 |
| 10 | 20 | 1140 | 6515 | 59.73 | 22.44 | 1.08 |
| 10 | 39 | 2218 | 7158 | 99.38 | 38.04 | 1.54 |

