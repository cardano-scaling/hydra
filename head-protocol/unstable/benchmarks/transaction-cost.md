--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-05 16:01:05.606476443 UTC |
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
| 1| 5837 | 10.85 | 3.45 | 0.52 |
| 2| 6038 | 12.67 | 4.01 | 0.55 |
| 3| 6242 | 14.31 | 4.52 | 0.57 |
| 5| 6638 | 18.64 | 5.88 | 0.64 |
| 10| 7647 | 29.02 | 9.14 | 0.79 |
| 43| 14279 | 99.25 | 31.03 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1278 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10055 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 34.20 | 9.84 | 0.53 |
| 3 | 171 | 751 | 42.56 | 12.27 | 0.62 |
| 4 | 226 | 858 | 50.84 | 14.62 | 0.70 |
| 5 | 282 | 974 | 64.71 | 18.37 | 0.85 |
| 6 | 339 | 1081 | 64.24 | 18.61 | 0.85 |
| 7 | 395 | 1192 | 84.33 | 23.86 | 1.06 |
| 8 | 450 | 1303 | 82.80 | 23.85 | 1.05 |
| 9 | 506 | 1414 | 99.81 | 28.54 | 1.22 |
| 10 | 560 | 1525 | 98.47 | 28.58 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 24.37 | 7.71 | 0.48 |
| 2| 1936 | 25.51 | 8.70 | 0.50 |
| 3| 2114 | 28.31 | 10.14 | 0.55 |
| 5| 2342 | 30.26 | 12.02 | 0.58 |
| 10| 3162 | 40.97 | 18.36 | 0.75 |
| 39| 7689 | 99.56 | 53.98 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.57 | 7.32 | 0.41 |
| 2| 799 | 25.59 | 8.81 | 0.46 |
| 3| 900 | 25.03 | 9.30 | 0.46 |
| 5| 1251 | 30.06 | 12.04 | 0.54 |
| 10| 2133 | 41.59 | 18.61 | 0.71 |
| 41| 6630 | 95.13 | 54.19 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 27.54 | 8.47 | 0.46 |
| 2| 770 | 28.51 | 9.39 | 0.48 |
| 3| 1082 | 32.21 | 11.15 | 0.54 |
| 5| 1288 | 37.62 | 13.97 | 0.61 |
| 10| 2034 | 44.79 | 19.34 | 0.74 |
| 35| 5930 | 97.14 | 50.73 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.17 | 11.17 | 0.55 |
| 3| 1007 | 38.47 | 12.79 | 0.60 |
| 5| 1203 | 41.82 | 15.03 | 0.65 |
| 10| 2062 | 53.94 | 21.78 | 0.83 |
| 28| 4927 | 98.29 | 46.16 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5780 | 27.16 | 9.10 | 0.69 |
| 2| 5895 | 32.49 | 10.85 | 0.75 |
| 3| 5996 | 43.88 | 14.75 | 0.87 |
| 4| 6240 | 53.77 | 18.10 | 0.99 |
| 5| 6422 | 63.61 | 21.50 | 1.10 |
| 6| 6489 | 73.26 | 24.64 | 1.21 |
| 7| 6804 | 82.10 | 27.74 | 1.31 |
| 8| 6842 | 89.50 | 30.17 | 1.39 |
| 9| 6876 | 94.43 | 31.73 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 10 | 569 | 6173 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1139 | 6513 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1709 | 6855 | 80.22 | 30.52 | 1.32 |
| 10 | 39 | 2223 | 7162 | 97.61 | 37.43 | 1.52 |

