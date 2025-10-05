--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-05 04:38:15.17160959 UTC |
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
| 1| 5837 | 10.76 | 3.42 | 0.52 |
| 2| 6039 | 12.32 | 3.89 | 0.54 |
| 3| 6236 | 14.48 | 4.58 | 0.57 |
| 5| 6640 | 18.81 | 5.94 | 0.64 |
| 10| 7644 | 28.92 | 9.11 | 0.79 |
| 43| 14282 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2169 | 12.13 | 7.25 | 0.40 |
| 54| 10075 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.62 | 0.52 |
| 3 | 169 | 747 | 42.38 | 12.19 | 0.61 |
| 4 | 225 | 862 | 49.55 | 14.29 | 0.69 |
| 5 | 282 | 969 | 62.62 | 17.89 | 0.83 |
| 6 | 341 | 1081 | 74.77 | 21.13 | 0.95 |
| 7 | 395 | 1196 | 86.79 | 24.54 | 1.08 |
| 8 | 450 | 1303 | 96.75 | 27.24 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1801 | 24.37 | 7.71 | 0.48 |
| 2| 1989 | 26.88 | 9.08 | 0.52 |
| 3| 2186 | 29.54 | 10.48 | 0.56 |
| 5| 2349 | 30.04 | 11.97 | 0.58 |
| 10| 3215 | 42.70 | 18.84 | 0.77 |
| 40| 7680 | 98.61 | 54.38 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 22.77 | 7.36 | 0.41 |
| 2| 828 | 25.16 | 8.71 | 0.45 |
| 3| 927 | 26.94 | 9.85 | 0.48 |
| 5| 1251 | 30.09 | 12.07 | 0.54 |
| 10| 1792 | 36.35 | 17.14 | 0.65 |
| 39| 6402 | 93.00 | 52.25 | 1.56 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 653 | 29.13 | 8.90 | 0.48 |
| 2| 821 | 29.18 | 9.60 | 0.49 |
| 3| 988 | 33.51 | 11.47 | 0.55 |
| 5| 1255 | 35.08 | 13.26 | 0.58 |
| 10| 2021 | 48.30 | 20.29 | 0.78 |
| 36| 5953 | 98.07 | 51.68 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 33.79 | 10.15 | 0.53 |
| 2| 819 | 35.85 | 11.38 | 0.56 |
| 3| 1056 | 39.34 | 13.05 | 0.61 |
| 5| 1224 | 41.97 | 15.07 | 0.65 |
| 10| 1983 | 53.23 | 21.56 | 0.82 |
| 30| 4917 | 99.24 | 47.66 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 27.00 | 9.07 | 0.69 |
| 2| 5990 | 36.81 | 12.42 | 0.80 |
| 3| 5969 | 40.35 | 13.44 | 0.84 |
| 4| 6257 | 52.26 | 17.61 | 0.97 |
| 5| 6494 | 66.06 | 22.38 | 1.13 |
| 6| 6600 | 74.18 | 25.03 | 1.22 |
| 7| 6758 | 85.35 | 28.82 | 1.34 |
| 8| 6908 | 93.48 | 31.54 | 1.44 |
| 9| 6886 | 95.46 | 32.16 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5869 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6005 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6174 | 40.39 | 14.75 | 0.85 |
| 10 | 39 | 2220 | 7159 | 98.93 | 37.88 | 1.54 |

