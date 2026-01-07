--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-07 16:28:42.399471662 UTC |
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
| 1| 5837 | 10.59 | 3.36 | 0.52 |
| 2| 6035 | 12.23 | 3.86 | 0.54 |
| 3| 6236 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 18.62 | 5.87 | 0.64 |
| 10| 7647 | 29.11 | 9.17 | 0.79 |
| 43| 14281 | 99.13 | 30.99 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10044 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 171 | 747 | 40.12 | 11.65 | 0.59 |
| 4 | 225 | 858 | 50.45 | 14.52 | 0.70 |
| 5 | 283 | 969 | 62.09 | 17.71 | 0.82 |
| 6 | 340 | 1081 | 71.25 | 20.29 | 0.92 |
| 7 | 394 | 1192 | 82.87 | 23.51 | 1.04 |
| 8 | 450 | 1303 | 89.49 | 25.50 | 1.11 |
| 10 | 560 | 1529 | 98.41 | 28.56 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1784 | 24.29 | 7.69 | 0.48 |
| 2| 1941 | 25.76 | 8.76 | 0.51 |
| 3| 2172 | 29.09 | 10.37 | 0.56 |
| 5| 2373 | 30.88 | 12.21 | 0.59 |
| 10| 3176 | 40.98 | 18.37 | 0.75 |
| 40| 7350 | 95.77 | 53.57 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 609 | 22.57 | 7.31 | 0.41 |
| 2| 767 | 23.98 | 8.37 | 0.44 |
| 3| 1017 | 28.33 | 10.23 | 0.50 |
| 5| 1139 | 28.69 | 11.68 | 0.52 |
| 10| 2045 | 40.05 | 18.17 | 0.69 |
| 41| 6364 | 93.68 | 53.75 | 1.57 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 693 | 27.50 | 8.46 | 0.46 |
| 2| 894 | 29.82 | 9.80 | 0.50 |
| 3| 944 | 30.82 | 10.73 | 0.52 |
| 5| 1235 | 36.98 | 13.76 | 0.60 |
| 10| 1953 | 46.77 | 19.84 | 0.76 |
| 36| 6005 | 97.78 | 51.53 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.14 | 11.16 | 0.55 |
| 3| 892 | 37.20 | 12.40 | 0.58 |
| 5| 1249 | 42.53 | 15.25 | 0.66 |
| 10| 2096 | 54.50 | 21.96 | 0.84 |
| 28| 4718 | 96.70 | 45.70 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5818 | 26.96 | 9.06 | 0.69 |
| 2| 5999 | 37.00 | 12.49 | 0.80 |
| 3| 5946 | 40.33 | 13.45 | 0.83 |
| 4| 6256 | 53.60 | 18.04 | 0.99 |
| 5| 6352 | 60.50 | 20.30 | 1.06 |
| 6| 6707 | 76.67 | 25.89 | 1.25 |
| 7| 6751 | 83.88 | 28.22 | 1.33 |
| 8| 6979 | 94.43 | 31.95 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 284 | 6003 | 28.46 | 10.13 | 0.72 |
| 10 | 20 | 1140 | 6514 | 60.61 | 22.74 | 1.09 |
| 10 | 30 | 1704 | 6850 | 79.15 | 30.16 | 1.31 |
| 10 | 39 | 2218 | 7157 | 98.05 | 37.58 | 1.53 |

