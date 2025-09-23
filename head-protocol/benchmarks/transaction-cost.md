--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-23 06:29:36.771799464 UTC |
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
| 2| 6039 | 12.61 | 4.00 | 0.55 |
| 3| 6238 | 14.97 | 4.75 | 0.58 |
| 5| 6643 | 19.17 | 6.07 | 0.64 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14279 | 99.02 | 30.95 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2167 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 640 | 34.38 | 9.91 | 0.53 |
| 3 | 169 | 747 | 43.73 | 12.51 | 0.63 |
| 4 | 225 | 858 | 49.73 | 14.35 | 0.69 |
| 5 | 282 | 974 | 62.28 | 17.78 | 0.82 |
| 6 | 337 | 1081 | 73.23 | 20.84 | 0.94 |
| 7 | 393 | 1192 | 72.74 | 21.13 | 0.94 |
| 8 | 449 | 1307 | 98.04 | 27.59 | 1.20 |
| 9 | 504 | 1414 | 88.74 | 25.72 | 1.11 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1746 | 23.30 | 7.41 | 0.47 |
| 2| 1997 | 26.42 | 8.96 | 0.52 |
| 3| 2194 | 29.42 | 10.45 | 0.56 |
| 5| 2423 | 31.76 | 12.46 | 0.60 |
| 10| 3109 | 40.00 | 18.07 | 0.74 |
| 40| 7628 | 97.22 | 53.96 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 658 | 22.81 | 7.38 | 0.42 |
| 2| 746 | 23.65 | 8.25 | 0.43 |
| 3| 899 | 25.10 | 9.32 | 0.46 |
| 5| 1213 | 29.08 | 11.77 | 0.52 |
| 10| 2075 | 40.79 | 18.38 | 0.70 |
| 42| 6750 | 98.14 | 55.69 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 657 | 29.17 | 8.91 | 0.48 |
| 2| 855 | 29.89 | 9.82 | 0.50 |
| 3| 916 | 32.76 | 11.24 | 0.54 |
| 5| 1236 | 34.30 | 13.02 | 0.58 |
| 10| 1992 | 44.08 | 19.12 | 0.73 |
| 37| 6053 | 98.64 | 52.44 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 33.87 | 10.16 | 0.53 |
| 2| 773 | 35.17 | 11.17 | 0.55 |
| 3| 942 | 37.91 | 12.62 | 0.59 |
| 5| 1359 | 43.40 | 15.50 | 0.67 |
| 10| 1984 | 53.97 | 21.79 | 0.83 |
| 31| 4918 | 99.70 | 48.40 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5788 | 26.97 | 9.05 | 0.69 |
| 2| 5972 | 36.03 | 12.10 | 0.79 |
| 3| 6201 | 46.92 | 15.86 | 0.92 |
| 4| 6251 | 55.14 | 18.55 | 1.00 |
| 5| 6393 | 60.52 | 20.36 | 1.07 |
| 6| 6470 | 71.54 | 24.01 | 1.19 |
| 7| 6620 | 79.60 | 26.72 | 1.28 |
| 8| 6777 | 89.11 | 30.03 | 1.38 |
| 9| 6837 | 89.93 | 30.12 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 284 | 6003 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1138 | 6512 | 59.73 | 22.44 | 1.08 |
| 10 | 30 | 1710 | 6856 | 80.22 | 30.52 | 1.32 |
| 10 | 39 | 2219 | 7159 | 99.82 | 38.19 | 1.55 |

