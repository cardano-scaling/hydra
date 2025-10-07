--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-07 16:00:03.595796302 UTC |
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
| 1| 5836 | 10.76 | 3.42 | 0.52 |
| 2| 6035 | 12.23 | 3.86 | 0.54 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6641 | 18.71 | 5.91 | 0.64 |
| 10| 7651 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10052 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.63 | 0.52 |
| 3 | 171 | 747 | 43.64 | 12.49 | 0.63 |
| 4 | 227 | 862 | 48.17 | 14.00 | 0.68 |
| 5 | 284 | 969 | 61.22 | 17.50 | 0.81 |
| 6 | 339 | 1081 | 64.48 | 18.75 | 0.85 |
| 7 | 394 | 1192 | 80.16 | 22.90 | 1.02 |
| 8 | 449 | 1303 | 82.09 | 23.62 | 1.04 |
| 9 | 505 | 1414 | 96.62 | 27.72 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1811 | 23.92 | 7.60 | 0.48 |
| 2| 1924 | 25.76 | 8.76 | 0.51 |
| 3| 2073 | 26.98 | 9.78 | 0.53 |
| 5| 2490 | 33.36 | 12.89 | 0.62 |
| 10| 3287 | 42.92 | 18.89 | 0.78 |
| 39| 7531 | 97.87 | 53.51 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 638 | 22.84 | 7.39 | 0.42 |
| 2| 830 | 24.97 | 8.64 | 0.45 |
| 3| 854 | 23.99 | 9.01 | 0.45 |
| 5| 1192 | 29.18 | 11.80 | 0.52 |
| 10| 1882 | 37.43 | 17.44 | 0.66 |
| 41| 6439 | 97.71 | 54.81 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 27.50 | 8.46 | 0.46 |
| 2| 774 | 30.94 | 10.07 | 0.51 |
| 3| 941 | 32.76 | 11.24 | 0.54 |
| 5| 1281 | 35.08 | 13.26 | 0.59 |
| 10| 2088 | 48.95 | 20.50 | 0.79 |
| 36| 6056 | 97.32 | 51.45 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 33.87 | 10.16 | 0.53 |
| 2| 878 | 36.48 | 11.58 | 0.57 |
| 3| 939 | 37.91 | 12.62 | 0.59 |
| 5| 1339 | 43.40 | 15.50 | 0.67 |
| 10| 2044 | 54.50 | 21.96 | 0.84 |
| 28| 4808 | 98.42 | 46.21 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5815 | 26.92 | 9.04 | 0.69 |
| 2| 5961 | 35.87 | 12.05 | 0.79 |
| 3| 6039 | 43.77 | 14.65 | 0.88 |
| 4| 6249 | 55.07 | 18.54 | 1.00 |
| 5| 6492 | 65.78 | 22.25 | 1.13 |
| 6| 6756 | 75.04 | 25.42 | 1.24 |
| 7| 6750 | 79.89 | 26.92 | 1.29 |
| 8| 6796 | 87.62 | 29.50 | 1.37 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 22.29 | 7.58 | 0.64 |
| 10 | 10 | 571 | 6175 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6512 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1708 | 6854 | 80.41 | 30.59 | 1.32 |
| 10 | 39 | 2222 | 7161 | 99.38 | 38.04 | 1.54 |

