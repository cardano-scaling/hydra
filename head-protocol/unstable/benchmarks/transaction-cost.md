--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-01 04:42:44.639607081 UTC |
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
| 1| 5837 | 10.17 | 3.22 | 0.51 |
| 2| 6035 | 12.32 | 3.89 | 0.54 |
| 3| 6236 | 14.38 | 4.54 | 0.57 |
| 5| 6640 | 18.83 | 5.95 | 0.64 |
| 10| 7647 | 29.18 | 9.20 | 0.79 |
| 43| 14285 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1281 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.17 | 9.59 | 0.52 |
| 3 | 170 | 751 | 42.27 | 12.15 | 0.61 |
| 4 | 228 | 858 | 50.94 | 14.65 | 0.71 |
| 5 | 284 | 969 | 62.88 | 17.96 | 0.83 |
| 6 | 338 | 1081 | 64.31 | 18.67 | 0.85 |
| 7 | 394 | 1192 | 81.30 | 23.23 | 1.03 |
| 8 | 451 | 1303 | 83.13 | 23.98 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 24.00 | 7.62 | 0.48 |
| 2| 1891 | 24.77 | 8.48 | 0.49 |
| 3| 2014 | 25.98 | 9.50 | 0.52 |
| 5| 2279 | 28.89 | 11.65 | 0.57 |
| 10| 3101 | 39.64 | 17.98 | 0.74 |
| 42| 7850 | 99.77 | 56.05 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 644 | 22.57 | 7.33 | 0.41 |
| 2| 846 | 25.37 | 8.75 | 0.46 |
| 3| 947 | 26.87 | 9.84 | 0.48 |
| 5| 1265 | 30.93 | 12.32 | 0.54 |
| 10| 1907 | 38.18 | 17.65 | 0.67 |
| 43| 6789 | 99.06 | 56.59 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 650 | 29.17 | 8.91 | 0.48 |
| 2| 770 | 28.47 | 9.38 | 0.48 |
| 3| 945 | 30.94 | 10.75 | 0.52 |
| 5| 1134 | 35.71 | 13.37 | 0.59 |
| 10| 2085 | 48.07 | 20.24 | 0.78 |
| 38| 6000 | 98.85 | 53.08 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 799 | 35.85 | 11.38 | 0.56 |
| 3| 1071 | 39.26 | 13.03 | 0.61 |
| 5| 1203 | 41.89 | 15.05 | 0.65 |
| 10| 2001 | 53.26 | 21.57 | 0.82 |
| 28| 4958 | 99.29 | 46.46 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5792 | 27.13 | 9.09 | 0.69 |
| 2| 5998 | 37.01 | 12.47 | 0.80 |
| 3| 6065 | 44.53 | 14.98 | 0.88 |
| 4| 6151 | 50.57 | 16.95 | 0.95 |
| 5| 6367 | 64.95 | 21.83 | 1.11 |
| 6| 6542 | 70.51 | 23.75 | 1.18 |
| 7| 6691 | 81.52 | 27.45 | 1.30 |
| 8| 6920 | 93.05 | 31.36 | 1.43 |
| 9| 6765 | 92.99 | 31.19 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 570 | 6174 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1137 | 6512 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1706 | 6852 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2218 | 7157 | 98.05 | 37.58 | 1.53 |

