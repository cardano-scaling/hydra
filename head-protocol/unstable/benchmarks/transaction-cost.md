--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-05 10:00:34.986680561 UTC |
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
| 1| 5834 | 10.59 | 3.36 | 0.52 |
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6236 | 14.38 | 4.54 | 0.57 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7644 | 28.71 | 9.03 | 0.78 |
| 43| 14286 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10052 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.17 | 9.59 | 0.52 |
| 3 | 170 | 747 | 43.55 | 12.48 | 0.62 |
| 4 | 227 | 858 | 53.66 | 15.27 | 0.73 |
| 5 | 282 | 974 | 57.78 | 16.70 | 0.78 |
| 6 | 340 | 1081 | 73.87 | 20.96 | 0.95 |
| 7 | 393 | 1192 | 86.43 | 24.32 | 1.08 |
| 8 | 449 | 1303 | 96.87 | 27.37 | 1.19 |
| 9 | 505 | 1414 | 96.28 | 27.58 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1794 | 24.29 | 7.69 | 0.48 |
| 2| 1920 | 25.51 | 8.70 | 0.50 |
| 3| 2073 | 26.86 | 9.75 | 0.53 |
| 5| 2399 | 30.91 | 12.22 | 0.59 |
| 10| 3118 | 40.55 | 18.24 | 0.75 |
| 42| 7743 | 99.62 | 55.97 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 630 | 22.81 | 7.37 | 0.42 |
| 2| 788 | 24.01 | 8.39 | 0.44 |
| 3| 830 | 24.06 | 9.03 | 0.45 |
| 5| 1187 | 27.93 | 11.45 | 0.51 |
| 10| 1915 | 37.44 | 17.44 | 0.66 |
| 40| 6541 | 96.50 | 53.89 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 664 | 29.17 | 8.91 | 0.48 |
| 2| 801 | 30.98 | 10.08 | 0.51 |
| 3| 953 | 33.51 | 11.46 | 0.55 |
| 5| 1250 | 37.35 | 13.88 | 0.61 |
| 10| 2079 | 48.78 | 20.45 | 0.78 |
| 34| 5730 | 94.06 | 49.19 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.83 | 10.15 | 0.53 |
| 2| 864 | 36.60 | 11.61 | 0.57 |
| 3| 1081 | 39.26 | 13.03 | 0.61 |
| 5| 1301 | 43.28 | 15.48 | 0.67 |
| 10| 2267 | 56.94 | 22.68 | 0.87 |
| 30| 4824 | 98.24 | 47.39 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5789 | 27.13 | 9.11 | 0.69 |
| 2| 5909 | 35.93 | 12.05 | 0.79 |
| 3| 5947 | 40.51 | 13.52 | 0.84 |
| 4| 6159 | 50.29 | 16.85 | 0.95 |
| 5| 6350 | 64.54 | 21.73 | 1.11 |
| 6| 6427 | 69.14 | 23.27 | 1.16 |
| 7| 6678 | 82.56 | 27.75 | 1.31 |
| 8| 6938 | 94.33 | 31.87 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 10 | 569 | 6173 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1138 | 6513 | 59.98 | 22.53 | 1.08 |
| 10 | 38 | 2162 | 7124 | 96.88 | 37.08 | 1.51 |

