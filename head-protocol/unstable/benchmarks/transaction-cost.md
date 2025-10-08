--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-08 17:38:51.151183346 UTC |
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
| 1| 5837 | 10.38 | 3.29 | 0.51 |
| 2| 6037 | 12.46 | 3.94 | 0.55 |
| 3| 6239 | 14.48 | 4.58 | 0.57 |
| 5| 6640 | 18.90 | 5.97 | 0.64 |
| 10| 7646 | 28.80 | 9.07 | 0.78 |
| 43| 14279 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10061 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 33.25 | 9.63 | 0.52 |
| 3 | 171 | 747 | 42.73 | 12.29 | 0.62 |
| 4 | 226 | 862 | 49.55 | 14.31 | 0.69 |
| 5 | 283 | 969 | 56.53 | 16.38 | 0.77 |
| 6 | 338 | 1081 | 66.36 | 19.16 | 0.87 |
| 7 | 394 | 1192 | 82.90 | 23.57 | 1.04 |
| 8 | 450 | 1303 | 93.12 | 26.52 | 1.15 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1814 | 24.37 | 7.71 | 0.48 |
| 2| 1922 | 25.76 | 8.76 | 0.51 |
| 3| 2059 | 27.32 | 9.86 | 0.53 |
| 5| 2364 | 30.88 | 12.21 | 0.59 |
| 10| 3086 | 39.40 | 17.92 | 0.73 |
| 40| 7718 | 98.91 | 54.46 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.81 | 7.37 | 0.42 |
| 2| 699 | 22.62 | 7.97 | 0.42 |
| 3| 941 | 26.88 | 9.84 | 0.48 |
| 5| 1261 | 31.37 | 12.42 | 0.55 |
| 10| 2088 | 41.88 | 18.70 | 0.72 |
| 41| 6622 | 98.06 | 54.98 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 653 | 29.17 | 8.91 | 0.48 |
| 2| 800 | 30.94 | 10.07 | 0.51 |
| 3| 958 | 30.86 | 10.73 | 0.52 |
| 5| 1344 | 35.76 | 13.47 | 0.60 |
| 10| 2015 | 47.09 | 19.95 | 0.76 |
| 34| 5655 | 98.86 | 50.43 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 694 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.17 | 11.17 | 0.55 |
| 3| 938 | 37.80 | 12.59 | 0.59 |
| 5| 1295 | 42.83 | 15.34 | 0.66 |
| 10| 1948 | 53.19 | 21.55 | 0.82 |
| 29| 4743 | 96.50 | 46.24 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5816 | 26.96 | 9.05 | 0.69 |
| 2| 5937 | 36.03 | 12.10 | 0.79 |
| 3| 6093 | 45.01 | 15.13 | 0.89 |
| 4| 6235 | 52.45 | 17.67 | 0.98 |
| 5| 6277 | 59.31 | 19.90 | 1.05 |
| 6| 6637 | 75.04 | 25.37 | 1.23 |
| 7| 6801 | 83.37 | 28.14 | 1.33 |
| 8| 6947 | 93.29 | 31.59 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 5 | 283 | 6003 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 571 | 6175 | 39.06 | 14.30 | 0.84 |
| 10 | 30 | 1708 | 6854 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2220 | 7160 | 98.05 | 37.58 | 1.53 |

