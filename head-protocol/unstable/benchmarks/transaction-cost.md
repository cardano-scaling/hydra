--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-27 10:03:48.113236155 UTC |
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
| 1| 5834 | 11.04 | 3.52 | 0.52 |
| 2| 6037 | 12.46 | 3.94 | 0.55 |
| 3| 6238 | 14.29 | 4.51 | 0.57 |
| 5| 6641 | 19.08 | 6.04 | 0.64 |
| 10| 7647 | 29.14 | 9.19 | 0.79 |
| 43| 14281 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 170 | 751 | 43.84 | 12.55 | 0.63 |
| 4 | 225 | 858 | 48.23 | 13.99 | 0.68 |
| 5 | 283 | 969 | 57.69 | 16.68 | 0.78 |
| 6 | 336 | 1081 | 69.49 | 19.87 | 0.90 |
| 7 | 395 | 1192 | 78.28 | 22.37 | 1.00 |
| 8 | 452 | 1303 | 88.11 | 25.27 | 1.10 |
| 9 | 506 | 1414 | 95.09 | 27.41 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1796 | 24.29 | 7.69 | 0.48 |
| 2| 1930 | 25.55 | 8.72 | 0.50 |
| 3| 2109 | 28.47 | 10.18 | 0.55 |
| 5| 2276 | 29.30 | 11.75 | 0.57 |
| 10| 3092 | 39.56 | 17.96 | 0.74 |
| 37| 7166 | 91.75 | 50.46 | 1.57 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.77 | 7.36 | 0.41 |
| 2| 822 | 24.97 | 8.65 | 0.45 |
| 3| 901 | 25.03 | 9.30 | 0.46 |
| 5| 1258 | 29.82 | 12.00 | 0.53 |
| 10| 2094 | 41.52 | 18.60 | 0.71 |
| 41| 6470 | 95.04 | 54.11 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 656 | 29.09 | 8.89 | 0.48 |
| 2| 854 | 31.66 | 10.29 | 0.52 |
| 3| 960 | 33.36 | 11.43 | 0.54 |
| 5| 1296 | 35.72 | 13.46 | 0.59 |
| 10| 1857 | 45.34 | 19.40 | 0.74 |
| 34| 5484 | 91.51 | 48.41 | 1.48 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.16 | 0.53 |
| 2| 883 | 36.52 | 11.59 | 0.57 |
| 3| 933 | 37.88 | 12.61 | 0.59 |
| 5| 1195 | 41.89 | 15.05 | 0.65 |
| 10| 1963 | 53.46 | 21.62 | 0.82 |
| 28| 4825 | 97.36 | 45.87 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 27.05 | 9.07 | 0.69 |
| 2| 5986 | 37.00 | 12.46 | 0.80 |
| 3| 6080 | 45.08 | 15.14 | 0.89 |
| 4| 6343 | 55.83 | 18.85 | 1.02 |
| 5| 6505 | 65.27 | 22.02 | 1.12 |
| 6| 6564 | 71.25 | 24.06 | 1.19 |
| 7| 6780 | 84.34 | 28.46 | 1.34 |
| 8| 6902 | 89.98 | 30.44 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 285 | 6004 | 29.09 | 10.34 | 0.72 |
| 10 | 10 | 568 | 6173 | 39.25 | 14.36 | 0.84 |
| 10 | 20 | 1139 | 6513 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1707 | 6854 | 79.78 | 30.37 | 1.32 |
| 10 | 40 | 2278 | 7194 | 99.66 | 38.24 | 1.55 |
| 10 | 39 | 2223 | 7162 | 99.38 | 38.04 | 1.54 |

