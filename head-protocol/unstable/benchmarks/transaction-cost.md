--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-17 04:44:49.497905092 UTC |
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
| 1| 5836 | 10.40 | 3.30 | 0.51 |
| 2| 6041 | 12.23 | 3.86 | 0.54 |
| 3| 6239 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 18.58 | 5.86 | 0.63 |
| 10| 7647 | 28.92 | 9.11 | 0.79 |
| 43| 14285 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.18 | 9.60 | 0.52 |
| 3 | 170 | 747 | 43.96 | 12.60 | 0.63 |
| 4 | 227 | 858 | 53.72 | 15.31 | 0.73 |
| 5 | 283 | 974 | 62.31 | 17.73 | 0.82 |
| 6 | 338 | 1081 | 73.77 | 20.94 | 0.94 |
| 7 | 395 | 1196 | 84.16 | 23.73 | 1.05 |
| 8 | 449 | 1303 | 85.57 | 24.61 | 1.08 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1819 | 24.37 | 7.71 | 0.48 |
| 2| 1942 | 25.76 | 8.76 | 0.51 |
| 3| 2068 | 27.43 | 9.89 | 0.53 |
| 5| 2359 | 30.88 | 12.21 | 0.59 |
| 10| 3069 | 39.66 | 17.99 | 0.74 |
| 40| 7599 | 97.74 | 54.16 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 633 | 22.81 | 7.37 | 0.42 |
| 2| 782 | 23.98 | 8.37 | 0.44 |
| 3| 873 | 25.12 | 9.34 | 0.46 |
| 5| 1289 | 30.79 | 12.27 | 0.54 |
| 10| 1947 | 38.46 | 17.73 | 0.67 |
| 40| 6420 | 94.39 | 53.29 | 1.58 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 26.83 | 8.26 | 0.45 |
| 2| 774 | 28.47 | 9.38 | 0.48 |
| 3| 980 | 33.47 | 11.45 | 0.55 |
| 5| 1272 | 37.02 | 13.78 | 0.60 |
| 10| 2024 | 48.23 | 20.27 | 0.78 |
| 37| 6139 | 99.90 | 52.81 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 33.87 | 10.16 | 0.53 |
| 2| 844 | 36.64 | 11.62 | 0.57 |
| 3| 1002 | 38.59 | 12.82 | 0.60 |
| 5| 1158 | 41.29 | 14.86 | 0.64 |
| 10| 2087 | 54.78 | 22.02 | 0.84 |
| 29| 4918 | 99.04 | 47.00 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5790 | 27.00 | 9.06 | 0.69 |
| 2| 5820 | 31.45 | 10.45 | 0.74 |
| 3| 6015 | 43.83 | 14.69 | 0.88 |
| 4| 6317 | 55.02 | 18.53 | 1.01 |
| 5| 6380 | 60.32 | 20.24 | 1.06 |
| 6| 6579 | 72.74 | 24.52 | 1.20 |
| 7| 6694 | 77.95 | 26.17 | 1.26 |
| 8| 7015 | 95.04 | 32.13 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.49 | 6.17 | 0.60 |
| 10 | 1 | 57 | 5868 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 283 | 6002 | 29.53 | 10.50 | 0.73 |
| 10 | 10 | 569 | 6173 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1138 | 6512 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1708 | 6854 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2224 | 7164 | 99.38 | 38.04 | 1.54 |

