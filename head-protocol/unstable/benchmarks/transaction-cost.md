--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-28 17:35:20.161768188 UTC |
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
| 2| 6038 | 12.92 | 4.11 | 0.55 |
| 3| 6238 | 14.71 | 4.65 | 0.58 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7644 | 28.94 | 9.11 | 0.79 |
| 43| 14281 | 99.40 | 31.09 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10075 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 32.31 | 9.40 | 0.51 |
| 3 | 171 | 747 | 41.38 | 11.95 | 0.60 |
| 4 | 225 | 858 | 49.70 | 14.35 | 0.69 |
| 5 | 281 | 969 | 60.87 | 17.44 | 0.81 |
| 6 | 340 | 1081 | 68.99 | 19.75 | 0.90 |
| 7 | 397 | 1192 | 71.99 | 20.86 | 0.93 |
| 8 | 453 | 1303 | 87.59 | 25.09 | 1.10 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1796 | 24.37 | 7.71 | 0.48 |
| 2| 1951 | 25.85 | 8.78 | 0.51 |
| 3| 2069 | 27.31 | 9.86 | 0.53 |
| 5| 2362 | 30.92 | 12.22 | 0.59 |
| 10| 3190 | 42.69 | 18.82 | 0.77 |
| 41| 7670 | 97.05 | 54.59 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 620 | 22.53 | 7.32 | 0.41 |
| 2| 852 | 25.36 | 8.75 | 0.46 |
| 3| 853 | 24.07 | 9.03 | 0.45 |
| 5| 1151 | 28.18 | 11.51 | 0.51 |
| 10| 1892 | 38.02 | 17.64 | 0.67 |
| 42| 6751 | 98.22 | 55.68 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 29.13 | 8.90 | 0.48 |
| 2| 882 | 29.86 | 9.81 | 0.50 |
| 3| 1008 | 31.65 | 10.97 | 0.53 |
| 5| 1347 | 38.44 | 14.21 | 0.62 |
| 10| 1979 | 44.11 | 19.13 | 0.73 |
| 36| 5966 | 97.98 | 51.60 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.83 | 10.16 | 0.53 |
| 2| 826 | 35.92 | 11.40 | 0.56 |
| 3| 1024 | 38.58 | 12.82 | 0.60 |
| 5| 1196 | 41.78 | 15.02 | 0.65 |
| 10| 2013 | 53.75 | 21.73 | 0.83 |
| 29| 4960 | 99.56 | 47.15 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5796 | 26.97 | 9.07 | 0.69 |
| 2| 5870 | 34.95 | 11.71 | 0.78 |
| 3| 6083 | 42.46 | 14.25 | 0.86 |
| 4| 6182 | 51.26 | 17.26 | 0.96 |
| 5| 6499 | 65.53 | 22.10 | 1.12 |
| 6| 6414 | 66.01 | 22.12 | 1.13 |
| 7| 6678 | 79.92 | 26.85 | 1.28 |
| 8| 6910 | 92.77 | 31.29 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 30 | 1706 | 6853 | 80.67 | 30.67 | 1.32 |
| 10 | 39 | 2219 | 7158 | 98.93 | 37.88 | 1.54 |

