--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-17 15:43:07.642096634 UTC |
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
| 1| 5837 | 10.19 | 3.22 | 0.51 |
| 2| 6038 | 12.63 | 4.00 | 0.55 |
| 3| 6238 | 14.50 | 4.58 | 0.57 |
| 5| 6638 | 18.90 | 5.97 | 0.64 |
| 10| 7644 | 29.19 | 9.21 | 0.79 |
| 43| 14279 | 98.95 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 915 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2182 | 12.13 | 7.25 | 0.40 |
| 54| 10069 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 32.27 | 9.39 | 0.51 |
| 3 | 170 | 747 | 43.84 | 12.55 | 0.63 |
| 4 | 226 | 862 | 47.82 | 13.92 | 0.67 |
| 5 | 283 | 969 | 64.31 | 18.27 | 0.84 |
| 6 | 338 | 1081 | 68.31 | 19.66 | 0.89 |
| 7 | 395 | 1192 | 76.25 | 21.88 | 0.98 |
| 8 | 448 | 1303 | 92.24 | 26.20 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 23.92 | 7.60 | 0.48 |
| 2| 1882 | 24.77 | 8.48 | 0.49 |
| 3| 2141 | 28.13 | 10.10 | 0.54 |
| 5| 2335 | 30.26 | 12.02 | 0.58 |
| 10| 3207 | 41.58 | 18.53 | 0.76 |
| 39| 7446 | 96.97 | 53.24 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.81 | 7.37 | 0.42 |
| 2| 739 | 23.65 | 8.24 | 0.43 |
| 3| 893 | 25.55 | 9.49 | 0.46 |
| 5| 1295 | 31.66 | 12.53 | 0.55 |
| 10| 1902 | 37.65 | 17.49 | 0.66 |
| 42| 6681 | 99.23 | 55.95 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 27.54 | 8.47 | 0.46 |
| 2| 815 | 29.15 | 9.59 | 0.49 |
| 3| 920 | 32.72 | 11.23 | 0.54 |
| 5| 1231 | 34.33 | 13.03 | 0.58 |
| 10| 2121 | 45.83 | 19.66 | 0.76 |
| 37| 5990 | 97.23 | 52.02 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 706 | 33.87 | 10.16 | 0.53 |
| 2| 809 | 35.85 | 11.38 | 0.56 |
| 3| 889 | 37.16 | 12.39 | 0.58 |
| 5| 1244 | 42.57 | 15.26 | 0.66 |
| 10| 2024 | 53.94 | 21.78 | 0.83 |
| 28| 4833 | 97.67 | 45.98 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5791 | 27.13 | 9.11 | 0.69 |
| 2| 5822 | 31.60 | 10.51 | 0.74 |
| 3| 6036 | 41.56 | 13.90 | 0.85 |
| 4| 6241 | 54.77 | 18.49 | 1.00 |
| 5| 6655 | 67.59 | 22.87 | 1.15 |
| 6| 6616 | 74.39 | 25.11 | 1.22 |
| 7| 6614 | 78.69 | 26.42 | 1.27 |
| 8| 7043 | 96.54 | 32.64 | 1.48 |
| 9| 6681 | 84.89 | 28.31 | 1.33 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 5 | 284 | 6003 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 569 | 6173 | 40.76 | 14.88 | 0.86 |
| 10 | 20 | 1138 | 6513 | 59.28 | 22.29 | 1.08 |
| 10 | 30 | 1710 | 6856 | 79.78 | 30.37 | 1.32 |
| 10 | 38 | 2162 | 7125 | 97.51 | 37.29 | 1.52 |

