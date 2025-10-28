--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 13:46:57.197420686 UTC |
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
| 1| 5837 | 10.93 | 3.49 | 0.52 |
| 2| 6035 | 12.44 | 3.94 | 0.54 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 18.93 | 5.98 | 0.64 |
| 10| 7644 | 29.14 | 9.19 | 0.79 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 736 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2166 | 12.13 | 7.25 | 0.40 |
| 54| 10060 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 171 | 747 | 41.39 | 11.95 | 0.60 |
| 4 | 225 | 858 | 51.05 | 14.67 | 0.71 |
| 5 | 282 | 969 | 62.52 | 17.81 | 0.83 |
| 6 | 337 | 1085 | 69.59 | 19.93 | 0.90 |
| 7 | 396 | 1192 | 80.99 | 23.11 | 1.02 |
| 8 | 450 | 1307 | 86.07 | 24.78 | 1.08 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1800 | 24.29 | 7.69 | 0.48 |
| 2| 1975 | 26.76 | 9.04 | 0.52 |
| 3| 2110 | 27.93 | 10.05 | 0.54 |
| 5| 2317 | 29.97 | 11.95 | 0.58 |
| 10| 3108 | 39.72 | 18.00 | 0.74 |
| 42| 7810 | 99.39 | 55.92 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.57 | 7.33 | 0.41 |
| 2| 741 | 23.62 | 8.24 | 0.43 |
| 3| 830 | 24.13 | 9.05 | 0.45 |
| 5| 1254 | 31.19 | 12.37 | 0.55 |
| 10| 2034 | 41.05 | 18.45 | 0.70 |
| 40| 6375 | 94.65 | 53.34 | 1.58 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 597 | 28.46 | 8.69 | 0.47 |
| 2| 887 | 29.90 | 9.82 | 0.50 |
| 3| 917 | 32.80 | 11.25 | 0.54 |
| 5| 1216 | 34.26 | 13.01 | 0.57 |
| 10| 2073 | 45.64 | 19.59 | 0.75 |
| 36| 5903 | 97.40 | 51.40 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 671 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.17 | 11.17 | 0.55 |
| 3| 892 | 37.16 | 12.39 | 0.58 |
| 5| 1282 | 42.57 | 15.26 | 0.66 |
| 10| 2081 | 54.88 | 22.05 | 0.84 |
| 29| 5009 | 99.88 | 47.25 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5836 | 27.08 | 9.09 | 0.69 |
| 2| 5961 | 35.88 | 12.04 | 0.79 |
| 3| 6038 | 41.25 | 13.81 | 0.85 |
| 4| 6119 | 49.33 | 16.48 | 0.94 |
| 5| 6439 | 65.16 | 21.98 | 1.12 |
| 6| 6393 | 62.24 | 20.85 | 1.08 |
| 7| 6661 | 79.93 | 26.96 | 1.28 |
| 8| 6923 | 91.56 | 30.93 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.21 | 10.04 | 0.71 |
| 10 | 10 | 568 | 6172 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1139 | 6514 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1706 | 6853 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2222 | 7161 | 98.93 | 37.88 | 1.54 |

