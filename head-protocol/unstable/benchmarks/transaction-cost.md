--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-03 04:40:36.519635295 UTC |
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
| 1| 5836 | 10.38 | 3.29 | 0.51 |
| 2| 6038 | 12.63 | 4.00 | 0.55 |
| 3| 6240 | 14.50 | 4.58 | 0.58 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7644 | 29.47 | 9.30 | 0.79 |
| 43| 14281 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 640 | 34.23 | 9.85 | 0.53 |
| 3 | 171 | 747 | 42.23 | 12.15 | 0.61 |
| 4 | 226 | 858 | 51.97 | 14.91 | 0.72 |
| 5 | 284 | 969 | 59.08 | 16.98 | 0.79 |
| 6 | 338 | 1081 | 73.76 | 20.97 | 0.94 |
| 7 | 393 | 1192 | 83.85 | 23.70 | 1.05 |
| 8 | 449 | 1303 | 80.52 | 23.35 | 1.03 |
| 10 | 560 | 1525 | 98.79 | 28.66 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 24.37 | 7.71 | 0.48 |
| 2| 1880 | 24.80 | 8.49 | 0.49 |
| 3| 2128 | 28.39 | 10.16 | 0.55 |
| 5| 2367 | 31.44 | 12.35 | 0.60 |
| 10| 3161 | 40.93 | 18.34 | 0.75 |
| 38| 7357 | 95.63 | 52.23 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 650 | 22.54 | 7.31 | 0.41 |
| 2| 758 | 24.08 | 8.40 | 0.44 |
| 3| 948 | 26.76 | 9.81 | 0.48 |
| 5| 1154 | 28.84 | 11.73 | 0.52 |
| 10| 2078 | 41.05 | 18.46 | 0.71 |
| 42| 6803 | 99.95 | 56.18 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 702 | 27.54 | 8.47 | 0.46 |
| 2| 814 | 29.22 | 9.61 | 0.49 |
| 3| 962 | 33.51 | 11.47 | 0.55 |
| 5| 1248 | 36.90 | 13.75 | 0.60 |
| 10| 1949 | 44.26 | 19.17 | 0.73 |
| 36| 6052 | 98.19 | 51.67 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 683 | 33.83 | 10.15 | 0.53 |
| 2| 818 | 35.92 | 11.40 | 0.56 |
| 3| 899 | 37.24 | 12.41 | 0.58 |
| 5| 1301 | 43.28 | 15.48 | 0.67 |
| 10| 2182 | 56.07 | 22.43 | 0.86 |
| 28| 4673 | 95.05 | 45.19 | 1.45 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5813 | 26.96 | 9.06 | 0.69 |
| 2| 5965 | 35.96 | 12.07 | 0.79 |
| 3| 5971 | 37.98 | 12.63 | 0.81 |
| 4| 6293 | 54.91 | 18.52 | 1.00 |
| 5| 6399 | 64.46 | 21.67 | 1.11 |
| 6| 6662 | 75.44 | 25.49 | 1.24 |
| 7| 6877 | 85.60 | 28.88 | 1.35 |
| 8| 6930 | 89.96 | 30.32 | 1.40 |
| 9| 7072 | 99.15 | 33.49 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 283 | 6002 | 29.79 | 10.58 | 0.73 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6855 | 78.27 | 29.85 | 1.30 |
| 10 | 39 | 2220 | 7159 | 98.49 | 37.73 | 1.53 |

