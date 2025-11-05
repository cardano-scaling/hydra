--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-05 04:49:14.28700338 UTC |
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
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6238 | 14.88 | 4.72 | 0.58 |
| 5| 6645 | 19.19 | 6.08 | 0.64 |
| 10| 7647 | 28.73 | 9.04 | 0.78 |
| 43| 14281 | 98.94 | 30.92 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10051 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.19 | 9.36 | 0.51 |
| 3 | 170 | 747 | 43.79 | 12.52 | 0.63 |
| 4 | 226 | 858 | 51.99 | 14.89 | 0.72 |
| 5 | 285 | 969 | 56.30 | 16.32 | 0.77 |
| 6 | 337 | 1085 | 63.89 | 18.53 | 0.85 |
| 7 | 393 | 1192 | 78.39 | 22.40 | 1.00 |
| 8 | 450 | 1303 | 94.21 | 26.68 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1819 | 24.29 | 7.69 | 0.48 |
| 2| 1880 | 24.40 | 8.40 | 0.49 |
| 3| 2013 | 25.95 | 9.49 | 0.52 |
| 5| 2373 | 31.29 | 12.33 | 0.60 |
| 10| 3158 | 40.38 | 18.20 | 0.75 |
| 41| 7706 | 99.64 | 55.32 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 22.80 | 7.36 | 0.41 |
| 2| 794 | 23.59 | 8.23 | 0.44 |
| 3| 1035 | 28.33 | 10.24 | 0.50 |
| 5| 1234 | 29.82 | 11.99 | 0.53 |
| 10| 1915 | 37.52 | 17.46 | 0.66 |
| 42| 6732 | 99.76 | 56.10 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 29.13 | 8.90 | 0.48 |
| 2| 804 | 29.22 | 9.61 | 0.49 |
| 3| 976 | 33.05 | 11.33 | 0.54 |
| 5| 1376 | 36.51 | 13.69 | 0.60 |
| 10| 1903 | 43.40 | 18.92 | 0.72 |
| 38| 6067 | 98.94 | 53.19 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 33.83 | 10.16 | 0.53 |
| 2| 906 | 36.52 | 11.59 | 0.57 |
| 3| 962 | 37.88 | 12.61 | 0.59 |
| 5| 1245 | 42.57 | 15.26 | 0.66 |
| 10| 1860 | 52.04 | 21.19 | 0.81 |
| 30| 4961 | 99.01 | 47.60 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5807 | 27.13 | 9.11 | 0.69 |
| 2| 5907 | 35.80 | 12.04 | 0.79 |
| 3| 6133 | 44.87 | 15.06 | 0.89 |
| 4| 6211 | 53.95 | 18.13 | 0.99 |
| 5| 6327 | 60.30 | 20.27 | 1.06 |
| 6| 6418 | 68.29 | 22.91 | 1.15 |
| 7| 6727 | 81.06 | 27.26 | 1.30 |
| 8| 6932 | 94.28 | 31.80 | 1.45 |
| 9| 7023 | 98.66 | 33.26 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5869 | 19.45 | 6.61 | 0.61 |
| 10 | 20 | 1137 | 6512 | 59.54 | 22.38 | 1.08 |
| 10 | 38 | 2165 | 7127 | 95.56 | 36.62 | 1.50 |

