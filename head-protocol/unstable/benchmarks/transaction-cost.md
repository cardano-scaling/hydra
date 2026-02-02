--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-02 10:41:58.613656094 UTC |
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
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6239 | 15.07 | 4.78 | 0.58 |
| 5| 6640 | 18.60 | 5.87 | 0.64 |
| 10| 7644 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2182 | 12.13 | 7.25 | 0.40 |
| 54| 10060 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 169 | 747 | 40.10 | 11.64 | 0.59 |
| 4 | 226 | 858 | 53.82 | 15.31 | 0.73 |
| 5 | 283 | 974 | 57.63 | 16.67 | 0.78 |
| 6 | 339 | 1081 | 64.63 | 18.75 | 0.86 |
| 7 | 395 | 1192 | 83.04 | 23.56 | 1.04 |
| 8 | 448 | 1303 | 88.76 | 25.27 | 1.11 |
| 10 | 561 | 1525 | 96.61 | 27.94 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.00 | 7.62 | 0.48 |
| 2| 1935 | 25.80 | 8.77 | 0.51 |
| 3| 2064 | 27.28 | 9.85 | 0.53 |
| 5| 2339 | 30.41 | 12.06 | 0.59 |
| 10| 3222 | 41.85 | 18.59 | 0.76 |
| 42| 7731 | 96.39 | 55.11 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 22.81 | 7.37 | 0.42 |
| 2| 818 | 25.13 | 8.69 | 0.45 |
| 3| 871 | 25.09 | 9.31 | 0.46 |
| 5| 1294 | 30.00 | 12.03 | 0.54 |
| 10| 1970 | 38.39 | 17.72 | 0.67 |
| 42| 6671 | 99.43 | 56.03 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 27.54 | 8.47 | 0.46 |
| 2| 860 | 31.61 | 10.27 | 0.52 |
| 3| 979 | 33.39 | 11.43 | 0.55 |
| 5| 1277 | 37.58 | 13.95 | 0.61 |
| 10| 2009 | 44.90 | 19.37 | 0.74 |
| 36| 5948 | 96.86 | 51.30 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 33.15 | 9.95 | 0.52 |
| 2| 819 | 35.92 | 11.40 | 0.56 |
| 3| 949 | 37.88 | 12.61 | 0.59 |
| 5| 1210 | 41.86 | 15.04 | 0.65 |
| 10| 1855 | 52.15 | 21.22 | 0.81 |
| 30| 4799 | 97.31 | 47.10 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5821 | 26.96 | 9.06 | 0.69 |
| 2| 5977 | 36.97 | 12.49 | 0.80 |
| 3| 6125 | 45.87 | 15.46 | 0.90 |
| 4| 6315 | 54.99 | 18.50 | 1.01 |
| 5| 6349 | 60.62 | 20.37 | 1.07 |
| 6| 6655 | 75.96 | 25.76 | 1.24 |
| 7| 6617 | 82.21 | 27.68 | 1.31 |
| 8| 6901 | 93.88 | 31.61 | 1.44 |
| 9| 6840 | 93.41 | 31.31 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 20 | 1138 | 6513 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1709 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2222 | 7161 | 99.12 | 37.95 | 1.54 |

