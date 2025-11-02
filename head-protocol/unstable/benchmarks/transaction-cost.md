--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-02 04:48:41.058125136 UTC |
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
| 1| 5836 | 10.57 | 3.36 | 0.52 |
| 2| 6042 | 12.63 | 4.00 | 0.55 |
| 3| 6239 | 14.38 | 4.54 | 0.57 |
| 5| 6638 | 18.88 | 5.97 | 0.64 |
| 10| 7648 | 29.12 | 9.18 | 0.79 |
| 43| 14286 | 98.94 | 30.92 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2182 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 170 | 747 | 41.12 | 11.90 | 0.60 |
| 4 | 226 | 858 | 49.65 | 14.34 | 0.69 |
| 5 | 282 | 969 | 59.71 | 17.14 | 0.80 |
| 6 | 338 | 1081 | 69.66 | 19.91 | 0.90 |
| 7 | 392 | 1192 | 72.21 | 20.91 | 0.94 |
| 8 | 450 | 1303 | 85.65 | 24.63 | 1.08 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 23.92 | 7.60 | 0.48 |
| 2| 1958 | 26.54 | 9.00 | 0.52 |
| 3| 2086 | 27.32 | 9.86 | 0.53 |
| 5| 2367 | 31.30 | 12.31 | 0.60 |
| 10| 3121 | 39.75 | 18.01 | 0.74 |
| 39| 7526 | 95.83 | 52.94 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 650 | 22.54 | 7.31 | 0.41 |
| 2| 753 | 23.58 | 8.23 | 0.43 |
| 3| 964 | 26.97 | 9.86 | 0.48 |
| 5| 1092 | 27.11 | 11.23 | 0.50 |
| 10| 1800 | 35.47 | 16.89 | 0.64 |
| 40| 6518 | 98.34 | 54.39 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 27.50 | 8.46 | 0.46 |
| 2| 802 | 30.91 | 10.06 | 0.51 |
| 3| 912 | 32.72 | 11.23 | 0.54 |
| 5| 1199 | 36.38 | 13.58 | 0.59 |
| 10| 2006 | 48.10 | 20.24 | 0.77 |
| 34| 5717 | 95.09 | 49.51 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 33.83 | 10.15 | 0.53 |
| 2| 814 | 35.88 | 11.39 | 0.56 |
| 3| 944 | 37.91 | 12.62 | 0.59 |
| 5| 1288 | 42.53 | 15.25 | 0.66 |
| 10| 1909 | 52.56 | 21.36 | 0.81 |
| 28| 4920 | 98.86 | 46.34 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5818 | 27.08 | 9.08 | 0.69 |
| 2| 5914 | 35.95 | 12.09 | 0.79 |
| 3| 6163 | 45.61 | 15.39 | 0.90 |
| 4| 6071 | 45.80 | 15.24 | 0.90 |
| 5| 6372 | 59.94 | 20.20 | 1.06 |
| 6| 6515 | 70.20 | 23.66 | 1.17 |
| 7| 6759 | 83.15 | 28.03 | 1.32 |
| 8| 6917 | 90.35 | 30.47 | 1.40 |
| 9| 7054 | 99.61 | 33.57 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 20 | 1138 | 6513 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 38 | 2161 | 7124 | 97.77 | 37.38 | 1.52 |

