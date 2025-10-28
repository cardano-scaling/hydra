--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 17:44:08.897593294 UTC |
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
| 1| 5834 | 10.40 | 3.30 | 0.51 |
| 2| 6038 | 12.70 | 4.03 | 0.55 |
| 3| 6238 | 14.48 | 4.58 | 0.57 |
| 5| 6638 | 19.02 | 6.02 | 0.64 |
| 10| 7644 | 28.88 | 9.10 | 0.79 |
| 43| 14279 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2172 | 12.13 | 7.25 | 0.40 |
| 54| 10060 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 640 | 32.23 | 9.37 | 0.51 |
| 3 | 170 | 747 | 43.72 | 12.52 | 0.63 |
| 4 | 228 | 858 | 52.48 | 15.01 | 0.72 |
| 5 | 282 | 969 | 63.37 | 18.01 | 0.83 |
| 6 | 338 | 1081 | 70.11 | 20.06 | 0.91 |
| 7 | 394 | 1192 | 87.08 | 24.61 | 1.08 |
| 8 | 450 | 1303 | 94.07 | 26.59 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 24.37 | 7.71 | 0.48 |
| 2| 1985 | 26.39 | 8.96 | 0.52 |
| 3| 2013 | 25.95 | 9.49 | 0.52 |
| 5| 2380 | 31.52 | 12.37 | 0.60 |
| 10| 3106 | 39.96 | 18.06 | 0.74 |
| 39| 7672 | 98.85 | 53.80 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.54 | 7.30 | 0.41 |
| 2| 823 | 25.53 | 8.79 | 0.46 |
| 3| 923 | 26.16 | 9.63 | 0.47 |
| 5| 1157 | 28.16 | 11.51 | 0.51 |
| 10| 1862 | 37.35 | 17.43 | 0.66 |
| 43| 6654 | 99.30 | 56.59 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 712 | 27.50 | 8.46 | 0.46 |
| 2| 733 | 30.23 | 9.85 | 0.50 |
| 3| 983 | 33.40 | 11.44 | 0.55 |
| 5| 1289 | 37.43 | 13.90 | 0.61 |
| 10| 1949 | 44.00 | 19.10 | 0.73 |
| 37| 6109 | 98.74 | 52.50 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.87 | 10.16 | 0.53 |
| 2| 821 | 35.85 | 11.38 | 0.56 |
| 3| 954 | 37.91 | 12.62 | 0.59 |
| 5| 1344 | 43.54 | 15.56 | 0.67 |
| 10| 2030 | 54.14 | 21.83 | 0.83 |
| 30| 4890 | 98.85 | 47.56 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5822 | 27.08 | 9.09 | 0.69 |
| 2| 5976 | 35.92 | 12.06 | 0.79 |
| 3| 6079 | 44.69 | 15.06 | 0.89 |
| 4| 6258 | 55.14 | 18.56 | 1.00 |
| 5| 6556 | 65.99 | 22.30 | 1.13 |
| 6| 6709 | 76.44 | 25.87 | 1.25 |
| 7| 6726 | 84.45 | 28.48 | 1.33 |
| 8| 6851 | 86.86 | 29.33 | 1.36 |
| 9| 6920 | 97.99 | 32.96 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.49 | 6.17 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.52 | 6.98 | 0.62 |
| 10 | 5 | 285 | 6005 | 29.98 | 10.65 | 0.73 |
| 10 | 20 | 1137 | 6512 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1708 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2218 | 7157 | 99.38 | 38.04 | 1.54 |

