--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-14 04:42:36.902829352 UTC |
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
| 1| 5837 | 10.48 | 3.33 | 0.52 |
| 2| 6037 | 12.41 | 3.92 | 0.54 |
| 3| 6238 | 14.50 | 4.58 | 0.57 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7647 | 28.94 | 9.11 | 0.79 |
| 43| 14286 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10060 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.38 | 9.91 | 0.53 |
| 3 | 169 | 747 | 41.40 | 11.95 | 0.60 |
| 4 | 227 | 858 | 50.02 | 14.47 | 0.70 |
| 5 | 284 | 969 | 61.37 | 17.60 | 0.82 |
| 6 | 340 | 1081 | 65.04 | 18.89 | 0.86 |
| 7 | 393 | 1196 | 78.69 | 22.51 | 1.00 |
| 8 | 449 | 1303 | 89.25 | 25.44 | 1.11 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1796 | 24.00 | 7.62 | 0.48 |
| 2| 1941 | 25.47 | 8.70 | 0.50 |
| 3| 2074 | 27.39 | 9.88 | 0.53 |
| 5| 2317 | 30.00 | 11.96 | 0.58 |
| 10| 3314 | 44.11 | 19.22 | 0.79 |
| 41| 7755 | 99.96 | 55.39 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 634 | 22.84 | 7.39 | 0.42 |
| 2| 783 | 23.55 | 8.22 | 0.43 |
| 3| 948 | 26.95 | 9.88 | 0.48 |
| 5| 1190 | 29.10 | 11.77 | 0.52 |
| 10| 1835 | 36.69 | 17.22 | 0.65 |
| 40| 6318 | 96.40 | 53.80 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 702 | 27.50 | 8.46 | 0.46 |
| 2| 834 | 31.66 | 10.28 | 0.52 |
| 3| 952 | 33.43 | 11.44 | 0.54 |
| 5| 1226 | 37.02 | 13.78 | 0.60 |
| 10| 2044 | 48.16 | 20.26 | 0.78 |
| 36| 5939 | 97.03 | 51.33 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.21 | 11.18 | 0.55 |
| 3| 982 | 38.59 | 12.82 | 0.60 |
| 5| 1335 | 43.31 | 15.48 | 0.67 |
| 10| 2064 | 55.00 | 22.08 | 0.84 |
| 28| 4782 | 95.79 | 45.44 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5781 | 27.13 | 9.09 | 0.69 |
| 2| 5942 | 36.11 | 12.14 | 0.79 |
| 3| 6042 | 43.83 | 14.68 | 0.88 |
| 4| 6078 | 47.42 | 15.86 | 0.92 |
| 5| 6471 | 61.75 | 20.82 | 1.08 |
| 6| 6651 | 76.01 | 25.66 | 1.24 |
| 7| 6838 | 83.43 | 28.18 | 1.33 |
| 8| 6760 | 85.69 | 28.77 | 1.35 |
| 9| 6991 | 95.85 | 32.26 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.38 | 6.48 | 0.61 |
| 10 | 20 | 1139 | 6513 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1706 | 6852 | 80.04 | 30.46 | 1.32 |
| 10 | 40 | 2278 | 7194 | 99.22 | 38.09 | 1.54 |
| 10 | 39 | 2217 | 7156 | 97.79 | 37.50 | 1.53 |

