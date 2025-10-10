--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-10 04:42:08.885291093 UTC |
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
| 1| 5834 | 10.28 | 3.25 | 0.51 |
| 2| 6037 | 12.46 | 3.94 | 0.55 |
| 3| 6238 | 14.72 | 4.66 | 0.58 |
| 5| 6638 | 19.00 | 6.01 | 0.64 |
| 10| 7647 | 28.94 | 9.11 | 0.79 |
| 43| 14281 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10068 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 32.39 | 9.43 | 0.51 |
| 3 | 171 | 747 | 42.75 | 12.31 | 0.62 |
| 4 | 226 | 858 | 53.55 | 15.24 | 0.73 |
| 5 | 281 | 969 | 64.16 | 18.17 | 0.84 |
| 6 | 338 | 1085 | 69.13 | 19.74 | 0.90 |
| 7 | 393 | 1192 | 86.44 | 24.32 | 1.08 |
| 8 | 450 | 1303 | 93.81 | 26.53 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 24.37 | 7.71 | 0.48 |
| 2| 1924 | 25.39 | 8.68 | 0.50 |
| 3| 2014 | 25.95 | 9.49 | 0.52 |
| 5| 2318 | 29.89 | 11.93 | 0.58 |
| 10| 2983 | 38.00 | 17.51 | 0.71 |
| 40| 7508 | 97.36 | 54.01 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.54 | 7.30 | 0.41 |
| 2| 699 | 22.62 | 7.97 | 0.42 |
| 3| 830 | 24.13 | 9.05 | 0.45 |
| 5| 1406 | 33.60 | 13.04 | 0.58 |
| 10| 2057 | 40.88 | 18.40 | 0.70 |
| 41| 6699 | 98.97 | 55.24 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 29.13 | 8.90 | 0.48 |
| 2| 882 | 29.97 | 9.84 | 0.50 |
| 3| 965 | 30.87 | 10.74 | 0.52 |
| 5| 1324 | 35.71 | 13.45 | 0.59 |
| 10| 2030 | 45.13 | 19.42 | 0.74 |
| 37| 6079 | 98.85 | 52.50 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 33.83 | 10.15 | 0.53 |
| 2| 886 | 36.64 | 11.62 | 0.57 |
| 3| 941 | 37.91 | 12.62 | 0.59 |
| 5| 1307 | 43.43 | 15.51 | 0.67 |
| 10| 1955 | 52.78 | 21.43 | 0.82 |
| 28| 4593 | 94.17 | 44.90 | 1.44 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5788 | 27.09 | 9.08 | 0.69 |
| 2| 5962 | 35.99 | 12.10 | 0.79 |
| 3| 6094 | 42.59 | 14.31 | 0.87 |
| 4| 6395 | 56.61 | 19.16 | 1.03 |
| 5| 6224 | 55.47 | 18.53 | 1.01 |
| 6| 6516 | 69.01 | 23.18 | 1.16 |
| 7| 6877 | 85.50 | 28.87 | 1.35 |
| 8| 7032 | 94.66 | 31.97 | 1.45 |
| 9| 6992 | 99.49 | 33.50 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 56 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 284 | 6003 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 568 | 6172 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1139 | 6514 | 60.17 | 22.59 | 1.09 |
| 10 | 30 | 1709 | 6855 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2222 | 7161 | 98.93 | 37.88 | 1.54 |

