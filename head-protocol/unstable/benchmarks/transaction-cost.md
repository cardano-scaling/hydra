--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-07 14:10:59.862175413 UTC |
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
| 1| 5836 | 10.95 | 3.49 | 0.52 |
| 2| 6038 | 12.65 | 4.01 | 0.55 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 19.00 | 6.01 | 0.64 |
| 10| 7646 | 29.47 | 9.30 | 0.79 |
| 43| 14282 | 99.11 | 30.98 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10076 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 169 | 747 | 40.09 | 11.66 | 0.59 |
| 4 | 227 | 858 | 52.50 | 15.02 | 0.72 |
| 5 | 284 | 969 | 64.52 | 18.32 | 0.85 |
| 6 | 339 | 1081 | 69.21 | 19.91 | 0.90 |
| 7 | 392 | 1192 | 80.91 | 23.09 | 1.02 |
| 8 | 450 | 1303 | 87.85 | 25.11 | 1.10 |
| 9 | 506 | 1414 | 88.84 | 25.74 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 22.93 | 7.32 | 0.47 |
| 2| 1928 | 25.88 | 8.79 | 0.51 |
| 3| 2110 | 28.30 | 10.14 | 0.54 |
| 5| 2408 | 31.43 | 12.36 | 0.60 |
| 10| 3147 | 40.90 | 18.33 | 0.75 |
| 41| 7773 | 98.07 | 54.91 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 22.54 | 7.31 | 0.41 |
| 2| 758 | 24.28 | 8.45 | 0.44 |
| 3| 939 | 26.01 | 9.58 | 0.47 |
| 5| 1244 | 31.04 | 12.36 | 0.54 |
| 10| 2077 | 40.83 | 18.40 | 0.70 |
| 41| 6687 | 99.47 | 55.36 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 640 | 29.17 | 8.91 | 0.48 |
| 2| 770 | 28.55 | 9.40 | 0.48 |
| 3| 953 | 33.31 | 11.41 | 0.54 |
| 5| 1244 | 35.01 | 13.24 | 0.58 |
| 10| 2123 | 48.93 | 20.51 | 0.79 |
| 34| 5530 | 96.30 | 49.69 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 33.83 | 10.15 | 0.53 |
| 2| 897 | 36.56 | 11.60 | 0.57 |
| 3| 1033 | 38.47 | 12.79 | 0.60 |
| 5| 1246 | 42.57 | 15.26 | 0.66 |
| 10| 2043 | 54.17 | 21.85 | 0.84 |
| 27| 4822 | 95.98 | 44.88 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5829 | 27.00 | 9.07 | 0.69 |
| 2| 5892 | 32.61 | 10.90 | 0.75 |
| 3| 6152 | 45.97 | 15.48 | 0.90 |
| 4| 6211 | 51.56 | 17.33 | 0.97 |
| 5| 6402 | 64.67 | 21.80 | 1.11 |
| 6| 6611 | 74.91 | 25.30 | 1.23 |
| 7| 6721 | 83.58 | 28.20 | 1.32 |
| 8| 6849 | 91.74 | 30.83 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 17.79 | 5.94 | 0.59 |
| 10 | 1 | 57 | 5868 | 20.96 | 7.13 | 0.63 |
| 10 | 10 | 568 | 6172 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1140 | 6514 | 60.87 | 22.83 | 1.09 |
| 10 | 39 | 2220 | 7159 | 99.38 | 38.04 | 1.54 |

