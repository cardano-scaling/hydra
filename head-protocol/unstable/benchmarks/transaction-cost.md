--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-23 04:56:29.696912967 UTC |
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
| 1| 5840 | 10.28 | 3.25 | 0.51 |
| 2| 6039 | 12.46 | 3.94 | 0.55 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 19.36 | 6.14 | 0.64 |
| 10| 7646 | 28.94 | 9.11 | 0.79 |
| 43| 14282 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 915 | 4.36 | 2.33 | 0.24 |
| 5| 1275 | 6.41 | 3.60 | 0.28 |
| 10| 2172 | 12.13 | 7.25 | 0.40 |
| 54| 10061 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 640 | 33.25 | 9.61 | 0.52 |
| 3 | 171 | 747 | 42.25 | 12.15 | 0.61 |
| 4 | 225 | 858 | 52.77 | 15.13 | 0.72 |
| 5 | 284 | 974 | 58.87 | 16.90 | 0.79 |
| 6 | 339 | 1081 | 65.88 | 19.04 | 0.87 |
| 7 | 394 | 1196 | 74.36 | 21.47 | 0.96 |
| 8 | 451 | 1303 | 85.11 | 24.50 | 1.07 |
| 9 | 506 | 1414 | 91.11 | 26.23 | 1.14 |
| 10 | 560 | 1525 | 96.49 | 27.91 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1807 | 24.00 | 7.62 | 0.48 |
| 2| 1979 | 26.47 | 8.98 | 0.52 |
| 3| 2150 | 29.13 | 10.38 | 0.56 |
| 5| 2373 | 31.28 | 12.31 | 0.60 |
| 10| 3076 | 38.67 | 17.71 | 0.73 |
| 39| 7429 | 96.76 | 53.17 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 22.54 | 7.30 | 0.41 |
| 2| 833 | 25.57 | 8.80 | 0.46 |
| 3| 931 | 26.16 | 9.62 | 0.47 |
| 5| 1282 | 32.08 | 12.62 | 0.56 |
| 10| 1953 | 38.50 | 17.73 | 0.68 |
| 41| 6631 | 99.60 | 55.41 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 652 | 29.17 | 8.91 | 0.48 |
| 2| 800 | 30.91 | 10.06 | 0.51 |
| 3| 943 | 32.76 | 11.24 | 0.54 |
| 5| 1303 | 35.76 | 13.47 | 0.59 |
| 10| 2040 | 48.07 | 20.23 | 0.77 |
| 33| 5605 | 93.61 | 48.44 | 1.50 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 695 | 33.87 | 10.16 | 0.53 |
| 2| 824 | 35.92 | 11.40 | 0.56 |
| 3| 944 | 37.95 | 12.63 | 0.59 |
| 5| 1302 | 43.36 | 15.49 | 0.67 |
| 10| 2279 | 57.06 | 22.71 | 0.88 |
| 30| 4921 | 98.88 | 47.57 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5826 | 27.05 | 9.08 | 0.69 |
| 2| 5940 | 35.80 | 12.04 | 0.79 |
| 3| 6063 | 44.62 | 15.01 | 0.89 |
| 4| 6260 | 54.79 | 18.53 | 1.00 |
| 5| 6416 | 61.28 | 20.63 | 1.08 |
| 6| 6562 | 74.57 | 25.17 | 1.22 |
| 7| 6674 | 83.92 | 28.25 | 1.33 |
| 8| 6870 | 92.93 | 31.32 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 19.89 | 6.76 | 0.62 |
| 10 | 10 | 568 | 6172 | 38.81 | 14.21 | 0.84 |
| 10 | 30 | 1709 | 6855 | 79.15 | 30.16 | 1.31 |
| 10 | 39 | 2217 | 7156 | 98.05 | 37.58 | 1.53 |

