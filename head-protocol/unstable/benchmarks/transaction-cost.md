--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-27 04:48:42.105050945 UTC |
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
| 1| 5837 | 10.17 | 3.22 | 0.51 |
| 2| 6035 | 12.41 | 3.92 | 0.54 |
| 3| 6240 | 14.88 | 4.72 | 0.58 |
| 5| 6641 | 18.58 | 5.86 | 0.63 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14283 | 99.23 | 31.02 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10061 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.27 | 9.39 | 0.51 |
| 3 | 171 | 747 | 40.37 | 11.74 | 0.59 |
| 4 | 225 | 858 | 50.84 | 14.62 | 0.70 |
| 5 | 282 | 969 | 64.42 | 18.30 | 0.85 |
| 6 | 338 | 1081 | 73.89 | 20.97 | 0.95 |
| 7 | 394 | 1196 | 81.28 | 23.26 | 1.03 |
| 8 | 450 | 1303 | 93.35 | 26.32 | 1.15 |
| 9 | 505 | 1414 | 95.53 | 27.45 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1806 | 24.00 | 7.62 | 0.48 |
| 2| 2001 | 26.96 | 9.09 | 0.52 |
| 3| 2159 | 29.13 | 10.38 | 0.56 |
| 5| 2368 | 31.33 | 12.32 | 0.60 |
| 10| 3041 | 39.08 | 17.81 | 0.73 |
| 42| 7839 | 99.59 | 55.97 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.81 | 7.37 | 0.42 |
| 2| 793 | 23.59 | 8.23 | 0.44 |
| 3| 857 | 23.99 | 9.01 | 0.45 |
| 5| 1303 | 31.18 | 12.36 | 0.55 |
| 10| 1875 | 38.44 | 17.73 | 0.67 |
| 41| 6733 | 97.83 | 54.91 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 27.54 | 8.47 | 0.46 |
| 2| 809 | 29.15 | 9.59 | 0.49 |
| 3| 939 | 32.79 | 11.25 | 0.54 |
| 5| 1282 | 37.62 | 13.97 | 0.61 |
| 10| 2089 | 48.97 | 20.50 | 0.79 |
| 35| 5653 | 98.82 | 51.04 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.83 | 10.15 | 0.53 |
| 2| 761 | 35.17 | 11.17 | 0.55 |
| 3| 983 | 38.47 | 12.79 | 0.60 |
| 5| 1285 | 42.68 | 15.29 | 0.66 |
| 10| 2054 | 54.17 | 21.84 | 0.84 |
| 29| 4882 | 98.29 | 46.78 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5797 | 27.00 | 9.08 | 0.69 |
| 2| 5881 | 32.64 | 10.90 | 0.75 |
| 3| 6014 | 41.60 | 13.93 | 0.85 |
| 4| 6190 | 51.12 | 17.18 | 0.96 |
| 5| 6451 | 64.60 | 21.74 | 1.11 |
| 6| 6638 | 75.64 | 25.53 | 1.24 |
| 7| 6765 | 83.91 | 28.40 | 1.33 |
| 8| 6935 | 90.76 | 30.56 | 1.41 |
| 9| 6882 | 94.19 | 31.64 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 10 | 569 | 6173 | 39.51 | 14.45 | 0.85 |
| 10 | 30 | 1705 | 6851 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2222 | 7161 | 98.93 | 37.88 | 1.54 |

