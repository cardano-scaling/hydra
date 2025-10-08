--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-08 19:08:16.566676407 UTC |
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
| 1| 5836 | 10.47 | 3.32 | 0.52 |
| 2| 6038 | 12.54 | 3.97 | 0.55 |
| 3| 6238 | 14.50 | 4.58 | 0.57 |
| 5| 6641 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 29.49 | 9.31 | 0.79 |
| 43| 14282 | 98.73 | 30.85 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 736 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2178 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 528 | 24.46 | 7.13 | 0.42 |
| 2 | 112 | 636 | 32.23 | 9.37 | 0.51 |
| 3 | 171 | 747 | 41.20 | 11.90 | 0.60 |
| 4 | 227 | 858 | 50.99 | 14.66 | 0.71 |
| 5 | 283 | 969 | 64.08 | 18.18 | 0.84 |
| 6 | 338 | 1081 | 66.18 | 19.08 | 0.87 |
| 7 | 394 | 1192 | 82.45 | 23.37 | 1.04 |
| 8 | 449 | 1303 | 98.42 | 27.63 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1814 | 24.37 | 7.71 | 0.48 |
| 2| 1931 | 25.43 | 8.69 | 0.50 |
| 3| 2058 | 26.99 | 9.78 | 0.53 |
| 5| 2338 | 30.29 | 12.03 | 0.58 |
| 10| 3350 | 44.55 | 19.35 | 0.80 |
| 39| 7552 | 98.62 | 53.70 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 22.81 | 7.37 | 0.42 |
| 2| 780 | 25.43 | 8.76 | 0.45 |
| 3| 971 | 28.16 | 10.18 | 0.49 |
| 5| 1239 | 29.52 | 11.90 | 0.53 |
| 10| 2005 | 40.71 | 18.37 | 0.70 |
| 41| 6660 | 96.91 | 54.68 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 703 | 27.54 | 8.47 | 0.46 |
| 2| 842 | 31.62 | 10.27 | 0.52 |
| 3| 1008 | 33.43 | 11.44 | 0.55 |
| 5| 1131 | 35.68 | 13.36 | 0.58 |
| 10| 2054 | 45.08 | 19.43 | 0.74 |
| 35| 5823 | 95.43 | 50.25 | 1.54 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 807 | 35.92 | 11.40 | 0.56 |
| 3| 965 | 37.84 | 12.60 | 0.59 |
| 5| 1334 | 44.07 | 15.71 | 0.68 |
| 10| 2033 | 54.13 | 21.83 | 0.83 |
| 29| 4831 | 97.53 | 46.59 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5842 | 27.00 | 9.07 | 0.69 |
| 2| 5935 | 35.96 | 12.06 | 0.79 |
| 3| 6148 | 45.74 | 15.41 | 0.90 |
| 4| 6315 | 56.07 | 18.92 | 1.02 |
| 5| 6320 | 63.10 | 21.20 | 1.09 |
| 6| 6470 | 65.79 | 22.07 | 1.13 |
| 7| 6841 | 83.33 | 28.19 | 1.33 |
| 8| 6639 | 83.79 | 27.99 | 1.32 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1139 | 6513 | 58.66 | 22.07 | 1.07 |
| 10 | 39 | 2222 | 7161 | 99.82 | 38.19 | 1.55 |

