--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-25 04:43:00.805983362 UTC |
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
| 1| 5836 | 10.19 | 3.22 | 0.51 |
| 2| 6035 | 12.25 | 3.87 | 0.54 |
| 3| 6239 | 14.72 | 4.66 | 0.58 |
| 5| 6640 | 18.91 | 5.98 | 0.64 |
| 10| 7651 | 28.73 | 9.04 | 0.78 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10057 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 41.27 | 11.92 | 0.60 |
| 4 | 225 | 862 | 49.72 | 14.38 | 0.69 |
| 5 | 283 | 969 | 61.18 | 17.52 | 0.81 |
| 6 | 341 | 1081 | 72.96 | 20.70 | 0.94 |
| 7 | 392 | 1196 | 72.46 | 20.98 | 0.94 |
| 8 | 451 | 1307 | 79.55 | 23.16 | 1.02 |
| 9 | 506 | 1414 | 93.43 | 26.78 | 1.16 |
| 10 | 561 | 1525 | 98.37 | 28.56 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1822 | 24.29 | 7.69 | 0.48 |
| 2| 1886 | 24.85 | 8.50 | 0.50 |
| 3| 2099 | 28.02 | 10.08 | 0.54 |
| 5| 2470 | 32.53 | 12.65 | 0.61 |
| 10| 3206 | 42.02 | 18.64 | 0.77 |
| 42| 7629 | 96.35 | 55.10 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 634 | 22.57 | 7.33 | 0.41 |
| 2| 840 | 25.49 | 8.78 | 0.46 |
| 3| 914 | 25.79 | 9.54 | 0.47 |
| 5| 1186 | 28.54 | 11.65 | 0.52 |
| 10| 1966 | 39.41 | 18.01 | 0.68 |
| 40| 6682 | 99.86 | 54.81 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 28.46 | 8.69 | 0.47 |
| 2| 779 | 30.91 | 10.06 | 0.51 |
| 3| 956 | 33.47 | 11.46 | 0.55 |
| 5| 1327 | 38.36 | 14.19 | 0.62 |
| 10| 1947 | 46.62 | 19.80 | 0.76 |
| 37| 6103 | 99.36 | 52.66 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 694 | 33.83 | 10.15 | 0.53 |
| 2| 861 | 36.56 | 11.60 | 0.57 |
| 3| 1016 | 38.59 | 12.82 | 0.60 |
| 5| 1242 | 42.68 | 15.29 | 0.66 |
| 10| 2103 | 54.89 | 22.05 | 0.85 |
| 29| 4998 | 99.94 | 47.29 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5816 | 27.05 | 9.07 | 0.69 |
| 2| 6016 | 37.01 | 12.48 | 0.80 |
| 3| 6083 | 44.70 | 15.04 | 0.89 |
| 4| 6299 | 54.98 | 18.53 | 1.01 |
| 5| 6534 | 62.86 | 21.20 | 1.10 |
| 6| 6385 | 64.90 | 21.70 | 1.11 |
| 7| 6711 | 79.20 | 26.67 | 1.28 |
| 8| 6845 | 89.02 | 30.00 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.82 | 6.63 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 30 | 1708 | 6855 | 81.81 | 31.06 | 1.34 |
| 10 | 39 | 2221 | 7161 | 98.93 | 37.88 | 1.54 |

