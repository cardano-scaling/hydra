--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-19 12:34:47.352277922 UTC |
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
| 1| 5836 | 10.66 | 3.39 | 0.52 |
| 2| 6038 | 12.42 | 3.93 | 0.54 |
| 3| 6239 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 18.96 | 6.00 | 0.64 |
| 10| 7644 | 28.92 | 9.11 | 0.79 |
| 43| 14283 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1281 | 6.41 | 3.60 | 0.28 |
| 10| 2178 | 12.13 | 7.25 | 0.40 |
| 54| 10052 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 32.30 | 9.40 | 0.51 |
| 3 | 171 | 747 | 39.86 | 11.58 | 0.59 |
| 4 | 228 | 858 | 48.01 | 13.99 | 0.68 |
| 5 | 283 | 969 | 60.97 | 17.41 | 0.81 |
| 6 | 336 | 1085 | 70.86 | 20.19 | 0.92 |
| 7 | 396 | 1192 | 82.20 | 23.27 | 1.03 |
| 8 | 450 | 1303 | 91.12 | 25.88 | 1.13 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1800 | 24.00 | 7.62 | 0.48 |
| 2| 1946 | 25.92 | 8.80 | 0.51 |
| 3| 2013 | 26.24 | 9.56 | 0.52 |
| 5| 2275 | 28.81 | 11.63 | 0.57 |
| 10| 3159 | 40.78 | 18.30 | 0.75 |
| 41| 7631 | 97.38 | 54.73 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.81 | 7.37 | 0.42 |
| 2| 771 | 23.55 | 8.22 | 0.43 |
| 3| 967 | 26.61 | 9.78 | 0.48 |
| 5| 1285 | 31.08 | 12.34 | 0.55 |
| 10| 2059 | 40.35 | 18.29 | 0.70 |
| 42| 6545 | 97.18 | 55.38 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 694 | 27.50 | 8.46 | 0.46 |
| 2| 834 | 31.62 | 10.27 | 0.52 |
| 3| 972 | 33.43 | 11.45 | 0.55 |
| 5| 1290 | 34.93 | 13.22 | 0.58 |
| 10| 1956 | 46.83 | 19.87 | 0.76 |
| 36| 5759 | 94.32 | 50.52 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.15 | 0.53 |
| 2| 836 | 36.52 | 11.59 | 0.57 |
| 3| 1021 | 39.26 | 13.03 | 0.61 |
| 5| 1325 | 43.28 | 15.48 | 0.67 |
| 10| 2047 | 54.14 | 21.83 | 0.84 |
| 29| 4776 | 97.47 | 46.54 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5811 | 27.13 | 9.10 | 0.69 |
| 2| 5974 | 35.99 | 12.09 | 0.79 |
| 3| 6086 | 44.57 | 14.98 | 0.89 |
| 4| 6217 | 54.01 | 18.19 | 0.99 |
| 5| 6362 | 60.37 | 20.28 | 1.06 |
| 6| 6718 | 75.65 | 25.63 | 1.24 |
| 7| 6790 | 84.09 | 28.35 | 1.33 |
| 8| 6930 | 93.74 | 31.60 | 1.44 |
| 9| 6863 | 96.78 | 32.45 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 5 | 284 | 6003 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 569 | 6174 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1140 | 6514 | 59.98 | 22.53 | 1.08 |
| 10 | 39 | 2225 | 7164 | 98.49 | 37.73 | 1.53 |

