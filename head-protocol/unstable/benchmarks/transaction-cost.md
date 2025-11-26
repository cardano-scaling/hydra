--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-26 11:03:24.994880772 UTC |
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
| 1| 5836 | 10.40 | 3.30 | 0.51 |
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6640 | 19.17 | 6.07 | 0.64 |
| 10| 7646 | 28.88 | 9.10 | 0.79 |
| 43| 14281 | 99.02 | 30.95 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1278 | 6.41 | 3.60 | 0.28 |
| 10| 2183 | 12.13 | 7.25 | 0.40 |
| 54| 10049 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 528 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.62 | 0.52 |
| 3 | 170 | 747 | 39.96 | 11.62 | 0.59 |
| 4 | 228 | 858 | 52.77 | 15.13 | 0.72 |
| 5 | 284 | 969 | 57.81 | 16.68 | 0.78 |
| 6 | 336 | 1081 | 66.81 | 19.31 | 0.88 |
| 7 | 394 | 1192 | 72.39 | 21.01 | 0.94 |
| 8 | 449 | 1303 | 98.44 | 27.64 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1750 | 23.30 | 7.41 | 0.47 |
| 2| 2009 | 27.00 | 9.10 | 0.52 |
| 3| 2098 | 27.90 | 10.04 | 0.54 |
| 5| 2511 | 33.32 | 12.88 | 0.62 |
| 10| 3188 | 42.01 | 18.64 | 0.76 |
| 40| 7807 | 99.78 | 54.71 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.54 | 7.30 | 0.41 |
| 2| 814 | 25.48 | 8.77 | 0.46 |
| 3| 902 | 25.05 | 9.31 | 0.46 |
| 5| 1278 | 30.03 | 12.03 | 0.54 |
| 10| 1986 | 39.76 | 18.11 | 0.69 |
| 39| 6554 | 98.09 | 53.67 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 693 | 27.54 | 8.47 | 0.46 |
| 2| 860 | 31.58 | 10.27 | 0.52 |
| 3| 951 | 30.98 | 10.76 | 0.52 |
| 5| 1165 | 33.70 | 12.84 | 0.57 |
| 10| 2129 | 48.82 | 20.46 | 0.79 |
| 36| 6083 | 98.86 | 51.90 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.83 | 10.15 | 0.53 |
| 2| 811 | 35.85 | 11.38 | 0.56 |
| 3| 985 | 38.55 | 12.81 | 0.60 |
| 5| 1226 | 42.01 | 15.08 | 0.65 |
| 10| 2003 | 54.10 | 21.82 | 0.83 |
| 29| 4993 | 99.41 | 47.15 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5807 | 27.13 | 9.12 | 0.69 |
| 2| 5928 | 35.88 | 12.05 | 0.79 |
| 3| 6130 | 45.74 | 15.43 | 0.90 |
| 4| 6115 | 47.14 | 15.76 | 0.91 |
| 5| 6389 | 63.82 | 21.50 | 1.10 |
| 6| 6478 | 70.44 | 23.63 | 1.17 |
| 7| 6676 | 75.81 | 25.50 | 1.24 |
| 8| 6790 | 89.10 | 29.96 | 1.38 |
| 9| 7050 | 98.03 | 33.03 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.12 | 6.39 | 0.61 |
| 10 | 1 | 57 | 5868 | 19.01 | 6.46 | 0.61 |
| 10 | 5 | 284 | 6004 | 29.09 | 10.34 | 0.72 |
| 10 | 10 | 569 | 6173 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1138 | 6512 | 59.91 | 22.51 | 1.08 |
| 10 | 38 | 2162 | 7125 | 97.07 | 37.14 | 1.52 |

