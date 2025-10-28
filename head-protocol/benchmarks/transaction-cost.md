--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 20:06:38.926803989 UTC |
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
| 1| 5834 | 10.38 | 3.29 | 0.51 |
| 2| 6042 | 12.32 | 3.89 | 0.54 |
| 3| 6238 | 14.72 | 4.66 | 0.58 |
| 5| 6638 | 18.41 | 5.80 | 0.63 |
| 10| 7646 | 29.09 | 9.17 | 0.79 |
| 43| 14279 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10050 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.40 | 9.67 | 0.52 |
| 3 | 169 | 747 | 42.62 | 12.24 | 0.62 |
| 4 | 226 | 858 | 47.94 | 13.93 | 0.68 |
| 5 | 281 | 969 | 64.38 | 18.29 | 0.84 |
| 6 | 339 | 1081 | 64.69 | 18.80 | 0.86 |
| 7 | 395 | 1192 | 76.22 | 21.96 | 0.98 |
| 8 | 452 | 1307 | 96.24 | 27.07 | 1.18 |
| 9 | 505 | 1414 | 92.71 | 26.61 | 1.15 |
| 10 | 561 | 1529 | 98.80 | 28.40 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 24.00 | 7.62 | 0.48 |
| 2| 1945 | 25.51 | 8.70 | 0.50 |
| 3| 2079 | 27.43 | 9.89 | 0.53 |
| 5| 2413 | 32.08 | 12.54 | 0.61 |
| 10| 3229 | 42.43 | 18.77 | 0.77 |
| 40| 7551 | 95.99 | 53.62 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 656 | 22.54 | 7.31 | 0.41 |
| 2| 726 | 22.60 | 7.95 | 0.42 |
| 3| 954 | 26.65 | 9.79 | 0.48 |
| 5| 1234 | 30.38 | 12.17 | 0.54 |
| 10| 2158 | 41.17 | 18.51 | 0.71 |
| 39| 6481 | 98.44 | 53.76 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 653 | 29.13 | 8.90 | 0.48 |
| 2| 817 | 29.26 | 9.62 | 0.49 |
| 3| 1010 | 31.69 | 10.98 | 0.53 |
| 5| 1218 | 37.02 | 13.77 | 0.60 |
| 10| 2060 | 45.61 | 19.58 | 0.75 |
| 38| 6199 | 98.97 | 53.20 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 812 | 35.85 | 11.38 | 0.56 |
| 3| 896 | 37.20 | 12.40 | 0.58 |
| 5| 1241 | 42.64 | 15.28 | 0.66 |
| 10| 2024 | 54.72 | 22.01 | 0.84 |
| 29| 4958 | 99.18 | 47.02 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5823 | 27.05 | 9.08 | 0.69 |
| 2| 6022 | 37.15 | 12.52 | 0.81 |
| 3| 6127 | 45.97 | 15.48 | 0.90 |
| 4| 6298 | 56.24 | 18.97 | 1.02 |
| 5| 6387 | 62.25 | 21.00 | 1.09 |
| 6| 6578 | 70.57 | 23.80 | 1.18 |
| 7| 6695 | 84.16 | 28.36 | 1.33 |
| 8| 6940 | 93.36 | 31.48 | 1.44 |
| 9| 7034 | 98.97 | 33.34 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 5 | 285 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 20 | 1141 | 6515 | 60.87 | 22.83 | 1.09 |
| 10 | 39 | 2221 | 7161 | 98.93 | 37.88 | 1.54 |

