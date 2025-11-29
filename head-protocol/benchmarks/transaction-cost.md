--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-29 10:57:59.461157574 UTC |
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
| 1| 5834 | 10.57 | 3.36 | 0.52 |
| 2| 6039 | 12.72 | 4.03 | 0.55 |
| 3| 6239 | 14.72 | 4.66 | 0.58 |
| 5| 6638 | 18.98 | 6.00 | 0.64 |
| 10| 7644 | 28.94 | 9.11 | 0.79 |
| 43| 14281 | 99.08 | 30.97 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10044 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.31 | 9.40 | 0.51 |
| 3 | 170 | 747 | 40.14 | 11.65 | 0.59 |
| 4 | 225 | 858 | 50.89 | 14.63 | 0.70 |
| 5 | 282 | 969 | 61.23 | 17.50 | 0.81 |
| 6 | 339 | 1081 | 64.76 | 18.81 | 0.86 |
| 7 | 394 | 1192 | 72.35 | 20.99 | 0.94 |
| 8 | 449 | 1303 | 98.89 | 27.75 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 24.37 | 7.71 | 0.48 |
| 2| 1941 | 25.84 | 8.78 | 0.51 |
| 3| 2111 | 27.94 | 10.05 | 0.54 |
| 5| 2316 | 30.49 | 12.08 | 0.59 |
| 10| 3176 | 40.56 | 18.24 | 0.75 |
| 43| 7890 | 99.51 | 56.62 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 22.81 | 7.37 | 0.42 |
| 2| 739 | 24.35 | 8.48 | 0.44 |
| 3| 853 | 23.99 | 9.01 | 0.45 |
| 5| 1262 | 30.21 | 12.08 | 0.54 |
| 10| 2133 | 43.64 | 19.18 | 0.74 |
| 41| 6572 | 96.16 | 54.44 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 702 | 27.54 | 8.47 | 0.46 |
| 2| 842 | 29.22 | 9.61 | 0.49 |
| 3| 944 | 30.82 | 10.73 | 0.52 |
| 5| 1226 | 37.10 | 13.79 | 0.60 |
| 10| 1955 | 46.54 | 19.78 | 0.75 |
| 35| 5855 | 96.03 | 50.42 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.15 | 0.53 |
| 2| 835 | 35.88 | 11.39 | 0.56 |
| 3| 933 | 37.88 | 12.61 | 0.59 |
| 5| 1309 | 43.20 | 15.46 | 0.67 |
| 10| 2011 | 53.91 | 21.77 | 0.83 |
| 29| 4985 | 99.90 | 47.24 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5790 | 27.13 | 9.11 | 0.69 |
| 2| 5988 | 35.92 | 12.07 | 0.79 |
| 3| 6081 | 45.90 | 15.46 | 0.90 |
| 4| 6194 | 54.04 | 18.16 | 0.99 |
| 5| 6263 | 59.49 | 19.93 | 1.05 |
| 6| 6541 | 71.36 | 24.02 | 1.19 |
| 7| 6673 | 80.24 | 26.97 | 1.29 |
| 8| 6850 | 93.34 | 31.56 | 1.43 |
| 9| 6990 | 95.58 | 32.21 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 19.89 | 6.76 | 0.62 |
| 10 | 5 | 284 | 6003 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 30 | 1708 | 6854 | 80.04 | 30.46 | 1.32 |
| 10 | 38 | 2163 | 7126 | 96.37 | 36.90 | 1.51 |

