--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-11 04:39:50.150100005 UTC |
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
| 1| 5834 | 10.55 | 3.35 | 0.52 |
| 2| 6037 | 12.67 | 4.01 | 0.55 |
| 3| 6238 | 14.29 | 4.51 | 0.57 |
| 5| 6641 | 18.84 | 5.95 | 0.64 |
| 10| 7646 | 29.23 | 9.22 | 0.79 |
| 43| 14286 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1273 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10076 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 39.89 | 11.59 | 0.59 |
| 4 | 227 | 858 | 53.85 | 15.36 | 0.73 |
| 5 | 282 | 974 | 64.19 | 18.21 | 0.84 |
| 6 | 338 | 1081 | 65.38 | 18.88 | 0.86 |
| 7 | 396 | 1192 | 76.58 | 22.05 | 0.98 |
| 8 | 449 | 1303 | 85.27 | 24.48 | 1.07 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 24.37 | 7.71 | 0.48 |
| 2| 1923 | 25.39 | 8.68 | 0.50 |
| 3| 2127 | 28.31 | 10.14 | 0.55 |
| 5| 2466 | 32.11 | 12.55 | 0.61 |
| 10| 3296 | 43.53 | 19.06 | 0.78 |
| 40| 7457 | 95.11 | 53.42 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 612 | 22.53 | 7.30 | 0.41 |
| 2| 790 | 24.25 | 8.44 | 0.44 |
| 3| 914 | 26.95 | 9.85 | 0.48 |
| 5| 1135 | 28.04 | 11.49 | 0.51 |
| 10| 1921 | 39.02 | 17.90 | 0.68 |
| 42| 6705 | 99.94 | 56.16 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 27.51 | 8.47 | 0.46 |
| 2| 893 | 29.90 | 9.82 | 0.50 |
| 3| 921 | 32.76 | 11.24 | 0.54 |
| 5| 1175 | 36.39 | 13.58 | 0.59 |
| 10| 2040 | 48.56 | 20.38 | 0.78 |
| 36| 6005 | 97.58 | 51.50 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 687 | 33.79 | 10.15 | 0.53 |
| 2| 807 | 35.85 | 11.38 | 0.56 |
| 3| 938 | 37.87 | 12.61 | 0.59 |
| 5| 1301 | 43.13 | 15.44 | 0.67 |
| 10| 1914 | 52.63 | 21.37 | 0.81 |
| 29| 4957 | 98.95 | 47.00 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.93 | 7.56 | 0.64 |
| 2| 5948 | 35.80 | 12.03 | 0.79 |
| 3| 6077 | 44.45 | 14.97 | 0.88 |
| 4| 6215 | 54.02 | 18.18 | 0.99 |
| 5| 6402 | 64.01 | 21.54 | 1.10 |
| 6| 6456 | 69.21 | 23.27 | 1.16 |
| 7| 6885 | 82.86 | 28.08 | 1.33 |
| 8| 6984 | 94.90 | 32.06 | 1.46 |
| 9| 6951 | 95.78 | 32.20 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 20.07 | 6.71 | 0.62 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6003 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 571 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 39 | 2219 | 7158 | 99.12 | 37.95 | 1.54 |

