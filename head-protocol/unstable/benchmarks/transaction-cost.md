--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-04 04:38:50.543500642 UTC |
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
| 1| 5836 | 10.78 | 3.43 | 0.52 |
| 2| 6038 | 12.44 | 3.94 | 0.54 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 18.43 | 5.81 | 0.63 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2178 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 33.18 | 9.60 | 0.52 |
| 3 | 170 | 747 | 43.84 | 12.55 | 0.63 |
| 4 | 227 | 858 | 51.99 | 14.89 | 0.72 |
| 5 | 283 | 969 | 61.72 | 17.69 | 0.82 |
| 6 | 337 | 1081 | 64.12 | 18.59 | 0.85 |
| 7 | 392 | 1192 | 87.06 | 24.56 | 1.08 |
| 8 | 451 | 1303 | 89.88 | 25.64 | 1.12 |
| 10 | 560 | 1525 | 97.60 | 28.37 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.37 | 7.71 | 0.48 |
| 2| 1932 | 25.85 | 8.78 | 0.51 |
| 3| 2121 | 28.39 | 10.16 | 0.55 |
| 5| 2501 | 33.40 | 12.90 | 0.62 |
| 10| 3108 | 39.82 | 18.03 | 0.74 |
| 39| 7549 | 96.50 | 53.14 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 22.57 | 7.32 | 0.41 |
| 2| 864 | 25.33 | 8.75 | 0.46 |
| 3| 992 | 26.97 | 9.87 | 0.48 |
| 5| 1307 | 31.61 | 12.52 | 0.55 |
| 10| 2066 | 41.44 | 18.57 | 0.71 |
| 45| 6894 | 99.85 | 58.13 | 1.68 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 703 | 27.50 | 8.46 | 0.46 |
| 2| 732 | 30.23 | 9.85 | 0.50 |
| 3| 945 | 30.87 | 10.74 | 0.52 |
| 5| 1324 | 35.76 | 13.47 | 0.59 |
| 10| 1872 | 42.73 | 18.71 | 0.71 |
| 37| 5799 | 96.54 | 51.77 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 759 | 35.14 | 11.16 | 0.55 |
| 3| 937 | 37.95 | 12.63 | 0.59 |
| 5| 1214 | 41.82 | 15.03 | 0.65 |
| 10| 1996 | 53.95 | 21.78 | 0.83 |
| 30| 4844 | 99.10 | 47.63 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5698 | 22.93 | 7.56 | 0.64 |
| 2| 5924 | 36.01 | 12.08 | 0.79 |
| 3| 6063 | 44.83 | 15.09 | 0.89 |
| 4| 6172 | 54.08 | 18.17 | 0.99 |
| 5| 6410 | 64.12 | 21.65 | 1.11 |
| 6| 6520 | 67.17 | 22.55 | 1.14 |
| 7| 6797 | 85.09 | 28.74 | 1.34 |
| 8| 7040 | 96.35 | 32.60 | 1.47 |
| 9| 6906 | 96.98 | 32.59 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.49 | 6.17 | 0.60 |
| 10 | 5 | 285 | 6004 | 29.53 | 10.50 | 0.73 |
| 10 | 10 | 570 | 6174 | 39.44 | 14.43 | 0.84 |
| 10 | 20 | 1139 | 6514 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1706 | 6852 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2221 | 7161 | 97.61 | 37.43 | 1.52 |

