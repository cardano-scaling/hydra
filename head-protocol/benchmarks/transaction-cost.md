--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 16:58:18.045862778 UTC |
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
| 2| 6038 | 12.46 | 3.94 | 0.55 |
| 3| 6239 | 14.78 | 4.68 | 0.58 |
| 5| 6638 | 18.50 | 5.83 | 0.63 |
| 10| 7646 | 28.81 | 9.07 | 0.78 |
| 43| 14285 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 556 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10066 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 34.19 | 9.84 | 0.53 |
| 3 | 170 | 747 | 40.18 | 11.66 | 0.59 |
| 4 | 226 | 858 | 49.95 | 14.46 | 0.70 |
| 5 | 282 | 969 | 59.10 | 17.02 | 0.79 |
| 6 | 338 | 1081 | 66.11 | 19.14 | 0.87 |
| 7 | 396 | 1192 | 80.43 | 22.93 | 1.02 |
| 8 | 449 | 1303 | 98.84 | 27.74 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 24.00 | 7.62 | 0.48 |
| 2| 2011 | 26.55 | 9.00 | 0.52 |
| 3| 2053 | 26.95 | 9.77 | 0.53 |
| 5| 2374 | 31.03 | 12.25 | 0.59 |
| 10| 3256 | 41.90 | 18.63 | 0.77 |
| 38| 7324 | 95.63 | 52.19 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 604 | 22.84 | 7.38 | 0.41 |
| 2| 815 | 25.45 | 8.77 | 0.46 |
| 3| 934 | 26.05 | 9.58 | 0.47 |
| 5| 1304 | 30.63 | 12.23 | 0.54 |
| 10| 2021 | 38.47 | 17.73 | 0.68 |
| 40| 6493 | 96.20 | 53.78 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 29.13 | 8.90 | 0.48 |
| 2| 770 | 28.55 | 9.40 | 0.48 |
| 3| 910 | 32.75 | 11.24 | 0.54 |
| 5| 1194 | 36.24 | 13.54 | 0.59 |
| 10| 1951 | 44.12 | 19.13 | 0.73 |
| 34| 5735 | 95.55 | 49.62 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 698 | 33.87 | 10.16 | 0.53 |
| 2| 807 | 35.92 | 11.40 | 0.56 |
| 3| 1006 | 38.51 | 12.80 | 0.60 |
| 5| 1241 | 42.64 | 15.28 | 0.66 |
| 10| 2133 | 54.50 | 21.96 | 0.84 |
| 28| 4770 | 98.66 | 46.25 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5803 | 27.00 | 9.08 | 0.69 |
| 2| 5868 | 34.84 | 11.66 | 0.77 |
| 3| 6086 | 44.64 | 15.00 | 0.89 |
| 4| 6247 | 54.60 | 18.48 | 1.00 |
| 5| 6408 | 64.86 | 21.90 | 1.11 |
| 6| 6425 | 65.98 | 22.18 | 1.13 |
| 7| 6732 | 79.39 | 26.79 | 1.28 |
| 8| 6833 | 90.14 | 30.35 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 20 | 1139 | 6514 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1706 | 6852 | 80.48 | 30.61 | 1.32 |
| 10 | 38 | 2162 | 7124 | 97.33 | 37.23 | 1.52 |

