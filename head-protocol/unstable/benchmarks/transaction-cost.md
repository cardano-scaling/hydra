--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-09 13:03:02.801346396 UTC |
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
| 1| 5834 | 10.17 | 3.22 | 0.51 |
| 2| 6038 | 12.67 | 4.01 | 0.55 |
| 3| 6238 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 18.90 | 5.97 | 0.64 |
| 10| 7644 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.85 | 30.89 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10063 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.31 | 9.40 | 0.51 |
| 3 | 170 | 747 | 42.72 | 12.29 | 0.62 |
| 4 | 226 | 858 | 48.03 | 13.97 | 0.68 |
| 5 | 283 | 969 | 63.84 | 18.12 | 0.84 |
| 6 | 337 | 1081 | 69.89 | 20.04 | 0.91 |
| 7 | 394 | 1192 | 86.23 | 24.23 | 1.07 |
| 8 | 448 | 1303 | 93.39 | 26.48 | 1.15 |
| 10 | 560 | 1525 | 97.88 | 28.31 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1810 | 24.00 | 7.62 | 0.48 |
| 2| 1886 | 24.43 | 8.40 | 0.49 |
| 3| 2097 | 28.39 | 10.16 | 0.55 |
| 5| 2426 | 31.50 | 12.38 | 0.60 |
| 10| 3202 | 42.03 | 18.64 | 0.77 |
| 40| 7717 | 99.50 | 54.62 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 22.53 | 7.29 | 0.41 |
| 2| 752 | 23.58 | 8.24 | 0.43 |
| 3| 961 | 26.98 | 9.87 | 0.48 |
| 5| 1202 | 29.95 | 12.02 | 0.53 |
| 10| 1822 | 36.03 | 17.06 | 0.64 |
| 40| 6472 | 98.40 | 54.40 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 597 | 28.42 | 8.68 | 0.47 |
| 2| 771 | 28.47 | 9.38 | 0.48 |
| 3| 910 | 32.72 | 11.23 | 0.54 |
| 5| 1334 | 38.49 | 14.22 | 0.62 |
| 10| 2214 | 49.90 | 20.79 | 0.80 |
| 36| 6047 | 98.77 | 51.84 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.16 | 0.53 |
| 2| 887 | 36.48 | 11.58 | 0.57 |
| 3| 947 | 37.84 | 12.60 | 0.59 |
| 5| 1201 | 41.86 | 15.04 | 0.65 |
| 10| 1990 | 53.20 | 21.55 | 0.82 |
| 30| 4812 | 98.27 | 47.38 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5805 | 27.13 | 9.09 | 0.69 |
| 2| 5893 | 34.87 | 11.69 | 0.78 |
| 3| 6235 | 46.53 | 15.76 | 0.91 |
| 4| 6364 | 55.92 | 18.91 | 1.02 |
| 5| 6304 | 59.96 | 20.18 | 1.06 |
| 6| 6507 | 69.78 | 23.48 | 1.17 |
| 7| 6822 | 85.34 | 28.79 | 1.35 |
| 8| 6908 | 92.48 | 31.27 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 20 | 1140 | 6514 | 59.98 | 22.53 | 1.08 |
| 10 | 38 | 2161 | 7123 | 97.33 | 37.23 | 1.52 |

