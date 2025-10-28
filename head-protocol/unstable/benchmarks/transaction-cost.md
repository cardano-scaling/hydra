--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 15:11:53.648978954 UTC |
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
| 1| 5834 | 10.40 | 3.30 | 0.51 |
| 2| 6035 | 12.67 | 4.01 | 0.55 |
| 3| 6238 | 15.05 | 4.78 | 0.58 |
| 5| 6641 | 18.84 | 5.95 | 0.64 |
| 10| 7647 | 28.88 | 9.10 | 0.79 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10067 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 170 | 747 | 43.64 | 12.49 | 0.63 |
| 4 | 227 | 858 | 53.92 | 15.38 | 0.73 |
| 5 | 282 | 969 | 56.36 | 16.37 | 0.77 |
| 6 | 338 | 1085 | 73.32 | 20.82 | 0.94 |
| 7 | 393 | 1192 | 78.42 | 22.45 | 1.00 |
| 8 | 450 | 1303 | 99.27 | 27.94 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1800 | 24.37 | 7.71 | 0.48 |
| 2| 1883 | 24.47 | 8.41 | 0.49 |
| 3| 2017 | 25.95 | 9.49 | 0.52 |
| 5| 2317 | 30.18 | 12.00 | 0.58 |
| 10| 3136 | 40.83 | 18.31 | 0.75 |
| 38| 7496 | 96.51 | 52.44 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 22.57 | 7.32 | 0.41 |
| 2| 747 | 24.08 | 8.41 | 0.44 |
| 3| 895 | 25.10 | 9.32 | 0.46 |
| 5| 1149 | 28.80 | 11.70 | 0.52 |
| 10| 2094 | 41.66 | 18.65 | 0.71 |
| 42| 6625 | 96.22 | 55.09 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 643 | 29.13 | 8.90 | 0.48 |
| 2| 767 | 28.55 | 9.40 | 0.48 |
| 3| 940 | 30.90 | 10.74 | 0.52 |
| 5| 1164 | 33.66 | 12.83 | 0.57 |
| 10| 2192 | 47.03 | 20.01 | 0.77 |
| 35| 5884 | 96.45 | 50.50 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 33.15 | 9.95 | 0.52 |
| 2| 814 | 35.81 | 11.37 | 0.56 |
| 3| 896 | 37.16 | 12.39 | 0.58 |
| 5| 1221 | 41.86 | 15.04 | 0.65 |
| 10| 2071 | 54.88 | 22.05 | 0.84 |
| 28| 4630 | 94.39 | 44.97 | 1.44 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5779 | 27.09 | 9.08 | 0.69 |
| 2| 5845 | 31.52 | 10.48 | 0.74 |
| 3| 6133 | 45.86 | 15.45 | 0.90 |
| 4| 6355 | 55.65 | 18.86 | 1.02 |
| 5| 6216 | 55.95 | 18.72 | 1.01 |
| 6| 6464 | 72.72 | 24.38 | 1.20 |
| 7| 6718 | 82.11 | 27.66 | 1.31 |
| 8| 6903 | 91.56 | 30.95 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.93 | 6.32 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 569 | 6173 | 39.06 | 14.30 | 0.84 |
| 10 | 40 | 2276 | 7192 | 99.66 | 38.24 | 1.55 |

