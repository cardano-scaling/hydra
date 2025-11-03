--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-03 04:53:30.847203443 UTC |
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
| 1| 5836 | 10.17 | 3.22 | 0.51 |
| 2| 6038 | 12.44 | 3.94 | 0.54 |
| 3| 6236 | 14.72 | 4.66 | 0.58 |
| 5| 6643 | 18.41 | 5.80 | 0.63 |
| 10| 7647 | 28.80 | 9.07 | 0.78 |
| 43| 14285 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10045 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 33.25 | 9.62 | 0.52 |
| 3 | 170 | 747 | 41.12 | 11.88 | 0.60 |
| 4 | 226 | 858 | 53.71 | 15.28 | 0.73 |
| 5 | 282 | 969 | 59.30 | 17.07 | 0.80 |
| 6 | 339 | 1081 | 66.11 | 19.10 | 0.87 |
| 7 | 392 | 1192 | 81.43 | 23.26 | 1.03 |
| 8 | 450 | 1303 | 84.95 | 24.41 | 1.07 |
| 9 | 505 | 1414 | 99.88 | 28.37 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1800 | 24.29 | 7.69 | 0.48 |
| 2| 2005 | 26.96 | 9.09 | 0.52 |
| 3| 2011 | 26.24 | 9.56 | 0.52 |
| 5| 2276 | 29.27 | 11.74 | 0.57 |
| 10| 3108 | 39.52 | 17.95 | 0.74 |
| 41| 7654 | 98.59 | 55.03 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 22.54 | 7.31 | 0.41 |
| 2| 778 | 23.98 | 8.37 | 0.44 |
| 3| 944 | 27.03 | 9.87 | 0.48 |
| 5| 1334 | 31.19 | 12.37 | 0.55 |
| 10| 1874 | 36.54 | 17.20 | 0.65 |
| 41| 6589 | 96.77 | 54.61 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 653 | 29.13 | 8.90 | 0.48 |
| 2| 779 | 30.91 | 10.06 | 0.51 |
| 3| 868 | 32.04 | 11.02 | 0.53 |
| 5| 1168 | 33.70 | 12.84 | 0.57 |
| 10| 2111 | 46.25 | 19.78 | 0.76 |
| 35| 5840 | 95.75 | 50.31 | 1.54 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 33.87 | 10.16 | 0.53 |
| 2| 812 | 35.81 | 11.37 | 0.56 |
| 3| 962 | 37.84 | 12.60 | 0.59 |
| 5| 1360 | 43.65 | 15.59 | 0.68 |
| 10| 2020 | 54.21 | 21.85 | 0.83 |
| 29| 4854 | 97.74 | 46.62 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5807 | 26.97 | 9.07 | 0.69 |
| 2| 5946 | 35.96 | 12.09 | 0.79 |
| 3| 6087 | 44.90 | 15.07 | 0.89 |
| 4| 6159 | 51.58 | 17.30 | 0.96 |
| 5| 6525 | 66.42 | 22.45 | 1.14 |
| 6| 6487 | 72.78 | 24.58 | 1.20 |
| 7| 6749 | 83.50 | 28.21 | 1.33 |
| 8| 6860 | 89.43 | 30.18 | 1.39 |
| 9| 6977 | 99.18 | 33.39 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 10 | 569 | 6173 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1139 | 6513 | 59.73 | 22.44 | 1.08 |
| 10 | 30 | 1710 | 6856 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2221 | 7160 | 97.61 | 37.43 | 1.52 |

