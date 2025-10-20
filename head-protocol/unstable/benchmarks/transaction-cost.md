--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-20 04:42:50.210937405 UTC |
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
| 1| 5836 | 10.55 | 3.35 | 0.52 |
| 2| 6037 | 12.67 | 4.01 | 0.55 |
| 3| 6239 | 14.72 | 4.66 | 0.58 |
| 5| 6638 | 18.62 | 5.87 | 0.64 |
| 10| 7644 | 28.92 | 9.11 | 0.79 |
| 43| 14282 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10065 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 640 | 32.20 | 9.36 | 0.51 |
| 3 | 170 | 747 | 39.89 | 11.59 | 0.59 |
| 4 | 227 | 858 | 48.23 | 14.02 | 0.68 |
| 5 | 282 | 969 | 64.93 | 18.45 | 0.85 |
| 6 | 340 | 1081 | 75.13 | 21.26 | 0.96 |
| 7 | 395 | 1192 | 82.37 | 23.35 | 1.04 |
| 8 | 449 | 1303 | 96.49 | 27.17 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1799 | 24.37 | 7.71 | 0.48 |
| 2| 1954 | 25.88 | 8.79 | 0.51 |
| 3| 2115 | 28.31 | 10.14 | 0.55 |
| 5| 2328 | 30.41 | 12.06 | 0.59 |
| 10| 3216 | 41.55 | 18.52 | 0.76 |
| 39| 7556 | 96.76 | 53.20 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 22.77 | 7.36 | 0.42 |
| 2| 770 | 23.59 | 8.23 | 0.43 |
| 3| 970 | 26.61 | 9.78 | 0.48 |
| 5| 1275 | 30.42 | 12.16 | 0.54 |
| 10| 1858 | 37.50 | 17.46 | 0.66 |
| 40| 6624 | 98.89 | 54.53 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 28.42 | 8.68 | 0.47 |
| 2| 833 | 31.58 | 10.26 | 0.52 |
| 3| 960 | 30.86 | 10.73 | 0.52 |
| 5| 1192 | 36.35 | 13.57 | 0.59 |
| 10| 2029 | 48.10 | 20.24 | 0.77 |
| 37| 6146 | 98.27 | 52.37 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 701 | 33.79 | 10.15 | 0.53 |
| 2| 825 | 35.89 | 11.39 | 0.56 |
| 3| 1001 | 38.51 | 12.80 | 0.60 |
| 5| 1200 | 41.97 | 15.07 | 0.65 |
| 10| 1982 | 53.16 | 21.54 | 0.82 |
| 31| 4811 | 98.39 | 48.05 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5785 | 27.16 | 9.12 | 0.69 |
| 2| 5998 | 36.98 | 12.47 | 0.80 |
| 3| 6018 | 41.29 | 13.82 | 0.85 |
| 4| 6345 | 57.36 | 19.37 | 1.03 |
| 5| 6243 | 58.41 | 19.60 | 1.04 |
| 6| 6343 | 64.19 | 21.42 | 1.10 |
| 7| 6721 | 79.61 | 26.76 | 1.28 |
| 8| 6799 | 88.89 | 29.91 | 1.38 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 284 | 6003 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 569 | 6173 | 39.95 | 14.60 | 0.85 |
| 10 | 30 | 1710 | 6856 | 79.60 | 30.31 | 1.31 |
| 10 | 38 | 2165 | 7127 | 96.44 | 36.92 | 1.51 |

