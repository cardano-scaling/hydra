--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-18 04:39:38.883381876 UTC |
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
| 1| 5837 | 10.38 | 3.29 | 0.51 |
| 2| 6038 | 12.99 | 4.13 | 0.55 |
| 3| 6239 | 14.29 | 4.51 | 0.57 |
| 5| 6638 | 18.64 | 5.88 | 0.64 |
| 10| 7647 | 28.73 | 9.04 | 0.78 |
| 43| 14282 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2182 | 12.13 | 7.25 | 0.40 |
| 54| 10077 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 33.32 | 9.66 | 0.52 |
| 3 | 171 | 747 | 42.51 | 12.22 | 0.61 |
| 4 | 227 | 858 | 53.49 | 15.25 | 0.73 |
| 5 | 282 | 969 | 57.72 | 16.66 | 0.78 |
| 6 | 338 | 1081 | 73.76 | 20.93 | 0.94 |
| 7 | 395 | 1192 | 74.96 | 21.63 | 0.96 |
| 8 | 449 | 1307 | 80.80 | 23.42 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 24.29 | 7.69 | 0.48 |
| 2| 1983 | 26.55 | 9.00 | 0.52 |
| 3| 2173 | 29.46 | 10.46 | 0.56 |
| 5| 2411 | 31.52 | 12.37 | 0.60 |
| 10| 3139 | 40.90 | 18.33 | 0.75 |
| 38| 7299 | 95.14 | 52.07 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.77 | 7.36 | 0.42 |
| 2| 786 | 23.59 | 8.23 | 0.43 |
| 3| 853 | 24.11 | 9.04 | 0.45 |
| 5| 1265 | 30.64 | 12.22 | 0.54 |
| 10| 1771 | 34.49 | 16.61 | 0.63 |
| 40| 6324 | 94.02 | 53.16 | 1.57 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 26.79 | 8.25 | 0.45 |
| 2| 879 | 29.93 | 9.83 | 0.50 |
| 3| 950 | 30.87 | 10.74 | 0.52 |
| 5| 1233 | 37.06 | 13.79 | 0.60 |
| 10| 1827 | 45.12 | 19.34 | 0.73 |
| 37| 6141 | 99.05 | 52.60 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 679 | 33.79 | 10.15 | 0.53 |
| 2| 807 | 35.89 | 11.39 | 0.56 |
| 3| 933 | 37.80 | 12.59 | 0.59 |
| 5| 1209 | 41.86 | 15.04 | 0.65 |
| 10| 2177 | 55.32 | 22.20 | 0.85 |
| 29| 4825 | 96.62 | 46.30 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5809 | 27.04 | 9.10 | 0.69 |
| 2| 5937 | 35.99 | 12.09 | 0.79 |
| 3| 6012 | 41.32 | 13.81 | 0.85 |
| 4| 6287 | 55.74 | 18.85 | 1.01 |
| 5| 6325 | 64.60 | 21.67 | 1.11 |
| 6| 6532 | 69.46 | 23.35 | 1.17 |
| 7| 6951 | 85.45 | 28.89 | 1.35 |
| 8| 6789 | 91.19 | 30.72 | 1.41 |
| 9| 6782 | 95.96 | 32.29 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6003 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 568 | 6172 | 41.27 | 15.06 | 0.86 |
| 10 | 30 | 1709 | 6856 | 78.71 | 30.00 | 1.30 |
| 10 | 39 | 2221 | 7160 | 98.93 | 37.88 | 1.54 |

