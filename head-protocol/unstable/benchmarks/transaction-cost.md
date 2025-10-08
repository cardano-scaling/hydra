--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-08 18:43:40.892771105 UTC |
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
| 1| 5836 | 10.35 | 3.28 | 0.51 |
| 2| 6039 | 12.44 | 3.94 | 0.54 |
| 3| 6236 | 14.48 | 4.58 | 0.57 |
| 5| 6640 | 18.62 | 5.87 | 0.64 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14282 | 99.32 | 31.06 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 170 | 747 | 44.07 | 12.65 | 0.63 |
| 4 | 226 | 858 | 53.63 | 15.27 | 0.73 |
| 5 | 282 | 969 | 64.03 | 18.20 | 0.84 |
| 6 | 337 | 1085 | 71.97 | 20.50 | 0.93 |
| 7 | 395 | 1192 | 72.86 | 21.16 | 0.94 |
| 8 | 451 | 1303 | 81.84 | 23.82 | 1.04 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1786 | 24.29 | 7.69 | 0.48 |
| 2| 1925 | 25.39 | 8.67 | 0.50 |
| 3| 2013 | 26.28 | 9.57 | 0.52 |
| 5| 2317 | 30.08 | 11.98 | 0.58 |
| 10| 2930 | 36.82 | 17.18 | 0.70 |
| 39| 7417 | 95.86 | 52.91 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.54 | 7.30 | 0.41 |
| 2| 740 | 24.08 | 8.40 | 0.44 |
| 3| 897 | 25.72 | 9.52 | 0.47 |
| 5| 1222 | 29.14 | 11.80 | 0.52 |
| 10| 2110 | 42.97 | 19.00 | 0.73 |
| 41| 6574 | 98.92 | 55.16 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 668 | 29.13 | 8.90 | 0.48 |
| 2| 783 | 30.91 | 10.06 | 0.51 |
| 3| 941 | 30.86 | 10.73 | 0.52 |
| 5| 1291 | 35.68 | 13.45 | 0.59 |
| 10| 1952 | 46.80 | 19.84 | 0.76 |
| 36| 5883 | 97.23 | 51.39 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 679 | 33.87 | 10.16 | 0.53 |
| 2| 803 | 35.88 | 11.39 | 0.56 |
| 3| 960 | 37.88 | 12.61 | 0.59 |
| 5| 1310 | 43.32 | 15.49 | 0.67 |
| 10| 1869 | 51.73 | 21.11 | 0.80 |
| 29| 4928 | 98.66 | 46.91 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5782 | 27.16 | 9.11 | 0.69 |
| 2| 5938 | 35.96 | 12.08 | 0.79 |
| 3| 6067 | 45.27 | 15.20 | 0.89 |
| 4| 6318 | 52.08 | 17.56 | 0.98 |
| 5| 6588 | 66.36 | 22.44 | 1.14 |
| 6| 6404 | 62.41 | 20.90 | 1.09 |
| 7| 6829 | 84.98 | 28.69 | 1.34 |
| 8| 6847 | 86.52 | 29.12 | 1.36 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 56 | 5868 | 21.66 | 7.36 | 0.64 |
| 10 | 5 | 285 | 6005 | 28.65 | 10.19 | 0.72 |
| 10 | 10 | 568 | 6173 | 40.83 | 14.90 | 0.86 |
| 10 | 30 | 1711 | 6857 | 79.15 | 30.16 | 1.31 |
| 10 | 39 | 2223 | 7162 | 98.24 | 37.65 | 1.53 |

