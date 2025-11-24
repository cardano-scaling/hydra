--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-24 09:47:27.170271748 UTC |
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
| 1| 5834 | 10.85 | 3.45 | 0.52 |
| 2| 6035 | 12.80 | 4.07 | 0.55 |
| 3| 6239 | 14.29 | 4.51 | 0.57 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7646 | 29.18 | 9.20 | 0.79 |
| 43| 14279 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 171 | 747 | 41.07 | 11.87 | 0.60 |
| 4 | 228 | 862 | 47.87 | 13.89 | 0.68 |
| 5 | 283 | 969 | 56.28 | 16.32 | 0.77 |
| 6 | 338 | 1081 | 72.03 | 20.52 | 0.93 |
| 7 | 392 | 1192 | 72.27 | 20.89 | 0.94 |
| 8 | 449 | 1303 | 81.07 | 23.48 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1796 | 24.37 | 7.71 | 0.48 |
| 2| 1989 | 26.47 | 8.98 | 0.52 |
| 3| 2163 | 29.38 | 10.44 | 0.56 |
| 5| 2427 | 32.45 | 12.63 | 0.61 |
| 10| 3201 | 42.33 | 18.74 | 0.77 |
| 42| 7709 | 98.69 | 55.71 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.81 | 7.37 | 0.42 |
| 2| 738 | 24.31 | 8.45 | 0.44 |
| 3| 1011 | 27.69 | 10.07 | 0.49 |
| 5| 1239 | 31.14 | 12.35 | 0.55 |
| 10| 2031 | 39.47 | 18.01 | 0.69 |
| 44| 6923 | 99.96 | 57.48 | 1.68 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 29.13 | 8.90 | 0.48 |
| 2| 774 | 30.98 | 10.08 | 0.51 |
| 3| 989 | 33.40 | 11.44 | 0.55 |
| 5| 1279 | 37.81 | 14.01 | 0.61 |
| 10| 2007 | 44.29 | 19.18 | 0.73 |
| 39| 6152 | 99.11 | 53.87 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.15 | 0.53 |
| 2| 850 | 36.60 | 11.61 | 0.57 |
| 3| 980 | 38.59 | 12.82 | 0.60 |
| 5| 1327 | 43.32 | 15.49 | 0.67 |
| 10| 1907 | 52.45 | 21.33 | 0.81 |
| 29| 4863 | 97.89 | 46.68 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5820 | 27.13 | 9.12 | 0.69 |
| 2| 6016 | 37.08 | 12.49 | 0.80 |
| 3| 6080 | 44.60 | 14.98 | 0.89 |
| 4| 6299 | 54.86 | 18.47 | 1.00 |
| 5| 6388 | 65.00 | 21.94 | 1.11 |
| 6| 6479 | 72.50 | 24.36 | 1.20 |
| 7| 6618 | 79.91 | 26.81 | 1.28 |
| 8| 6829 | 89.39 | 30.05 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 285 | 6005 | 29.09 | 10.34 | 0.72 |
| 10 | 20 | 1139 | 6514 | 59.73 | 22.44 | 1.08 |
| 10 | 39 | 2218 | 7158 | 98.49 | 37.73 | 1.53 |

