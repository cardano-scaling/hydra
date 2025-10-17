--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-17 04:41:38.135327228 UTC |
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
| 1| 5836 | 10.47 | 3.32 | 0.52 |
| 2| 6037 | 13.10 | 4.17 | 0.55 |
| 3| 6239 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 98.66 | 30.82 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10065 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 34.31 | 9.88 | 0.53 |
| 3 | 169 | 747 | 43.84 | 12.56 | 0.63 |
| 4 | 226 | 858 | 49.41 | 14.28 | 0.69 |
| 5 | 283 | 969 | 55.96 | 16.24 | 0.76 |
| 6 | 337 | 1081 | 75.53 | 21.32 | 0.96 |
| 7 | 396 | 1192 | 76.75 | 22.09 | 0.98 |
| 8 | 449 | 1303 | 80.99 | 23.56 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1824 | 23.92 | 7.60 | 0.48 |
| 2| 2013 | 26.50 | 8.99 | 0.52 |
| 3| 2130 | 28.02 | 10.07 | 0.54 |
| 5| 2443 | 31.95 | 12.51 | 0.61 |
| 10| 3019 | 38.40 | 17.64 | 0.72 |
| 39| 7465 | 95.57 | 52.87 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 640 | 22.54 | 7.31 | 0.41 |
| 2| 786 | 24.28 | 8.45 | 0.44 |
| 3| 873 | 25.09 | 9.33 | 0.46 |
| 5| 1224 | 29.08 | 11.77 | 0.52 |
| 10| 2114 | 40.79 | 18.39 | 0.71 |
| 41| 6538 | 97.22 | 54.78 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 694 | 27.50 | 8.46 | 0.46 |
| 2| 802 | 30.91 | 10.06 | 0.51 |
| 3| 1003 | 33.47 | 11.45 | 0.55 |
| 5| 1186 | 36.31 | 13.56 | 0.59 |
| 10| 2094 | 45.57 | 19.57 | 0.75 |
| 37| 6108 | 99.98 | 52.83 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 33.83 | 10.15 | 0.53 |
| 2| 807 | 35.85 | 11.38 | 0.56 |
| 3| 945 | 37.91 | 12.62 | 0.59 |
| 5| 1406 | 43.91 | 15.67 | 0.68 |
| 10| 2032 | 54.17 | 21.85 | 0.83 |
| 29| 4965 | 99.84 | 47.24 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5815 | 27.00 | 9.09 | 0.69 |
| 2| 5893 | 34.87 | 11.68 | 0.78 |
| 3| 6016 | 43.99 | 14.73 | 0.88 |
| 4| 6305 | 55.13 | 18.54 | 1.01 |
| 5| 6346 | 60.41 | 20.31 | 1.06 |
| 6| 6552 | 70.72 | 23.78 | 1.18 |
| 7| 6627 | 81.76 | 27.46 | 1.30 |
| 8| 6815 | 89.80 | 30.25 | 1.39 |
| 9| 6887 | 97.45 | 32.75 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5869 | 20.78 | 7.06 | 0.63 |
| 10 | 10 | 570 | 6174 | 40.39 | 14.75 | 0.85 |
| 10 | 20 | 1139 | 6513 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1706 | 6853 | 80.48 | 30.61 | 1.32 |
| 10 | 38 | 2161 | 7123 | 97.33 | 37.23 | 1.52 |

