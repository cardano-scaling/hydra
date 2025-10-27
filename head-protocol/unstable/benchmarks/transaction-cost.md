--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-27 13:21:26.170083919 UTC |
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
| 1| 5840 | 10.57 | 3.36 | 0.52 |
| 2| 6041 | 12.63 | 4.00 | 0.55 |
| 3| 6236 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 18.62 | 5.87 | 0.64 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 737 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 33.25 | 9.63 | 0.52 |
| 3 | 170 | 747 | 41.39 | 11.97 | 0.60 |
| 4 | 227 | 858 | 49.70 | 14.35 | 0.69 |
| 5 | 283 | 969 | 64.20 | 18.21 | 0.84 |
| 6 | 341 | 1085 | 73.17 | 20.79 | 0.94 |
| 7 | 396 | 1192 | 76.36 | 21.91 | 0.98 |
| 8 | 450 | 1303 | 83.25 | 24.06 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1806 | 24.37 | 7.71 | 0.48 |
| 2| 1968 | 27.00 | 9.10 | 0.52 |
| 3| 2014 | 25.91 | 9.48 | 0.52 |
| 5| 2412 | 32.04 | 12.53 | 0.61 |
| 10| 3113 | 41.03 | 18.36 | 0.75 |
| 37| 7426 | 95.77 | 51.58 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 630 | 22.54 | 7.30 | 0.41 |
| 2| 767 | 24.35 | 8.46 | 0.44 |
| 3| 1012 | 27.97 | 10.15 | 0.49 |
| 5| 1194 | 29.03 | 11.77 | 0.52 |
| 10| 2101 | 43.62 | 19.16 | 0.73 |
| 40| 6593 | 99.32 | 54.67 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 692 | 27.54 | 8.47 | 0.46 |
| 2| 774 | 28.51 | 9.39 | 0.48 |
| 3| 919 | 32.72 | 11.23 | 0.54 |
| 5| 1298 | 38.06 | 14.09 | 0.62 |
| 10| 2056 | 44.98 | 19.39 | 0.74 |
| 37| 6037 | 98.53 | 52.42 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 671 | 33.87 | 10.16 | 0.53 |
| 2| 855 | 36.64 | 11.62 | 0.57 |
| 3| 896 | 37.13 | 12.38 | 0.58 |
| 5| 1241 | 42.61 | 15.27 | 0.66 |
| 10| 2051 | 54.78 | 22.02 | 0.84 |
| 28| 4772 | 96.99 | 45.78 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 27.09 | 9.10 | 0.69 |
| 2| 5937 | 36.03 | 12.11 | 0.79 |
| 3| 6148 | 47.23 | 15.93 | 0.92 |
| 4| 6093 | 49.36 | 16.49 | 0.94 |
| 5| 6361 | 60.41 | 20.28 | 1.06 |
| 6| 6611 | 74.52 | 25.12 | 1.22 |
| 7| 6644 | 80.00 | 26.86 | 1.28 |
| 8| 6722 | 83.29 | 28.02 | 1.32 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1139 | 6514 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1710 | 6856 | 80.48 | 30.61 | 1.32 |
| 10 | 38 | 2163 | 7125 | 96.19 | 36.84 | 1.51 |

