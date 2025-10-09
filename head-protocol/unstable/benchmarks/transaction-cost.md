--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-09 12:50:15.080788438 UTC |
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
| 1| 5837 | 10.40 | 3.30 | 0.51 |
| 2| 6038 | 12.82 | 4.07 | 0.55 |
| 3| 6236 | 14.84 | 4.71 | 0.58 |
| 5| 6638 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 28.88 | 9.10 | 0.79 |
| 43| 14281 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 916 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 33.33 | 9.64 | 0.52 |
| 3 | 171 | 747 | 39.94 | 11.60 | 0.59 |
| 4 | 227 | 858 | 51.32 | 14.76 | 0.71 |
| 5 | 282 | 969 | 59.58 | 17.14 | 0.80 |
| 6 | 339 | 1081 | 73.30 | 20.82 | 0.94 |
| 7 | 396 | 1192 | 86.54 | 24.35 | 1.08 |
| 8 | 450 | 1303 | 91.46 | 25.87 | 1.13 |
| 9 | 504 | 1414 | 94.12 | 27.06 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1795 | 24.37 | 7.71 | 0.48 |
| 2| 1938 | 25.51 | 8.70 | 0.50 |
| 3| 2126 | 28.13 | 10.10 | 0.54 |
| 5| 2434 | 32.11 | 12.55 | 0.61 |
| 10| 3095 | 40.04 | 18.08 | 0.74 |
| 40| 7437 | 95.28 | 53.42 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 616 | 22.84 | 7.37 | 0.42 |
| 2| 807 | 25.20 | 8.71 | 0.45 |
| 3| 831 | 24.13 | 9.06 | 0.45 |
| 5| 1244 | 31.21 | 12.38 | 0.55 |
| 10| 1850 | 37.11 | 17.36 | 0.66 |
| 40| 6375 | 94.60 | 53.37 | 1.58 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 684 | 27.50 | 8.46 | 0.46 |
| 2| 800 | 30.94 | 10.07 | 0.51 |
| 3| 955 | 30.82 | 10.73 | 0.52 |
| 5| 1272 | 37.69 | 13.98 | 0.61 |
| 10| 2056 | 48.22 | 20.27 | 0.78 |
| 35| 5689 | 93.04 | 49.52 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 807 | 35.89 | 11.39 | 0.56 |
| 3| 1030 | 39.34 | 13.05 | 0.61 |
| 5| 1389 | 44.03 | 15.70 | 0.68 |
| 10| 2100 | 54.77 | 22.02 | 0.84 |
| 29| 4994 | 98.86 | 47.00 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5779 | 27.13 | 9.09 | 0.69 |
| 2| 5989 | 35.91 | 12.07 | 0.79 |
| 3| 6134 | 45.84 | 15.45 | 0.90 |
| 4| 6323 | 55.85 | 18.89 | 1.02 |
| 5| 6529 | 66.03 | 22.37 | 1.13 |
| 6| 6522 | 73.37 | 24.72 | 1.21 |
| 7| 6558 | 77.38 | 25.95 | 1.25 |
| 8| 6797 | 84.36 | 28.35 | 1.34 |
| 9| 6851 | 95.34 | 32.01 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.23 | 6.09 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 284 | 6004 | 29.79 | 10.58 | 0.73 |
| 10 | 30 | 1709 | 6855 | 79.15 | 30.16 | 1.31 |
| 10 | 39 | 2219 | 7159 | 99.12 | 37.95 | 1.54 |

