--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-19 04:49:49.796342426 UTC |
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
| 1| 5841 | 10.61 | 3.37 | 0.52 |
| 2| 6035 | 12.23 | 3.86 | 0.54 |
| 3| 6236 | 14.59 | 4.61 | 0.58 |
| 5| 6641 | 19.00 | 6.01 | 0.64 |
| 10| 7651 | 29.38 | 9.27 | 0.79 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10054 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.24 | 9.37 | 0.51 |
| 3 | 170 | 747 | 41.29 | 11.92 | 0.60 |
| 4 | 225 | 858 | 50.89 | 14.61 | 0.70 |
| 5 | 282 | 969 | 59.24 | 17.02 | 0.79 |
| 6 | 337 | 1081 | 73.38 | 20.80 | 0.94 |
| 7 | 395 | 1192 | 76.40 | 21.92 | 0.98 |
| 8 | 449 | 1303 | 89.00 | 25.33 | 1.11 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1817 | 23.92 | 7.60 | 0.48 |
| 2| 1943 | 25.47 | 8.70 | 0.50 |
| 3| 2058 | 27.39 | 9.88 | 0.53 |
| 5| 2451 | 32.23 | 12.58 | 0.61 |
| 10| 3227 | 42.19 | 18.68 | 0.77 |
| 41| 7776 | 99.64 | 55.35 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 615 | 22.80 | 7.37 | 0.41 |
| 2| 742 | 23.65 | 8.25 | 0.43 |
| 3| 872 | 25.51 | 9.46 | 0.46 |
| 5| 1204 | 29.54 | 11.93 | 0.53 |
| 10| 2034 | 39.54 | 18.03 | 0.69 |
| 40| 6475 | 95.67 | 53.65 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 27.51 | 8.47 | 0.46 |
| 2| 778 | 30.87 | 10.05 | 0.51 |
| 3| 910 | 32.72 | 11.23 | 0.54 |
| 5| 1290 | 35.08 | 13.26 | 0.59 |
| 10| 1952 | 44.19 | 19.15 | 0.73 |
| 37| 6078 | 99.25 | 52.63 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 33.83 | 10.16 | 0.53 |
| 2| 761 | 35.14 | 11.16 | 0.55 |
| 3| 896 | 37.16 | 12.39 | 0.58 |
| 5| 1298 | 43.36 | 15.49 | 0.67 |
| 10| 2127 | 55.26 | 22.16 | 0.85 |
| 29| 4943 | 99.92 | 47.26 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5814 | 26.97 | 9.06 | 0.69 |
| 2| 6022 | 36.97 | 12.45 | 0.80 |
| 3| 6234 | 46.91 | 15.85 | 0.92 |
| 4| 6198 | 50.33 | 16.84 | 0.95 |
| 5| 6366 | 64.02 | 21.55 | 1.10 |
| 6| 6395 | 68.92 | 23.08 | 1.15 |
| 7| 6717 | 79.71 | 26.79 | 1.28 |
| 8| 6968 | 95.85 | 32.39 | 1.46 |
| 9| 6890 | 99.26 | 33.30 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6003 | 28.46 | 10.13 | 0.72 |
| 10 | 20 | 1136 | 6510 | 59.54 | 22.38 | 1.08 |
| 10 | 39 | 2219 | 7158 | 99.38 | 38.04 | 1.54 |

