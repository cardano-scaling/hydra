--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-30 13:46:47.15838063 UTC |
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
| 1| 5836 | 10.26 | 3.25 | 0.51 |
| 2| 6041 | 12.25 | 3.87 | 0.54 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6640 | 18.72 | 5.91 | 0.64 |
| 10| 7646 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.87 | 30.90 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2183 | 12.13 | 7.25 | 0.40 |
| 54| 10067 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.18 | 9.60 | 0.52 |
| 3 | 171 | 747 | 40.09 | 11.64 | 0.59 |
| 4 | 227 | 858 | 48.42 | 14.07 | 0.68 |
| 5 | 284 | 969 | 64.27 | 18.26 | 0.84 |
| 6 | 337 | 1085 | 74.81 | 21.10 | 0.95 |
| 7 | 393 | 1192 | 82.88 | 23.52 | 1.04 |
| 8 | 450 | 1303 | 96.27 | 27.12 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1800 | 24.00 | 7.62 | 0.48 |
| 2| 1924 | 25.76 | 8.76 | 0.51 |
| 3| 2132 | 28.43 | 10.17 | 0.55 |
| 5| 2540 | 33.79 | 13.03 | 0.63 |
| 10| 3156 | 41.18 | 18.40 | 0.75 |
| 39| 7337 | 94.46 | 52.53 | 1.61 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 610 | 22.53 | 7.30 | 0.41 |
| 2| 703 | 22.55 | 7.95 | 0.42 |
| 3| 897 | 25.52 | 9.47 | 0.46 |
| 5| 1277 | 30.64 | 12.24 | 0.54 |
| 10| 1915 | 38.74 | 17.80 | 0.68 |
| 43| 6875 | 99.22 | 56.63 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 699 | 27.47 | 8.46 | 0.46 |
| 2| 811 | 30.98 | 10.08 | 0.51 |
| 3| 964 | 33.47 | 11.45 | 0.55 |
| 5| 1293 | 35.56 | 13.42 | 0.59 |
| 10| 1950 | 43.52 | 18.94 | 0.72 |
| 35| 5655 | 94.29 | 49.85 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 33.83 | 10.16 | 0.53 |
| 2| 820 | 35.92 | 11.40 | 0.56 |
| 3| 937 | 37.88 | 12.61 | 0.59 |
| 5| 1242 | 42.53 | 15.25 | 0.66 |
| 10| 2028 | 53.86 | 21.76 | 0.83 |
| 29| 4920 | 98.48 | 46.86 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5790 | 27.13 | 9.10 | 0.69 |
| 2| 5935 | 35.83 | 12.03 | 0.79 |
| 3| 6140 | 45.62 | 15.38 | 0.90 |
| 4| 6216 | 52.57 | 17.71 | 0.98 |
| 5| 6468 | 64.88 | 21.94 | 1.12 |
| 6| 6458 | 71.70 | 24.10 | 1.19 |
| 7| 6675 | 77.21 | 26.00 | 1.26 |
| 8| 6705 | 86.74 | 29.10 | 1.36 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 10 | 568 | 6172 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6512 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1703 | 6849 | 81.11 | 30.83 | 1.33 |
| 10 | 37 | 2107 | 7092 | 94.39 | 36.12 | 1.49 |

