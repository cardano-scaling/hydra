--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 20:59:19.96867225 UTC |
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
| 2| 6041 | 12.63 | 4.00 | 0.55 |
| 3| 6238 | 14.67 | 4.64 | 0.58 |
| 5| 6641 | 18.84 | 5.95 | 0.64 |
| 10| 7646 | 28.94 | 9.11 | 0.79 |
| 43| 14282 | 98.87 | 30.90 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10037 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 171 | 747 | 40.38 | 11.75 | 0.59 |
| 4 | 226 | 858 | 49.69 | 14.35 | 0.69 |
| 5 | 284 | 969 | 64.64 | 18.38 | 0.85 |
| 6 | 338 | 1081 | 72.02 | 20.55 | 0.93 |
| 7 | 392 | 1192 | 80.37 | 22.87 | 1.02 |
| 8 | 449 | 1303 | 84.41 | 24.27 | 1.06 |
| 9 | 506 | 1418 | 99.01 | 28.18 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 24.00 | 7.62 | 0.48 |
| 2| 1926 | 25.88 | 8.79 | 0.51 |
| 3| 2013 | 26.28 | 9.57 | 0.52 |
| 5| 2453 | 32.04 | 12.53 | 0.61 |
| 10| 3181 | 41.16 | 18.39 | 0.76 |
| 39| 7644 | 99.58 | 53.99 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 22.84 | 7.38 | 0.42 |
| 2| 796 | 24.05 | 8.40 | 0.44 |
| 3| 831 | 24.13 | 9.06 | 0.45 |
| 5| 1148 | 28.04 | 11.49 | 0.51 |
| 10| 2054 | 39.69 | 18.07 | 0.69 |
| 43| 6843 | 98.65 | 56.47 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 664 | 29.17 | 8.91 | 0.48 |
| 2| 873 | 29.90 | 9.82 | 0.50 |
| 3| 1026 | 34.11 | 11.65 | 0.55 |
| 5| 1269 | 35.04 | 13.25 | 0.58 |
| 10| 2007 | 44.78 | 19.34 | 0.74 |
| 37| 6109 | 99.58 | 52.72 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 705 | 33.83 | 10.15 | 0.53 |
| 2| 826 | 35.85 | 11.38 | 0.56 |
| 3| 945 | 37.88 | 12.61 | 0.59 |
| 5| 1314 | 43.39 | 15.50 | 0.67 |
| 10| 2032 | 53.98 | 21.79 | 0.83 |
| 29| 4841 | 98.45 | 46.80 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 27.05 | 9.07 | 0.69 |
| 2| 5823 | 31.48 | 10.47 | 0.74 |
| 3| 5971 | 38.12 | 12.70 | 0.81 |
| 4| 6283 | 54.20 | 18.33 | 1.00 |
| 5| 6410 | 64.47 | 21.74 | 1.11 |
| 6| 6406 | 66.19 | 22.22 | 1.13 |
| 7| 6876 | 85.02 | 28.75 | 1.35 |
| 8| 6684 | 81.70 | 27.43 | 1.30 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6005 | 29.53 | 10.50 | 0.73 |
| 10 | 10 | 568 | 6173 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1136 | 6511 | 59.98 | 22.53 | 1.08 |
| 10 | 40 | 2274 | 7190 | 99.66 | 38.24 | 1.55 |
| 10 | 36 | 2051 | 7060 | 91.90 | 35.16 | 1.46 |

