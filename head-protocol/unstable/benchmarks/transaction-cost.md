--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-22 04:42:01.390979654 UTC |
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
| 1| 5836 | 10.17 | 3.22 | 0.51 |
| 2| 6038 | 12.41 | 3.92 | 0.54 |
| 3| 6242 | 14.76 | 4.67 | 0.58 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7644 | 28.94 | 9.11 | 0.79 |
| 43| 14282 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10063 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.17 | 9.59 | 0.52 |
| 3 | 171 | 747 | 40.09 | 11.66 | 0.59 |
| 4 | 225 | 858 | 48.07 | 13.93 | 0.68 |
| 5 | 284 | 969 | 62.80 | 17.91 | 0.83 |
| 6 | 339 | 1081 | 65.86 | 19.04 | 0.87 |
| 7 | 395 | 1192 | 81.17 | 23.20 | 1.03 |
| 8 | 450 | 1303 | 92.75 | 26.43 | 1.15 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1803 | 24.37 | 7.71 | 0.48 |
| 2| 1887 | 24.40 | 8.39 | 0.49 |
| 3| 2130 | 28.51 | 10.19 | 0.55 |
| 5| 2498 | 34.07 | 13.10 | 0.63 |
| 10| 3177 | 41.74 | 18.57 | 0.76 |
| 40| 7514 | 95.02 | 53.38 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 606 | 22.84 | 7.37 | 0.41 |
| 2| 792 | 25.13 | 8.68 | 0.45 |
| 3| 973 | 26.68 | 9.80 | 0.48 |
| 5| 1216 | 29.74 | 11.98 | 0.53 |
| 10| 1953 | 38.80 | 17.83 | 0.68 |
| 40| 6502 | 96.00 | 53.74 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 709 | 27.50 | 8.46 | 0.46 |
| 2| 770 | 30.91 | 10.06 | 0.51 |
| 3| 948 | 30.82 | 10.73 | 0.52 |
| 5| 1200 | 36.38 | 13.58 | 0.59 |
| 10| 2018 | 47.89 | 20.19 | 0.77 |
| 34| 5702 | 93.71 | 49.09 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 33.83 | 10.15 | 0.53 |
| 2| 765 | 35.17 | 11.17 | 0.55 |
| 3| 899 | 37.24 | 12.41 | 0.58 |
| 5| 1307 | 43.46 | 15.54 | 0.67 |
| 10| 2013 | 53.31 | 21.58 | 0.83 |
| 29| 4743 | 96.77 | 46.31 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5818 | 27.08 | 9.08 | 0.69 |
| 2| 6032 | 36.98 | 12.46 | 0.80 |
| 3| 6094 | 46.14 | 15.55 | 0.90 |
| 4| 6238 | 53.86 | 18.09 | 0.99 |
| 5| 6289 | 57.01 | 19.11 | 1.03 |
| 6| 6664 | 75.66 | 25.56 | 1.24 |
| 7| 6670 | 81.16 | 27.35 | 1.30 |
| 8| 6838 | 91.51 | 30.88 | 1.41 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.93 | 6.32 | 0.61 |
| 10 | 5 | 285 | 6005 | 29.35 | 10.43 | 0.73 |
| 10 | 20 | 1138 | 6512 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1708 | 6855 | 81.37 | 30.91 | 1.33 |
| 10 | 39 | 2219 | 7158 | 98.49 | 37.73 | 1.53 |

