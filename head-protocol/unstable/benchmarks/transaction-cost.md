--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-15 12:58:58.029680023 UTC |
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
| 1| 5837 | 10.17 | 3.22 | 0.51 |
| 2| 6038 | 12.65 | 4.01 | 0.55 |
| 3| 6240 | 14.50 | 4.58 | 0.58 |
| 5| 6641 | 18.43 | 5.81 | 0.63 |
| 10| 7646 | 28.92 | 9.11 | 0.79 |
| 43| 14279 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2172 | 12.13 | 7.25 | 0.40 |
| 54| 10066 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 170 | 747 | 41.36 | 11.94 | 0.60 |
| 4 | 225 | 858 | 47.98 | 13.96 | 0.68 |
| 5 | 285 | 969 | 55.99 | 16.22 | 0.76 |
| 6 | 339 | 1081 | 64.29 | 18.63 | 0.85 |
| 7 | 393 | 1192 | 73.12 | 21.27 | 0.95 |
| 8 | 449 | 1303 | 87.33 | 25.03 | 1.09 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 24.37 | 7.71 | 0.48 |
| 2| 1926 | 25.88 | 8.79 | 0.51 |
| 3| 2066 | 27.35 | 9.87 | 0.53 |
| 5| 2440 | 32.44 | 12.63 | 0.61 |
| 10| 3123 | 39.67 | 17.99 | 0.74 |
| 40| 7714 | 99.56 | 54.61 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.57 | 7.31 | 0.41 |
| 2| 739 | 23.65 | 8.24 | 0.43 |
| 3| 954 | 26.92 | 9.85 | 0.48 |
| 5| 1188 | 29.89 | 12.02 | 0.53 |
| 10| 1887 | 36.48 | 17.17 | 0.65 |
| 43| 6827 | 98.99 | 56.58 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 27.54 | 8.47 | 0.46 |
| 2| 877 | 29.94 | 9.83 | 0.50 |
| 3| 1040 | 31.50 | 10.93 | 0.53 |
| 5| 1258 | 37.74 | 14.00 | 0.61 |
| 10| 2033 | 47.74 | 20.14 | 0.77 |
| 38| 5988 | 98.06 | 52.92 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 706 | 33.87 | 10.16 | 0.53 |
| 2| 811 | 35.89 | 11.39 | 0.56 |
| 3| 941 | 37.80 | 12.59 | 0.59 |
| 5| 1357 | 44.03 | 15.70 | 0.68 |
| 10| 2105 | 54.78 | 22.02 | 0.84 |
| 29| 4752 | 96.49 | 46.26 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5795 | 27.16 | 9.10 | 0.69 |
| 2| 5937 | 35.96 | 12.08 | 0.79 |
| 3| 6106 | 45.77 | 15.43 | 0.90 |
| 4| 6178 | 51.46 | 17.30 | 0.96 |
| 5| 6450 | 64.90 | 21.86 | 1.12 |
| 6| 6545 | 70.55 | 23.71 | 1.18 |
| 7| 6866 | 87.49 | 29.65 | 1.37 |
| 8| 6922 | 93.41 | 31.45 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.38 | 6.48 | 0.61 |
| 10 | 1 | 56 | 5867 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 284 | 6003 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1137 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6854 | 79.60 | 30.31 | 1.31 |
| 10 | 40 | 2278 | 7194 | 99.66 | 38.24 | 1.55 |
| 10 | 38 | 2164 | 7126 | 97.77 | 37.38 | 1.52 |

