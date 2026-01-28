--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-28 09:35:10.690815737 UTC |
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
| 1| 5834 | 10.47 | 3.32 | 0.52 |
| 2| 6037 | 13.01 | 4.14 | 0.55 |
| 3| 6238 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 18.84 | 5.95 | 0.64 |
| 10| 7646 | 29.38 | 9.27 | 0.79 |
| 43| 14279 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10068 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.20 | 9.84 | 0.53 |
| 3 | 170 | 747 | 43.48 | 12.44 | 0.62 |
| 4 | 226 | 858 | 52.37 | 14.96 | 0.72 |
| 5 | 281 | 969 | 59.55 | 17.13 | 0.80 |
| 6 | 338 | 1081 | 72.32 | 20.63 | 0.93 |
| 7 | 393 | 1192 | 86.12 | 24.29 | 1.07 |
| 8 | 450 | 1303 | 81.38 | 23.65 | 1.04 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 24.00 | 7.62 | 0.48 |
| 2| 1940 | 25.76 | 8.76 | 0.51 |
| 3| 2013 | 26.32 | 9.58 | 0.52 |
| 5| 2332 | 29.93 | 11.94 | 0.58 |
| 10| 3038 | 38.56 | 17.68 | 0.72 |
| 40| 7642 | 97.80 | 54.15 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.80 | 7.38 | 0.42 |
| 2| 759 | 23.98 | 8.37 | 0.44 |
| 3| 917 | 25.49 | 9.46 | 0.46 |
| 5| 1244 | 31.35 | 12.41 | 0.55 |
| 10| 2060 | 41.52 | 18.58 | 0.71 |
| 41| 6592 | 95.92 | 54.39 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 27.51 | 8.47 | 0.46 |
| 2| 823 | 29.26 | 9.62 | 0.49 |
| 3| 966 | 33.40 | 11.44 | 0.55 |
| 5| 1404 | 36.40 | 13.66 | 0.60 |
| 10| 2029 | 47.67 | 20.12 | 0.77 |
| 35| 5897 | 95.93 | 50.39 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 687 | 33.83 | 10.16 | 0.53 |
| 2| 885 | 36.64 | 11.62 | 0.57 |
| 3| 967 | 37.95 | 12.63 | 0.59 |
| 5| 1340 | 43.43 | 15.51 | 0.67 |
| 10| 2050 | 53.98 | 21.79 | 0.83 |
| 29| 4846 | 97.55 | 46.56 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5820 | 26.97 | 9.07 | 0.69 |
| 2| 5937 | 35.84 | 12.04 | 0.79 |
| 3| 6133 | 45.62 | 15.39 | 0.90 |
| 4| 6399 | 57.16 | 19.31 | 1.03 |
| 5| 6429 | 64.19 | 21.63 | 1.11 |
| 6| 6598 | 72.12 | 24.33 | 1.20 |
| 7| 6797 | 84.48 | 28.56 | 1.34 |
| 8| 6985 | 94.51 | 32.03 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 17.86 | 5.96 | 0.59 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6003 | 28.02 | 9.98 | 0.71 |
| 10 | 10 | 570 | 6175 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1140 | 6514 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1705 | 6852 | 81.11 | 30.83 | 1.33 |
| 10 | 40 | 2278 | 7194 | 99.84 | 38.30 | 1.55 |
| 10 | 38 | 2164 | 7126 | 96.00 | 36.77 | 1.50 |

