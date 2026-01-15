--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-15 12:41:56.011296643 UTC |
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
| 1| 5836 | 10.19 | 3.22 | 0.51 |
| 2| 6038 | 12.25 | 3.87 | 0.54 |
| 3| 6239 | 14.88 | 4.72 | 0.58 |
| 5| 6641 | 18.90 | 5.97 | 0.64 |
| 10| 7646 | 28.94 | 9.11 | 0.79 |
| 43| 14281 | 98.87 | 30.90 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 915 | 4.36 | 2.33 | 0.24 |
| 5| 1275 | 6.41 | 3.60 | 0.28 |
| 10| 2167 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.18 | 9.60 | 0.52 |
| 3 | 171 | 751 | 40.25 | 11.70 | 0.59 |
| 4 | 226 | 858 | 52.10 | 14.92 | 0.72 |
| 5 | 282 | 969 | 59.61 | 17.09 | 0.80 |
| 6 | 341 | 1081 | 71.38 | 20.36 | 0.92 |
| 7 | 394 | 1192 | 82.43 | 23.37 | 1.04 |
| 8 | 449 | 1303 | 82.85 | 23.86 | 1.05 |
| 9 | 506 | 1414 | 92.12 | 26.64 | 1.15 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 24.29 | 7.69 | 0.48 |
| 2| 1880 | 24.40 | 8.40 | 0.49 |
| 3| 2127 | 28.39 | 10.16 | 0.55 |
| 5| 2370 | 31.25 | 12.30 | 0.60 |
| 10| 3226 | 41.95 | 18.64 | 0.77 |
| 41| 7786 | 98.14 | 54.97 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.84 | 7.39 | 0.42 |
| 2| 698 | 22.62 | 7.95 | 0.42 |
| 3| 942 | 26.67 | 9.80 | 0.48 |
| 5| 1229 | 29.94 | 12.03 | 0.53 |
| 10| 2082 | 41.79 | 18.66 | 0.71 |
| 43| 6784 | 96.60 | 55.91 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 696 | 27.54 | 8.47 | 0.46 |
| 2| 830 | 29.18 | 9.60 | 0.49 |
| 3| 1020 | 34.03 | 11.63 | 0.55 |
| 5| 1415 | 36.38 | 13.66 | 0.60 |
| 10| 1951 | 44.26 | 19.17 | 0.73 |
| 34| 5545 | 92.67 | 48.75 | 1.49 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 33.87 | 10.16 | 0.53 |
| 2| 798 | 35.85 | 11.38 | 0.56 |
| 3| 1022 | 38.59 | 12.82 | 0.60 |
| 5| 1244 | 42.49 | 15.24 | 0.66 |
| 10| 2002 | 53.34 | 21.59 | 0.82 |
| 28| 4847 | 97.04 | 45.79 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5802 | 27.13 | 9.11 | 0.69 |
| 2| 5966 | 35.80 | 12.03 | 0.79 |
| 3| 6190 | 46.99 | 15.91 | 0.92 |
| 4| 6165 | 53.92 | 18.14 | 0.99 |
| 5| 6429 | 62.94 | 21.24 | 1.09 |
| 6| 6562 | 70.91 | 23.86 | 1.18 |
| 7| 6656 | 79.61 | 26.81 | 1.28 |
| 8| 6998 | 95.03 | 32.07 | 1.46 |
| 9| 6916 | 95.26 | 32.07 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 10 | 570 | 6174 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1138 | 6513 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1706 | 6853 | 79.78 | 30.37 | 1.32 |
| 10 | 39 | 2222 | 7161 | 99.82 | 38.19 | 1.55 |

