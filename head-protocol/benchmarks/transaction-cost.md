--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-01 11:40:10.499212635 UTC |
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
| 1| 5836 | 10.36 | 3.28 | 0.51 |
| 2| 6037 | 12.44 | 3.94 | 0.54 |
| 3| 6236 | 14.47 | 4.57 | 0.57 |
| 5| 6641 | 19.36 | 6.14 | 0.64 |
| 10| 7644 | 29.14 | 9.19 | 0.79 |
| 43| 14282 | 98.85 | 30.89 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10049 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 34.30 | 9.88 | 0.53 |
| 3 | 171 | 747 | 41.34 | 11.94 | 0.60 |
| 4 | 225 | 858 | 53.82 | 15.31 | 0.73 |
| 5 | 282 | 974 | 60.55 | 17.33 | 0.81 |
| 6 | 337 | 1081 | 68.04 | 19.53 | 0.89 |
| 7 | 393 | 1196 | 82.49 | 23.38 | 1.04 |
| 8 | 448 | 1303 | 80.58 | 23.46 | 1.03 |
| 9 | 508 | 1418 | 93.65 | 26.90 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1812 | 24.00 | 7.62 | 0.48 |
| 2| 1925 | 25.80 | 8.77 | 0.51 |
| 3| 2057 | 27.32 | 9.86 | 0.53 |
| 5| 2346 | 30.34 | 12.04 | 0.59 |
| 10| 3164 | 40.92 | 18.33 | 0.75 |
| 39| 7435 | 95.86 | 52.93 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 22.84 | 7.39 | 0.42 |
| 2| 743 | 24.27 | 8.45 | 0.44 |
| 3| 944 | 26.60 | 9.77 | 0.48 |
| 5| 1181 | 28.08 | 11.49 | 0.51 |
| 10| 1966 | 38.30 | 17.69 | 0.67 |
| 46| 7046 | 98.81 | 58.51 | 1.68 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 640 | 29.17 | 8.91 | 0.48 |
| 2| 779 | 30.91 | 10.06 | 0.51 |
| 3| 902 | 30.15 | 10.52 | 0.51 |
| 5| 1256 | 37.62 | 13.97 | 0.61 |
| 10| 2019 | 48.00 | 20.22 | 0.77 |
| 37| 6022 | 99.58 | 52.70 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 33.15 | 9.95 | 0.52 |
| 2| 817 | 35.92 | 11.40 | 0.56 |
| 3| 1004 | 38.51 | 12.80 | 0.60 |
| 5| 1245 | 42.49 | 15.24 | 0.66 |
| 10| 2110 | 54.65 | 21.99 | 0.84 |
| 29| 4757 | 97.28 | 46.45 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5836 | 27.08 | 9.09 | 0.69 |
| 2| 5937 | 35.99 | 12.09 | 0.79 |
| 3| 6017 | 43.83 | 14.67 | 0.87 |
| 4| 6278 | 55.30 | 18.70 | 1.01 |
| 5| 6403 | 63.86 | 21.51 | 1.10 |
| 6| 6594 | 74.24 | 25.02 | 1.22 |
| 7| 6701 | 82.81 | 27.93 | 1.32 |
| 8| 6953 | 94.28 | 31.86 | 1.45 |
| 9| 7022 | 98.47 | 33.23 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5868 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 283 | 6002 | 29.53 | 10.50 | 0.73 |
| 10 | 10 | 568 | 6172 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1139 | 6513 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1706 | 6852 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2221 | 7161 | 98.93 | 37.88 | 1.54 |

