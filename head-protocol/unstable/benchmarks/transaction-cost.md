--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-06 12:51:19.672988856 UTC |
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
| 2| 6038 | 12.65 | 4.01 | 0.55 |
| 3| 6239 | 14.47 | 4.57 | 0.57 |
| 5| 6641 | 18.79 | 5.94 | 0.64 |
| 10| 7646 | 29.31 | 9.25 | 0.79 |
| 43| 14286 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10065 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.31 | 9.88 | 0.53 |
| 3 | 171 | 747 | 43.48 | 12.44 | 0.62 |
| 4 | 226 | 858 | 51.00 | 14.64 | 0.71 |
| 5 | 283 | 969 | 64.59 | 18.31 | 0.85 |
| 6 | 339 | 1081 | 64.30 | 18.63 | 0.85 |
| 7 | 394 | 1192 | 82.62 | 23.41 | 1.04 |
| 8 | 448 | 1303 | 81.05 | 23.53 | 1.03 |
| 10 | 562 | 1525 | 97.80 | 28.29 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1818 | 24.00 | 7.62 | 0.48 |
| 2| 1883 | 24.43 | 8.40 | 0.49 |
| 3| 2014 | 25.87 | 9.47 | 0.52 |
| 5| 2432 | 32.33 | 12.60 | 0.61 |
| 10| 3035 | 38.78 | 17.74 | 0.73 |
| 41| 7623 | 98.02 | 54.89 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 22.77 | 7.36 | 0.42 |
| 2| 722 | 22.60 | 7.95 | 0.42 |
| 3| 853 | 24.07 | 9.03 | 0.45 |
| 5| 1132 | 28.11 | 11.49 | 0.51 |
| 10| 1855 | 36.70 | 17.23 | 0.65 |
| 41| 6588 | 98.16 | 54.99 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 656 | 29.09 | 8.89 | 0.48 |
| 2| 800 | 29.19 | 9.60 | 0.49 |
| 3| 953 | 33.43 | 11.44 | 0.54 |
| 5| 1238 | 36.99 | 13.77 | 0.60 |
| 10| 2210 | 47.21 | 20.08 | 0.77 |
| 37| 6076 | 98.80 | 52.52 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.16 | 0.53 |
| 2| 807 | 35.88 | 11.39 | 0.56 |
| 3| 947 | 37.88 | 12.61 | 0.59 |
| 5| 1389 | 44.33 | 15.79 | 0.68 |
| 10| 2015 | 53.98 | 21.79 | 0.83 |
| 29| 5032 | 99.50 | 47.16 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5811 | 26.97 | 9.06 | 0.69 |
| 2| 5823 | 31.52 | 10.49 | 0.74 |
| 3| 6034 | 42.50 | 14.24 | 0.86 |
| 4| 6353 | 55.64 | 18.78 | 1.01 |
| 5| 6483 | 60.85 | 20.54 | 1.08 |
| 6| 6618 | 74.00 | 24.97 | 1.22 |
| 7| 6660 | 79.18 | 26.67 | 1.28 |
| 8| 6777 | 91.64 | 30.89 | 1.41 |
| 9| 6852 | 97.26 | 32.63 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.63 | 6.56 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 285 | 6004 | 29.79 | 10.58 | 0.73 |
| 10 | 20 | 1134 | 6509 | 60.17 | 22.59 | 1.09 |
| 10 | 30 | 1708 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 40 | 2276 | 7192 | 99.66 | 38.24 | 1.55 |

