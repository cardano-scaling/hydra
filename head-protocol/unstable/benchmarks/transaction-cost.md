--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-23 04:41:38.956324323 UTC |
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
| 1| 5836 | 10.64 | 3.38 | 0.52 |
| 2| 6037 | 12.25 | 3.87 | 0.54 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 18.62 | 5.87 | 0.64 |
| 10| 7644 | 29.38 | 9.27 | 0.79 |
| 43| 14283 | 98.73 | 30.85 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10073 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 636 | 33.32 | 9.64 | 0.52 |
| 3 | 168 | 747 | 41.61 | 12.04 | 0.61 |
| 4 | 227 | 858 | 51.13 | 14.69 | 0.71 |
| 5 | 283 | 969 | 64.05 | 18.15 | 0.84 |
| 6 | 339 | 1081 | 72.89 | 20.65 | 0.94 |
| 7 | 394 | 1192 | 84.89 | 24.00 | 1.06 |
| 8 | 449 | 1303 | 82.47 | 23.77 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1812 | 24.00 | 7.62 | 0.48 |
| 2| 1953 | 25.39 | 8.68 | 0.50 |
| 3| 2013 | 26.28 | 9.57 | 0.52 |
| 5| 2375 | 31.04 | 12.25 | 0.59 |
| 10| 3136 | 40.89 | 18.32 | 0.75 |
| 38| 7183 | 92.65 | 51.37 | 1.59 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.57 | 7.32 | 0.41 |
| 2| 786 | 25.39 | 8.76 | 0.45 |
| 3| 916 | 26.60 | 9.78 | 0.48 |
| 5| 1301 | 32.40 | 12.70 | 0.56 |
| 10| 2077 | 40.43 | 18.28 | 0.70 |
| 41| 6619 | 98.82 | 55.20 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 648 | 29.17 | 8.91 | 0.48 |
| 2| 844 | 31.62 | 10.28 | 0.52 |
| 3| 906 | 32.69 | 11.22 | 0.54 |
| 5| 1378 | 36.44 | 13.67 | 0.60 |
| 10| 2003 | 47.36 | 20.02 | 0.77 |
| 35| 5754 | 94.28 | 49.92 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 33.87 | 10.16 | 0.53 |
| 2| 764 | 35.17 | 11.17 | 0.55 |
| 3| 891 | 37.20 | 12.40 | 0.58 |
| 5| 1245 | 42.65 | 15.28 | 0.66 |
| 10| 2030 | 54.28 | 21.88 | 0.84 |
| 28| 4817 | 95.53 | 45.37 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5782 | 27.00 | 9.08 | 0.69 |
| 2| 6011 | 37.00 | 12.47 | 0.80 |
| 3| 6122 | 44.71 | 15.06 | 0.89 |
| 4| 6199 | 50.21 | 16.83 | 0.95 |
| 5| 6455 | 63.66 | 21.43 | 1.10 |
| 6| 6467 | 69.17 | 23.23 | 1.16 |
| 7| 6870 | 86.88 | 29.38 | 1.37 |
| 8| 6945 | 93.16 | 31.43 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.02 | 9.98 | 0.71 |
| 10 | 10 | 568 | 6172 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1139 | 6514 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1708 | 6855 | 80.92 | 30.76 | 1.33 |
| 10 | 39 | 2220 | 7160 | 98.93 | 37.88 | 1.54 |

