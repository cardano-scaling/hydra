--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-01 05:13:43.060290617 UTC |
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
| 1| 5837 | 10.19 | 3.22 | 0.51 |
| 2| 6042 | 12.72 | 4.03 | 0.55 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6641 | 18.62 | 5.87 | 0.64 |
| 10| 7647 | 29.11 | 9.17 | 0.79 |
| 43| 14285 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10061 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 640 | 34.34 | 9.90 | 0.53 |
| 3 | 171 | 747 | 43.72 | 12.52 | 0.63 |
| 4 | 226 | 858 | 52.66 | 15.08 | 0.72 |
| 5 | 283 | 969 | 62.81 | 17.88 | 0.83 |
| 6 | 339 | 1081 | 73.01 | 20.68 | 0.94 |
| 7 | 394 | 1192 | 76.78 | 22.02 | 0.98 |
| 8 | 450 | 1307 | 83.45 | 24.10 | 1.06 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 24.29 | 7.69 | 0.48 |
| 2| 1966 | 26.46 | 8.97 | 0.52 |
| 3| 2053 | 27.36 | 9.87 | 0.53 |
| 5| 2443 | 32.44 | 12.63 | 0.61 |
| 10| 3172 | 41.20 | 18.42 | 0.76 |
| 42| 7724 | 97.57 | 55.44 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 643 | 22.81 | 7.37 | 0.42 |
| 2| 768 | 23.63 | 8.24 | 0.43 |
| 3| 831 | 24.02 | 9.02 | 0.45 |
| 5| 1299 | 30.71 | 12.25 | 0.54 |
| 10| 2052 | 39.54 | 18.03 | 0.69 |
| 44| 6868 | 99.61 | 57.41 | 1.67 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 27.54 | 8.47 | 0.46 |
| 2| 883 | 31.62 | 10.28 | 0.52 |
| 3| 1037 | 34.19 | 11.67 | 0.56 |
| 5| 1325 | 35.56 | 13.42 | 0.59 |
| 10| 1936 | 46.87 | 19.86 | 0.76 |
| 36| 5766 | 94.06 | 50.43 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 33.87 | 10.16 | 0.53 |
| 2| 858 | 36.64 | 11.62 | 0.57 |
| 3| 937 | 37.80 | 12.59 | 0.59 |
| 5| 1265 | 42.64 | 15.28 | 0.66 |
| 10| 2172 | 55.22 | 22.17 | 0.85 |
| 30| 4917 | 98.93 | 47.58 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5698 | 22.97 | 7.57 | 0.64 |
| 2| 6012 | 37.03 | 12.49 | 0.80 |
| 3| 6084 | 45.00 | 15.10 | 0.89 |
| 4| 6275 | 54.99 | 18.52 | 1.00 |
| 5| 6412 | 63.70 | 21.45 | 1.10 |
| 6| 6439 | 68.61 | 22.95 | 1.15 |
| 7| 6544 | 75.43 | 25.22 | 1.23 |
| 8| 6778 | 88.02 | 29.66 | 1.37 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6005 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 569 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 30 | 1708 | 6854 | 81.37 | 30.91 | 1.33 |
| 10 | 37 | 2108 | 7094 | 95.28 | 36.42 | 1.49 |

