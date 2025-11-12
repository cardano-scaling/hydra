--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-12 04:50:43.280034396 UTC |
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
| 1| 5836 | 10.38 | 3.29 | 0.51 |
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6242 | 14.69 | 4.65 | 0.58 |
| 5| 6640 | 18.50 | 5.83 | 0.63 |
| 10| 7647 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10066 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 33.25 | 9.62 | 0.52 |
| 3 | 171 | 747 | 43.90 | 12.57 | 0.63 |
| 4 | 226 | 858 | 52.44 | 15.00 | 0.72 |
| 5 | 283 | 969 | 59.26 | 17.03 | 0.79 |
| 6 | 338 | 1085 | 64.66 | 18.76 | 0.86 |
| 7 | 393 | 1192 | 72.52 | 21.04 | 0.94 |
| 8 | 449 | 1303 | 87.84 | 25.11 | 1.10 |
| 10 | 561 | 1529 | 98.13 | 28.44 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1805 | 23.92 | 7.60 | 0.48 |
| 2| 1936 | 25.43 | 8.68 | 0.50 |
| 3| 2013 | 25.91 | 9.48 | 0.52 |
| 5| 2329 | 30.00 | 11.96 | 0.58 |
| 10| 3223 | 41.93 | 18.62 | 0.77 |
| 38| 7286 | 95.55 | 52.18 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 622 | 22.57 | 7.32 | 0.41 |
| 2| 809 | 24.94 | 8.63 | 0.45 |
| 3| 857 | 24.03 | 9.02 | 0.45 |
| 5| 1201 | 29.22 | 11.81 | 0.52 |
| 10| 1955 | 38.03 | 17.62 | 0.67 |
| 41| 6819 | 99.37 | 55.33 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 29.17 | 8.91 | 0.48 |
| 2| 791 | 30.98 | 10.08 | 0.51 |
| 3| 956 | 33.51 | 11.46 | 0.55 |
| 5| 1194 | 36.27 | 13.55 | 0.59 |
| 10| 1981 | 47.26 | 20.00 | 0.76 |
| 36| 6119 | 99.72 | 52.13 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 907 | 36.56 | 11.60 | 0.57 |
| 3| 1004 | 38.59 | 12.82 | 0.60 |
| 5| 1230 | 41.82 | 15.03 | 0.65 |
| 10| 2226 | 56.89 | 22.67 | 0.87 |
| 29| 5069 | 99.67 | 47.22 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5816 | 27.08 | 9.08 | 0.69 |
| 2| 6025 | 36.93 | 12.46 | 0.80 |
| 3| 6176 | 45.76 | 15.43 | 0.90 |
| 4| 6353 | 56.72 | 19.22 | 1.03 |
| 5| 6513 | 65.03 | 21.94 | 1.12 |
| 6| 6523 | 70.45 | 23.76 | 1.18 |
| 7| 6561 | 77.52 | 26.05 | 1.25 |
| 8| 6999 | 93.49 | 31.55 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 284 | 6003 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 569 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1140 | 6515 | 59.28 | 22.29 | 1.08 |
| 10 | 30 | 1709 | 6855 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2218 | 7157 | 99.38 | 38.04 | 1.54 |

