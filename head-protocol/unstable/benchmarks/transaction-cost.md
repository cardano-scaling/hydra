--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-17 09:19:45.668512594 UTC |
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
| 2| 6035 | 12.53 | 3.97 | 0.55 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6638 | 18.84 | 5.95 | 0.64 |
| 10| 7647 | 29.14 | 9.19 | 0.79 |
| 43| 14282 | 98.66 | 30.82 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10049 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 640 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 751 | 41.49 | 11.99 | 0.61 |
| 4 | 227 | 858 | 48.25 | 14.00 | 0.68 |
| 5 | 283 | 974 | 64.39 | 18.26 | 0.85 |
| 6 | 340 | 1081 | 68.23 | 19.57 | 0.89 |
| 7 | 395 | 1192 | 74.20 | 21.35 | 0.96 |
| 8 | 448 | 1303 | 95.92 | 26.94 | 1.18 |
| 9 | 504 | 1414 | 96.62 | 27.67 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.37 | 7.71 | 0.48 |
| 2| 1951 | 25.84 | 8.78 | 0.51 |
| 3| 2015 | 25.95 | 9.49 | 0.52 |
| 5| 2445 | 32.15 | 12.56 | 0.61 |
| 10| 3175 | 40.78 | 18.30 | 0.75 |
| 41| 7697 | 99.62 | 55.34 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 600 | 22.84 | 7.39 | 0.41 |
| 2| 722 | 22.60 | 7.95 | 0.42 |
| 3| 984 | 26.97 | 9.87 | 0.48 |
| 5| 1332 | 32.56 | 12.75 | 0.56 |
| 10| 2051 | 42.75 | 18.92 | 0.72 |
| 42| 6755 | 97.56 | 55.52 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 27.54 | 8.47 | 0.46 |
| 2| 771 | 28.55 | 9.40 | 0.48 |
| 3| 968 | 33.36 | 11.42 | 0.54 |
| 5| 1326 | 37.73 | 13.99 | 0.61 |
| 10| 2229 | 46.77 | 19.95 | 0.77 |
| 35| 6073 | 99.43 | 51.40 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 33.83 | 10.15 | 0.53 |
| 2| 795 | 35.81 | 11.37 | 0.56 |
| 3| 950 | 37.87 | 12.61 | 0.59 |
| 5| 1407 | 44.02 | 15.70 | 0.68 |
| 10| 2147 | 55.55 | 22.28 | 0.85 |
| 28| 4674 | 96.05 | 45.48 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5805 | 27.13 | 9.10 | 0.69 |
| 2| 5917 | 34.83 | 11.66 | 0.78 |
| 3| 6071 | 44.61 | 15.01 | 0.89 |
| 4| 6352 | 55.24 | 18.73 | 1.01 |
| 5| 6415 | 60.08 | 20.21 | 1.06 |
| 6| 6712 | 74.60 | 25.22 | 1.23 |
| 7| 6522 | 75.08 | 25.10 | 1.22 |
| 8| 6887 | 86.94 | 29.26 | 1.37 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 22.73 | 7.73 | 0.65 |
| 10 | 20 | 1137 | 6511 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1706 | 6853 | 79.78 | 30.37 | 1.32 |
| 10 | 39 | 2219 | 7159 | 98.05 | 37.58 | 1.53 |

