--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-22 14:56:09.807801174 UTC |
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
| 1| 5837 | 10.72 | 3.41 | 0.52 |
| 2| 6037 | 13.10 | 4.17 | 0.55 |
| 3| 6238 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7650 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.64 | 30.82 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10054 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 751 | 43.79 | 12.52 | 0.63 |
| 4 | 226 | 858 | 52.12 | 14.90 | 0.72 |
| 5 | 283 | 974 | 57.50 | 16.61 | 0.78 |
| 6 | 336 | 1081 | 69.85 | 20.00 | 0.91 |
| 7 | 395 | 1196 | 74.45 | 21.46 | 0.96 |
| 8 | 451 | 1303 | 83.22 | 24.00 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1746 | 22.92 | 7.32 | 0.47 |
| 2| 1953 | 25.84 | 8.78 | 0.51 |
| 3| 2116 | 28.02 | 10.07 | 0.54 |
| 5| 2275 | 29.00 | 11.68 | 0.57 |
| 10| 3041 | 38.51 | 17.67 | 0.72 |
| 38| 7441 | 98.88 | 53.12 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 22.77 | 7.36 | 0.42 |
| 2| 699 | 22.58 | 7.96 | 0.42 |
| 3| 914 | 25.83 | 9.55 | 0.47 |
| 5| 1313 | 32.07 | 12.62 | 0.56 |
| 10| 2042 | 40.34 | 18.28 | 0.70 |
| 41| 6524 | 96.54 | 54.52 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 27.51 | 8.47 | 0.46 |
| 2| 783 | 30.98 | 10.08 | 0.51 |
| 3| 941 | 32.75 | 11.24 | 0.54 |
| 5| 1230 | 34.22 | 13.00 | 0.57 |
| 10| 2079 | 45.50 | 19.56 | 0.75 |
| 35| 5914 | 96.44 | 50.52 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 663 | 33.83 | 10.16 | 0.53 |
| 2| 764 | 35.17 | 11.17 | 0.55 |
| 3| 896 | 37.20 | 12.40 | 0.58 |
| 5| 1249 | 42.56 | 15.26 | 0.66 |
| 10| 2041 | 54.09 | 21.81 | 0.83 |
| 29| 4746 | 96.53 | 46.23 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5788 | 27.13 | 9.10 | 0.69 |
| 2| 5976 | 37.05 | 12.47 | 0.80 |
| 3| 6019 | 41.44 | 13.84 | 0.85 |
| 4| 6190 | 53.86 | 18.08 | 0.99 |
| 5| 6207 | 52.43 | 17.46 | 0.97 |
| 6| 6674 | 74.38 | 25.08 | 1.23 |
| 7| 6778 | 84.62 | 28.49 | 1.34 |
| 8| 6799 | 89.84 | 30.31 | 1.39 |
| 9| 6985 | 97.88 | 33.00 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 20.52 | 6.86 | 0.62 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 10 | 569 | 6173 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1139 | 6513 | 59.28 | 22.29 | 1.08 |
| 10 | 30 | 1709 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2221 | 7160 | 98.86 | 37.86 | 1.54 |

