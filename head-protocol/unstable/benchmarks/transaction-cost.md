--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-10 15:38:17.743135446 UTC |
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
| 1| 5836 | 10.86 | 3.46 | 0.52 |
| 2| 6037 | 12.44 | 3.94 | 0.54 |
| 3| 6238 | 14.52 | 4.59 | 0.58 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14285 | 98.95 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 916 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2183 | 12.13 | 7.25 | 0.40 |
| 54| 10074 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 34.20 | 9.84 | 0.53 |
| 3 | 171 | 751 | 41.11 | 11.88 | 0.60 |
| 4 | 225 | 862 | 48.23 | 13.99 | 0.68 |
| 5 | 283 | 969 | 60.93 | 17.43 | 0.81 |
| 6 | 339 | 1081 | 73.26 | 20.77 | 0.94 |
| 7 | 395 | 1192 | 82.47 | 23.37 | 1.04 |
| 8 | 453 | 1303 | 94.45 | 26.74 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1807 | 24.00 | 7.62 | 0.48 |
| 2| 1882 | 24.85 | 8.50 | 0.50 |
| 3| 2055 | 26.87 | 9.75 | 0.53 |
| 5| 2385 | 31.11 | 12.27 | 0.60 |
| 10| 3241 | 42.78 | 18.86 | 0.77 |
| 41| 7477 | 94.88 | 54.03 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 664 | 22.54 | 7.31 | 0.41 |
| 2| 739 | 23.61 | 8.23 | 0.43 |
| 3| 922 | 26.94 | 9.85 | 0.48 |
| 5| 1135 | 28.07 | 11.50 | 0.51 |
| 10| 1898 | 36.75 | 17.25 | 0.66 |
| 42| 6616 | 97.59 | 55.49 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 652 | 29.13 | 8.90 | 0.48 |
| 2| 821 | 29.26 | 9.62 | 0.49 |
| 3| 915 | 32.72 | 11.23 | 0.54 |
| 5| 1308 | 37.73 | 13.99 | 0.61 |
| 10| 2219 | 50.57 | 20.99 | 0.81 |
| 36| 5824 | 96.80 | 51.23 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 698 | 33.87 | 10.16 | 0.53 |
| 2| 817 | 35.85 | 11.38 | 0.56 |
| 3| 939 | 37.84 | 12.60 | 0.59 |
| 5| 1263 | 42.53 | 15.25 | 0.66 |
| 10| 2028 | 54.02 | 21.80 | 0.83 |
| 29| 4781 | 96.29 | 46.20 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5816 | 26.97 | 9.06 | 0.69 |
| 2| 5916 | 34.76 | 11.63 | 0.78 |
| 3| 6064 | 44.52 | 14.98 | 0.88 |
| 4| 6252 | 55.21 | 18.57 | 1.01 |
| 5| 6530 | 66.20 | 22.39 | 1.13 |
| 6| 6483 | 69.40 | 23.31 | 1.16 |
| 7| 6786 | 80.87 | 27.24 | 1.30 |
| 8| 6848 | 90.33 | 30.44 | 1.40 |
| 9| 7109 | 99.75 | 33.55 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 571 | 6176 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1139 | 6514 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2222 | 7161 | 98.93 | 37.88 | 1.54 |

