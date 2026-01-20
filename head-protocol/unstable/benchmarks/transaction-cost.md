--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-20 11:56:13.050652086 UTC |
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
| 2| 6041 | 12.44 | 3.94 | 0.54 |
| 3| 6242 | 14.72 | 4.66 | 0.58 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7646 | 29.14 | 9.19 | 0.79 |
| 43| 14282 | 98.94 | 30.92 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10050 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 33.25 | 9.63 | 0.52 |
| 3 | 170 | 747 | 44.03 | 12.62 | 0.63 |
| 4 | 226 | 858 | 52.57 | 15.06 | 0.72 |
| 5 | 283 | 969 | 61.31 | 17.52 | 0.81 |
| 6 | 339 | 1081 | 73.27 | 20.74 | 0.94 |
| 7 | 393 | 1192 | 72.51 | 21.03 | 0.94 |
| 8 | 449 | 1303 | 89.97 | 25.62 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 23.30 | 7.41 | 0.47 |
| 2| 1966 | 26.92 | 9.08 | 0.52 |
| 3| 2059 | 26.94 | 9.77 | 0.53 |
| 5| 2455 | 32.41 | 12.62 | 0.61 |
| 10| 3210 | 41.30 | 18.46 | 0.76 |
| 41| 7778 | 98.49 | 55.02 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 606 | 22.53 | 7.29 | 0.41 |
| 2| 795 | 25.20 | 8.72 | 0.45 |
| 3| 961 | 26.09 | 9.60 | 0.47 |
| 5| 1161 | 28.00 | 11.48 | 0.51 |
| 10| 1953 | 38.53 | 17.75 | 0.68 |
| 44| 6816 | 98.10 | 56.98 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 704 | 27.51 | 8.47 | 0.46 |
| 2| 807 | 30.95 | 10.07 | 0.51 |
| 3| 927 | 32.79 | 11.25 | 0.54 |
| 5| 1297 | 37.73 | 13.99 | 0.61 |
| 10| 2014 | 47.32 | 20.01 | 0.77 |
| 36| 5859 | 96.11 | 51.06 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 33.83 | 10.16 | 0.53 |
| 2| 878 | 36.60 | 11.61 | 0.57 |
| 3| 957 | 37.91 | 12.62 | 0.59 |
| 5| 1305 | 43.43 | 15.51 | 0.67 |
| 10| 2064 | 54.54 | 21.97 | 0.84 |
| 29| 4936 | 98.26 | 46.75 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5789 | 27.00 | 9.06 | 0.69 |
| 2| 5865 | 32.56 | 10.87 | 0.75 |
| 3| 6066 | 45.00 | 15.13 | 0.89 |
| 4| 6245 | 54.98 | 18.52 | 1.00 |
| 5| 6375 | 64.28 | 21.59 | 1.11 |
| 6| 6551 | 72.54 | 24.41 | 1.20 |
| 7| 6815 | 84.15 | 28.45 | 1.34 |
| 8| 6776 | 87.99 | 29.59 | 1.37 |
| 9| 6874 | 95.08 | 32.01 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 20.34 | 6.91 | 0.62 |
| 10 | 10 | 569 | 6173 | 39.44 | 14.43 | 0.84 |
| 10 | 30 | 1709 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 40 | 2276 | 7193 | 99.66 | 38.24 | 1.55 |
| 10 | 38 | 2164 | 7126 | 97.77 | 37.38 | 1.52 |

