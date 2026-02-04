--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-04 13:20:54.900895568 UTC |
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
| 1| 5837 | 10.38 | 3.29 | 0.51 |
| 2| 6037 | 12.44 | 3.94 | 0.54 |
| 3| 6238 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14279 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1275 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10071 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 41.31 | 11.95 | 0.60 |
| 4 | 228 | 862 | 52.44 | 15.00 | 0.72 |
| 5 | 283 | 974 | 64.17 | 18.20 | 0.84 |
| 6 | 337 | 1081 | 65.01 | 18.88 | 0.86 |
| 7 | 396 | 1192 | 86.78 | 24.41 | 1.08 |
| 8 | 450 | 1307 | 98.47 | 27.65 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1811 | 24.37 | 7.71 | 0.48 |
| 2| 1955 | 25.76 | 8.76 | 0.51 |
| 3| 2067 | 27.27 | 9.85 | 0.53 |
| 5| 2377 | 30.93 | 12.22 | 0.59 |
| 10| 3158 | 41.71 | 18.56 | 0.76 |
| 39| 7257 | 92.12 | 51.90 | 1.59 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.77 | 7.36 | 0.42 |
| 2| 697 | 22.62 | 7.95 | 0.42 |
| 3| 881 | 25.82 | 9.55 | 0.47 |
| 5| 1159 | 28.01 | 11.47 | 0.51 |
| 10| 2046 | 41.06 | 18.47 | 0.71 |
| 40| 6434 | 93.39 | 53.03 | 1.57 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 697 | 27.47 | 8.46 | 0.46 |
| 2| 841 | 29.22 | 9.61 | 0.49 |
| 3| 975 | 33.36 | 11.43 | 0.55 |
| 5| 1131 | 35.63 | 13.35 | 0.58 |
| 10| 1961 | 46.51 | 19.77 | 0.75 |
| 37| 6099 | 99.90 | 52.79 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 33.12 | 9.94 | 0.52 |
| 2| 807 | 35.88 | 11.39 | 0.56 |
| 3| 937 | 37.87 | 12.61 | 0.59 |
| 5| 1283 | 43.28 | 15.47 | 0.67 |
| 10| 1965 | 53.87 | 21.74 | 0.83 |
| 29| 4829 | 97.97 | 46.70 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5803 | 26.97 | 9.06 | 0.69 |
| 2| 6037 | 36.92 | 12.47 | 0.80 |
| 3| 6142 | 45.60 | 15.40 | 0.90 |
| 4| 6188 | 52.96 | 17.73 | 0.98 |
| 5| 6333 | 60.37 | 20.26 | 1.06 |
| 6| 6631 | 74.31 | 25.14 | 1.22 |
| 7| 6718 | 82.38 | 27.68 | 1.31 |
| 8| 6735 | 87.00 | 29.17 | 1.36 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 17.86 | 5.96 | 0.59 |
| 10 | 5 | 285 | 6004 | 29.79 | 10.58 | 0.73 |
| 10 | 20 | 1139 | 6513 | 58.66 | 22.07 | 1.07 |
| 10 | 30 | 1707 | 6853 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2220 | 7159 | 98.93 | 37.88 | 1.54 |

