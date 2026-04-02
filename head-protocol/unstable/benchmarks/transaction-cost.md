--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 11:25:38.213929864 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | 21fa6ee40ea957042a3dee8bae69b15c7ed88102be7ecb056d8911b2 | 11452 | 
| μHead | aac60f1ab5b37ad38b49163a7a13bcbbdcb8ab12d839720cd7d8748e* | 4978 | 
| νDeposit | ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c | 1102 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5464 | 9.58 | 3.01 | 0.49 |
| 2| 5560 | 10.41 | 3.27 | 0.50 |
| 3| 5657 | 11.26 | 3.54 | 0.52 |
| 5| 5849 | 12.21 | 3.79 | 0.53 |
| 10| 6329 | 15.58 | 4.81 | 0.59 |
| 50| 10172 | 42.22 | 12.65 | 1.03 |
| 100| 14972 | 76.53 | 22.80 | 1.59 |
| 114| 16315 | 86.03 | 25.59 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 18.47 | 6.43 | 0.42 |
| 2| 1919 | 19.55 | 7.42 | 0.45 |
| 3| 2054 | 20.87 | 8.47 | 0.47 |
| 5| 2311 | 23.15 | 10.48 | 0.51 |
| 10| 2969 | 28.41 | 15.40 | 0.62 |
| 50| 8207 | 75.00 | 55.81 | 1.52 |
| 71| 10958 | 98.64 | 76.81 | 1.98 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.95 | 6.02 | 0.36 |
| 2| 720 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.17 | 8.02 | 0.40 |
| 5| 1114 | 21.32 | 10.00 | 0.44 |
| 10| 1771 | 26.77 | 14.96 | 0.55 |
| 50| 7011 | 72.32 | 55.10 | 1.44 |
| 72| 9893 | 98.37 | 77.44 | 1.94 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 20.65 | 6.88 | 0.39 |
| 2| 766 | 22.14 | 7.98 | 0.42 |
| 3| 864 | 24.97 | 9.45 | 0.46 |
| 5| 1156 | 26.97 | 11.37 | 0.50 |
| 10| 1812 | 36.02 | 17.28 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.89 | 9.31 | 0.47 |
| 3| 892 | 28.66 | 10.49 | 0.50 |
| 5| 1153 | 32.36 | 12.88 | 0.56 |
| 10| 1808 | 42.86 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5699 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5869 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1139 | 6208 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1706 | 6546 | 69.61 | 27.91 | 1.20 |
| 10 | 43 | 2448 | 6989 | 93.33 | 37.76 | 1.48 |

