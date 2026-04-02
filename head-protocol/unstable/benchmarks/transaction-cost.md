--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 15:25:54.263782156 UTC |
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
| 1| 5464 | 9.77 | 3.08 | 0.49 |
| 2| 5560 | 10.22 | 3.20 | 0.50 |
| 3| 5658 | 11.27 | 3.54 | 0.52 |
| 5| 5847 | 12.54 | 3.92 | 0.54 |
| 10| 6330 | 15.35 | 4.73 | 0.59 |
| 50| 10171 | 42.77 | 12.84 | 1.04 |
| 100| 14969 | 76.13 | 22.64 | 1.59 |
| 114| 16316 | 85.54 | 25.40 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 18.67 | 6.48 | 0.43 |
| 2| 1918 | 19.81 | 7.48 | 0.45 |
| 3| 2053 | 20.90 | 8.47 | 0.47 |
| 5| 2312 | 23.07 | 10.46 | 0.51 |
| 10| 2972 | 28.42 | 15.40 | 0.62 |
| 50| 8211 | 75.62 | 55.97 | 1.53 |
| 72| 11095 | 99.79 | 77.82 | 2.01 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 589 | 16.95 | 6.02 | 0.36 |
| 2| 720 | 18.03 | 7.01 | 0.38 |
| 3| 852 | 19.20 | 8.03 | 0.40 |
| 5| 1119 | 21.30 | 9.99 | 0.44 |
| 10| 1769 | 26.77 | 14.96 | 0.55 |
| 50| 7012 | 72.66 | 55.19 | 1.44 |
| 73| 10024 | 98.71 | 78.24 | 1.95 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 21.82 | 7.21 | 0.41 |
| 2| 767 | 22.14 | 7.98 | 0.42 |
| 3| 869 | 25.02 | 9.47 | 0.46 |
| 5| 1156 | 26.86 | 11.34 | 0.50 |
| 10| 1781 | 37.86 | 17.79 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 760 | 26.91 | 9.32 | 0.47 |
| 3| 896 | 28.68 | 10.50 | 0.50 |
| 5| 1153 | 32.44 | 12.90 | 0.56 |
| 10| 1808 | 42.84 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1139 | 6207 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1706 | 6546 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2272 | 6883 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2613 | 7085 | 98.80 | 40.03 | 1.55 |

