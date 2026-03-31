--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-31 14:22:23.245795554 UTC |
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
| 1| 5465 | 10.28 | 3.26 | 0.50 |
| 2| 5558 | 10.42 | 3.27 | 0.50 |
| 3| 5656 | 11.05 | 3.46 | 0.51 |
| 5| 5851 | 12.12 | 3.77 | 0.53 |
| 10| 6327 | 15.59 | 4.82 | 0.59 |
| 50| 10172 | 42.23 | 12.65 | 1.03 |
| 100| 14972 | 76.53 | 22.78 | 1.59 |
| 114| 16316 | 85.84 | 25.50 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 18.47 | 6.43 | 0.42 |
| 2| 1924 | 19.76 | 7.47 | 0.45 |
| 3| 2050 | 20.64 | 8.41 | 0.47 |
| 5| 2313 | 23.05 | 10.45 | 0.51 |
| 10| 2967 | 28.59 | 15.44 | 0.62 |
| 50| 8210 | 74.42 | 55.66 | 1.51 |
| 72| 11090 | 99.57 | 77.76 | 2.00 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 726 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.12 | 8.01 | 0.40 |
| 5| 1116 | 21.43 | 10.02 | 0.44 |
| 10| 1770 | 26.97 | 15.01 | 0.55 |
| 50| 7012 | 73.22 | 55.34 | 1.45 |
| 74| 10156 | 99.67 | 79.19 | 1.97 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 20.62 | 6.87 | 0.39 |
| 2| 728 | 23.37 | 8.32 | 0.43 |
| 3| 860 | 24.94 | 9.45 | 0.46 |
| 5| 1165 | 26.89 | 11.34 | 0.50 |
| 10| 1781 | 37.86 | 17.79 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 25.14 | 8.14 | 0.44 |
| 2| 761 | 26.89 | 9.31 | 0.47 |
| 3| 892 | 28.68 | 10.50 | 0.50 |
| 5| 1158 | 32.44 | 12.90 | 0.56 |
| 10| 1803 | 42.84 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 284 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 30 | 1707 | 6547 | 69.61 | 27.91 | 1.20 |
| 10 | 46 | 2618 | 7091 | 98.80 | 40.03 | 1.55 |

