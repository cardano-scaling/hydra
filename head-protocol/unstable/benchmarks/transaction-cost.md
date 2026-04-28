--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-28 15:39:00.653795608 UTC |
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
| 1| 5465 | 10.43 | 3.31 | 0.50 |
| 2| 5561 | 10.45 | 3.28 | 0.50 |
| 3| 5656 | 11.34 | 3.56 | 0.52 |
| 5| 5847 | 12.70 | 3.97 | 0.54 |
| 10| 6329 | 15.82 | 4.89 | 0.59 |
| 50| 10169 | 42.59 | 12.77 | 1.03 |
| 100| 14973 | 75.75 | 22.53 | 1.58 |
| 114| 16316 | 85.81 | 25.50 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.67 | 6.48 | 0.43 |
| 2| 1917 | 19.76 | 7.47 | 0.45 |
| 3| 2049 | 20.64 | 8.41 | 0.47 |
| 5| 2313 | 22.97 | 10.43 | 0.51 |
| 10| 2967 | 28.28 | 15.36 | 0.62 |
| 50| 8209 | 75.29 | 55.89 | 1.52 |
| 71| 10964 | 99.21 | 76.96 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.95 | 6.02 | 0.36 |
| 2| 725 | 18.06 | 7.02 | 0.38 |
| 3| 853 | 19.17 | 8.02 | 0.40 |
| 5| 1118 | 21.32 | 10.00 | 0.44 |
| 10| 1770 | 26.92 | 15.00 | 0.55 |
| 50| 7011 | 72.40 | 55.12 | 1.44 |
| 74| 10154 | 99.90 | 79.25 | 1.98 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 597 | 21.82 | 7.21 | 0.40 |
| 2| 732 | 23.39 | 8.33 | 0.43 |
| 3| 860 | 24.94 | 9.45 | 0.46 |
| 5| 1161 | 26.97 | 11.37 | 0.50 |
| 10| 1777 | 37.84 | 17.79 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.86 | 9.31 | 0.47 |
| 3| 892 | 28.68 | 10.50 | 0.50 |
| 5| 1154 | 32.42 | 12.90 | 0.56 |
| 10| 1808 | 42.86 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 30 | 1707 | 6547 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2275 | 6885 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2617 | 7089 | 98.80 | 40.03 | 1.55 |

