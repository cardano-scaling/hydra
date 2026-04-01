--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-01 13:29:11.333123583 UTC |
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
| 1| 5465 | 9.57 | 3.01 | 0.49 |
| 2| 5561 | 10.64 | 3.35 | 0.51 |
| 3| 5654 | 10.86 | 3.39 | 0.51 |
| 5| 5849 | 12.12 | 3.77 | 0.53 |
| 10| 6330 | 15.61 | 4.82 | 0.59 |
| 50| 10175 | 42.25 | 12.66 | 1.03 |
| 100| 14975 | 76.18 | 22.66 | 1.59 |
| 114| 16315 | 85.51 | 25.40 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1786 | 18.47 | 6.43 | 0.42 |
| 2| 1920 | 19.56 | 7.42 | 0.45 |
| 3| 2050 | 20.69 | 8.42 | 0.47 |
| 5| 2313 | 22.90 | 10.42 | 0.51 |
| 10| 2968 | 28.70 | 15.47 | 0.62 |
| 50| 8210 | 74.56 | 55.70 | 1.52 |
| 71| 10962 | 99.87 | 77.14 | 2.00 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 593 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.03 | 7.01 | 0.38 |
| 3| 851 | 19.17 | 8.02 | 0.40 |
| 5| 1114 | 21.35 | 10.00 | 0.44 |
| 10| 1770 | 26.97 | 15.01 | 0.55 |
| 50| 7012 | 72.32 | 55.10 | 1.44 |
| 72| 9893 | 97.70 | 77.27 | 1.93 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 636 | 20.65 | 6.88 | 0.39 |
| 2| 732 | 23.34 | 8.32 | 0.43 |
| 3| 860 | 25.02 | 9.47 | 0.46 |
| 5| 1161 | 26.89 | 11.34 | 0.50 |
| 10| 1781 | 37.84 | 17.79 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.91 | 9.32 | 0.47 |
| 3| 888 | 28.68 | 10.50 | 0.50 |
| 5| 1154 | 32.47 | 12.91 | 0.56 |
| 10| 1809 | 42.61 | 19.12 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1139 | 6207 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1707 | 6547 | 69.61 | 27.91 | 1.20 |
| 10 | 46 | 2616 | 7089 | 98.80 | 40.03 | 1.55 |

