--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-23 12:08:33.888666492 UTC |
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
| 1| 5464 | 9.59 | 3.01 | 0.49 |
| 2| 5560 | 10.45 | 3.28 | 0.50 |
| 3| 5657 | 10.85 | 3.39 | 0.51 |
| 5| 5849 | 12.20 | 3.80 | 0.53 |
| 10| 6327 | 15.89 | 4.93 | 0.59 |
| 50| 10171 | 42.56 | 12.76 | 1.03 |
| 100| 14973 | 76.12 | 22.63 | 1.59 |
| 114| 16319 | 85.86 | 25.54 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.67 | 6.48 | 0.43 |
| 2| 1924 | 19.79 | 7.48 | 0.45 |
| 3| 2047 | 20.70 | 8.42 | 0.47 |
| 5| 2312 | 23.13 | 10.47 | 0.51 |
| 10| 2966 | 28.62 | 15.45 | 0.62 |
| 50| 8210 | 75.12 | 55.84 | 1.52 |
| 71| 10960 | 98.99 | 76.91 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.03 | 7.01 | 0.38 |
| 3| 851 | 19.15 | 8.01 | 0.40 |
| 5| 1120 | 21.43 | 10.02 | 0.44 |
| 10| 1771 | 26.84 | 14.98 | 0.55 |
| 50| 7012 | 73.41 | 55.39 | 1.45 |
| 73| 10023 | 98.53 | 78.19 | 1.95 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 21.80 | 7.20 | 0.40 |
| 2| 732 | 23.37 | 8.32 | 0.43 |
| 3| 894 | 23.69 | 9.09 | 0.45 |
| 5| 1156 | 26.97 | 11.37 | 0.50 |
| 10| 1811 | 36.10 | 17.30 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.14 | 8.14 | 0.44 |
| 2| 761 | 26.89 | 9.31 | 0.47 |
| 3| 892 | 28.66 | 10.49 | 0.50 |
| 5| 1154 | 32.36 | 12.88 | 0.56 |
| 10| 1809 | 42.81 | 19.18 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 15.12 | 5.24 | 0.55 |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 30 | 1704 | 6545 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2276 | 6887 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2616 | 7088 | 98.80 | 40.03 | 1.55 |

