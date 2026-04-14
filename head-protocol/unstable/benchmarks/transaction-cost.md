--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-14 15:07:53.920866135 UTC |
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
| 1| 5465 | 9.67 | 3.04 | 0.49 |
| 2| 5560 | 10.22 | 3.20 | 0.50 |
| 3| 5656 | 10.86 | 3.39 | 0.51 |
| 5| 5850 | 12.25 | 3.81 | 0.53 |
| 10| 6327 | 15.44 | 4.76 | 0.59 |
| 50| 10176 | 42.73 | 12.81 | 1.04 |
| 100| 14972 | 76.61 | 22.83 | 1.59 |
| 114| 16315 | 85.51 | 25.40 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.47 | 6.43 | 0.42 |
| 2| 1924 | 19.76 | 7.47 | 0.45 |
| 3| 2049 | 20.85 | 8.46 | 0.47 |
| 5| 2312 | 22.95 | 10.43 | 0.51 |
| 10| 2972 | 28.77 | 15.49 | 0.63 |
| 50| 8210 | 74.44 | 55.66 | 1.51 |
| 71| 10961 | 98.96 | 76.90 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 589 | 16.97 | 6.03 | 0.36 |
| 2| 722 | 18.06 | 7.02 | 0.38 |
| 3| 856 | 19.12 | 8.01 | 0.40 |
| 5| 1115 | 21.40 | 10.02 | 0.44 |
| 10| 1769 | 26.95 | 15.01 | 0.55 |
| 50| 7016 | 72.97 | 55.27 | 1.45 |
| 72| 9892 | 99.12 | 77.64 | 1.95 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 21.82 | 7.21 | 0.41 |
| 2| 766 | 22.09 | 7.96 | 0.42 |
| 3| 864 | 24.94 | 9.45 | 0.46 |
| 5| 1161 | 26.86 | 11.34 | 0.50 |
| 10| 1777 | 37.86 | 17.79 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.91 | 9.32 | 0.47 |
| 3| 888 | 28.68 | 10.50 | 0.50 |
| 5| 1154 | 32.36 | 12.88 | 0.56 |
| 10| 1809 | 42.84 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 56 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 10 | 570 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 46 | 2617 | 7089 | 98.80 | 40.03 | 1.55 |

