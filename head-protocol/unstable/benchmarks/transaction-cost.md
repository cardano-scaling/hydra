--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-01 08:38:53.021748647 UTC |
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
| 1| 5462 | 9.76 | 3.07 | 0.49 |
| 2| 5560 | 10.62 | 3.34 | 0.50 |
| 3| 5656 | 11.08 | 3.47 | 0.51 |
| 5| 5854 | 12.36 | 3.85 | 0.54 |
| 10| 6329 | 15.40 | 4.75 | 0.59 |
| 50| 10172 | 42.87 | 12.88 | 1.04 |
| 100| 14969 | 76.61 | 22.82 | 1.59 |
| 114| 16316 | 85.87 | 25.53 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1786 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.58 | 7.43 | 0.45 |
| 3| 2054 | 20.95 | 8.49 | 0.47 |
| 5| 2313 | 23.10 | 10.47 | 0.51 |
| 10| 2968 | 28.64 | 15.45 | 0.62 |
| 50| 8213 | 75.40 | 55.92 | 1.52 |
| 71| 10959 | 99.19 | 76.96 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.95 | 6.02 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.12 | 8.01 | 0.40 |
| 5| 1114 | 21.43 | 10.02 | 0.44 |
| 10| 1774 | 26.84 | 14.98 | 0.55 |
| 50| 7010 | 72.77 | 55.22 | 1.44 |
| 74| 10156 | 99.85 | 79.24 | 1.98 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 20.65 | 6.88 | 0.39 |
| 2| 766 | 22.14 | 7.98 | 0.42 |
| 3| 899 | 23.67 | 9.08 | 0.45 |
| 5| 1164 | 26.94 | 11.36 | 0.50 |
| 10| 1815 | 36.07 | 17.29 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 760 | 26.86 | 9.31 | 0.47 |
| 3| 891 | 28.63 | 10.48 | 0.50 |
| 5| 1158 | 32.44 | 12.90 | 0.56 |
| 10| 1804 | 42.84 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 30 | 1709 | 6549 | 69.61 | 27.91 | 1.20 |
| 10 | 46 | 2616 | 7089 | 98.80 | 40.03 | 1.55 |

