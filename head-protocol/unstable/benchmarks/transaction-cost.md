--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-21 09:35:20.34149677 UTC |
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
| 1| 5468 | 10.15 | 3.21 | 0.50 |
| 2| 5558 | 10.41 | 3.27 | 0.50 |
| 3| 5660 | 11.06 | 3.46 | 0.51 |
| 5| 5850 | 12.12 | 3.77 | 0.53 |
| 10| 6329 | 15.69 | 4.85 | 0.59 |
| 50| 10171 | 42.30 | 12.67 | 1.03 |
| 100| 14972 | 76.16 | 22.65 | 1.59 |
| 114| 16313 | 86.06 | 25.60 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.67 | 6.48 | 0.43 |
| 2| 1915 | 19.79 | 7.48 | 0.45 |
| 3| 2052 | 20.72 | 8.43 | 0.47 |
| 5| 2313 | 23.10 | 10.47 | 0.51 |
| 10| 2968 | 28.56 | 15.43 | 0.62 |
| 50| 8211 | 74.98 | 55.80 | 1.52 |
| 71| 10959 | 99.17 | 76.95 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 591 | 16.95 | 6.02 | 0.36 |
| 2| 719 | 18.03 | 7.01 | 0.38 |
| 3| 853 | 19.15 | 8.01 | 0.40 |
| 5| 1115 | 21.32 | 10.00 | 0.44 |
| 10| 1773 | 26.79 | 14.97 | 0.55 |
| 50| 7011 | 72.77 | 55.22 | 1.44 |
| 73| 10023 | 98.61 | 78.21 | 1.95 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 605 | 21.82 | 7.21 | 0.41 |
| 2| 728 | 23.37 | 8.32 | 0.43 |
| 3| 869 | 24.94 | 9.45 | 0.46 |
| 5| 1160 | 26.84 | 11.33 | 0.50 |
| 10| 1815 | 35.89 | 17.24 | 0.64 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 25.16 | 8.15 | 0.44 |
| 2| 757 | 26.91 | 9.32 | 0.47 |
| 3| 896 | 28.68 | 10.50 | 0.50 |
| 5| 1154 | 32.39 | 12.89 | 0.56 |
| 10| 1805 | 42.63 | 19.13 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 568 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1136 | 6204 | 51.38 | 20.34 | 0.98 |
| 10 | 40 | 2277 | 6887 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2619 | 7092 | 98.80 | 40.03 | 1.55 |

