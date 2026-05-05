--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-05-05 18:08:49.423749668 UTC |
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
| 1| 5464 | 10.07 | 3.18 | 0.50 |
| 2| 5565 | 10.41 | 3.27 | 0.50 |
| 3| 5656 | 11.08 | 3.47 | 0.51 |
| 5| 5847 | 12.73 | 3.98 | 0.54 |
| 10| 6330 | 15.74 | 4.86 | 0.59 |
| 50| 10171 | 42.38 | 12.71 | 1.03 |
| 100| 14971 | 75.97 | 22.59 | 1.59 |
| 114| 16315 | 86.34 | 25.69 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.67 | 6.48 | 0.43 |
| 2| 1918 | 19.81 | 7.48 | 0.45 |
| 3| 2051 | 20.69 | 8.42 | 0.47 |
| 5| 2313 | 23.03 | 10.45 | 0.51 |
| 10| 2969 | 28.36 | 15.38 | 0.62 |
| 50| 8213 | 74.71 | 55.73 | 1.52 |
| 71| 10960 | 99.79 | 77.12 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 593 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.06 | 7.02 | 0.38 |
| 3| 853 | 19.20 | 8.03 | 0.40 |
| 5| 1115 | 21.40 | 10.02 | 0.44 |
| 10| 1771 | 26.87 | 14.99 | 0.55 |
| 50| 7012 | 72.66 | 55.19 | 1.44 |
| 72| 9893 | 98.12 | 77.38 | 1.94 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 21.80 | 7.20 | 0.40 |
| 2| 736 | 23.39 | 8.33 | 0.43 |
| 3| 864 | 25.02 | 9.47 | 0.46 |
| 5| 1160 | 26.94 | 11.36 | 0.50 |
| 10| 1778 | 37.91 | 17.81 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 25.14 | 8.14 | 0.44 |
| 2| 765 | 26.89 | 9.31 | 0.47 |
| 3| 888 | 28.71 | 10.50 | 0.50 |
| 5| 1153 | 32.36 | 12.88 | 0.56 |
| 10| 1805 | 42.68 | 19.14 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 10 | 570 | 5869 | 33.17 | 12.77 | 0.77 |
| 10 | 40 | 2270 | 6881 | 87.85 | 35.49 | 1.42 |
| 10 | 44 | 2508 | 7026 | 95.15 | 38.52 | 1.51 |

