--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 14:13:04.625646518 UTC |
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
| 1| 5462 | 9.78 | 3.08 | 0.49 |
| 2| 5558 | 10.25 | 3.21 | 0.50 |
| 3| 5657 | 11.06 | 3.46 | 0.51 |
| 5| 5849 | 12.21 | 3.80 | 0.53 |
| 10| 6330 | 15.75 | 4.87 | 0.59 |
| 50| 10175 | 42.17 | 12.64 | 1.03 |
| 100| 14972 | 76.31 | 22.70 | 1.59 |
| 114| 16316 | 85.64 | 25.46 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 18.47 | 6.43 | 0.42 |
| 2| 1916 | 19.78 | 7.48 | 0.45 |
| 3| 2051 | 20.87 | 8.47 | 0.47 |
| 5| 2314 | 22.92 | 10.42 | 0.51 |
| 10| 2968 | 28.64 | 15.45 | 0.62 |
| 50| 8213 | 74.18 | 55.60 | 1.51 |
| 71| 10965 | 99.97 | 77.16 | 2.00 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.06 | 7.02 | 0.38 |
| 3| 853 | 19.20 | 8.03 | 0.40 |
| 5| 1120 | 21.30 | 9.99 | 0.44 |
| 10| 1770 | 27.00 | 15.02 | 0.55 |
| 50| 7016 | 73.61 | 55.44 | 1.45 |
| 74| 10154 | 99.64 | 79.19 | 1.97 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 20.62 | 6.87 | 0.39 |
| 2| 732 | 23.34 | 8.32 | 0.43 |
| 3| 898 | 23.69 | 9.09 | 0.45 |
| 5| 1126 | 28.37 | 11.76 | 0.51 |
| 10| 1811 | 36.07 | 17.29 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.14 | 8.14 | 0.44 |
| 2| 761 | 26.86 | 9.31 | 0.47 |
| 3| 892 | 28.71 | 10.50 | 0.50 |
| 5| 1154 | 32.42 | 12.90 | 0.56 |
| 10| 1809 | 42.66 | 19.14 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1138 | 6206 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1707 | 6547 | 69.61 | 27.91 | 1.20 |
| 10 | 45 | 2559 | 7055 | 96.98 | 39.28 | 1.53 |

