--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 12:28:41.552619145 UTC |
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
| 1| 5464 | 9.57 | 3.01 | 0.49 |
| 2| 5560 | 10.24 | 3.21 | 0.50 |
| 3| 5657 | 10.86 | 3.39 | 0.51 |
| 5| 5851 | 12.16 | 3.78 | 0.53 |
| 10| 6329 | 16.11 | 5.00 | 0.59 |
| 50| 10171 | 42.35 | 12.69 | 1.03 |
| 100| 14973 | 76.21 | 22.68 | 1.59 |
| 114| 16315 | 85.33 | 25.36 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1792 | 18.67 | 6.48 | 0.43 |
| 2| 1918 | 19.79 | 7.48 | 0.45 |
| 3| 2049 | 20.87 | 8.47 | 0.47 |
| 5| 2311 | 22.87 | 10.41 | 0.51 |
| 10| 2968 | 28.56 | 15.43 | 0.62 |
| 50| 8206 | 74.92 | 55.79 | 1.52 |
| 71| 10961 | 99.30 | 76.98 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 589 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.20 | 8.03 | 0.40 |
| 5| 1115 | 21.35 | 10.00 | 0.44 |
| 10| 1770 | 26.97 | 15.01 | 0.55 |
| 50| 7011 | 72.40 | 55.12 | 1.44 |
| 73| 10024 | 99.34 | 78.40 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 20.65 | 6.88 | 0.39 |
| 2| 732 | 23.39 | 8.33 | 0.43 |
| 3| 864 | 24.94 | 9.45 | 0.46 |
| 5| 1165 | 26.97 | 11.37 | 0.50 |
| 10| 1781 | 37.94 | 17.81 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 25.16 | 8.15 | 0.44 |
| 2| 756 | 26.91 | 9.32 | 0.47 |
| 3| 887 | 28.63 | 10.48 | 0.50 |
| 5| 1158 | 32.34 | 12.88 | 0.56 |
| 10| 1809 | 42.81 | 19.18 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1139 | 6207 | 51.38 | 20.34 | 0.99 |
| 10 | 46 | 2616 | 7088 | 98.80 | 40.03 | 1.55 |

