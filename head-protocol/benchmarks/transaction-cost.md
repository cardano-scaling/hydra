--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 15:27:29.515865326 UTC |
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
| 1| 5462 | 10.00 | 3.16 | 0.49 |
| 2| 5561 | 10.63 | 3.35 | 0.51 |
| 3| 5656 | 11.32 | 3.56 | 0.52 |
| 5| 5849 | 12.19 | 3.79 | 0.53 |
| 10| 6329 | 15.39 | 4.75 | 0.59 |
| 50| 10176 | 42.58 | 12.76 | 1.03 |
| 100| 14969 | 75.83 | 22.55 | 1.58 |
| 114| 16316 | 85.48 | 25.39 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1786 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.76 | 7.47 | 0.45 |
| 3| 2050 | 20.90 | 8.48 | 0.47 |
| 5| 2311 | 22.82 | 10.39 | 0.51 |
| 10| 2968 | 28.67 | 15.46 | 0.62 |
| 50| 8208 | 75.42 | 55.92 | 1.52 |
| 71| 10961 | 98.77 | 76.85 | 1.98 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 591 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.06 | 7.02 | 0.38 |
| 3| 852 | 19.17 | 8.02 | 0.40 |
| 5| 1115 | 21.43 | 10.02 | 0.44 |
| 10| 1769 | 26.82 | 14.97 | 0.55 |
| 50| 7011 | 73.58 | 55.43 | 1.45 |
| 72| 9893 | 99.12 | 77.64 | 1.95 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 602 | 21.80 | 7.20 | 0.40 |
| 2| 732 | 23.37 | 8.32 | 0.43 |
| 3| 864 | 24.99 | 9.46 | 0.46 |
| 5| 1122 | 28.34 | 11.75 | 0.51 |
| 10| 1815 | 35.97 | 17.26 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.14 | 8.14 | 0.44 |
| 2| 760 | 26.89 | 9.31 | 0.47 |
| 3| 887 | 28.63 | 10.48 | 0.50 |
| 5| 1153 | 32.44 | 12.90 | 0.56 |
| 10| 1805 | 42.68 | 19.14 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 284 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1140 | 6208 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1708 | 6548 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2278 | 6888 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2618 | 7090 | 98.80 | 40.03 | 1.55 |

