--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 13:38:17.80429552 UTC |
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
| 1| 5468 | 9.59 | 3.01 | 0.49 |
| 2| 5558 | 10.42 | 3.27 | 0.50 |
| 3| 5660 | 11.24 | 3.53 | 0.52 |
| 5| 5850 | 12.81 | 4.01 | 0.54 |
| 10| 6333 | 15.28 | 4.71 | 0.59 |
| 50| 10171 | 42.75 | 12.83 | 1.04 |
| 100| 14972 | 76.33 | 22.70 | 1.59 |
| 114| 16315 | 85.94 | 25.54 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.47 | 6.43 | 0.42 |
| 2| 1919 | 19.61 | 7.43 | 0.45 |
| 3| 2047 | 20.69 | 8.42 | 0.47 |
| 5| 2316 | 23.08 | 10.46 | 0.51 |
| 10| 2968 | 28.28 | 15.36 | 0.62 |
| 50| 8210 | 74.71 | 55.74 | 1.52 |
| 71| 10962 | 99.59 | 77.06 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 589 | 16.95 | 6.02 | 0.36 |
| 2| 725 | 18.03 | 7.01 | 0.38 |
| 3| 852 | 19.17 | 8.02 | 0.40 |
| 5| 1115 | 21.40 | 10.02 | 0.44 |
| 10| 1770 | 26.89 | 14.99 | 0.55 |
| 50| 7012 | 73.44 | 55.40 | 1.45 |
| 74| 10158 | 99.75 | 79.21 | 1.97 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 20.62 | 6.87 | 0.39 |
| 2| 728 | 23.37 | 8.32 | 0.43 |
| 3| 902 | 23.64 | 9.08 | 0.45 |
| 5| 1160 | 26.91 | 11.35 | 0.50 |
| 10| 1785 | 37.76 | 17.77 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 633 | 25.14 | 8.14 | 0.44 |
| 2| 761 | 26.91 | 9.32 | 0.47 |
| 3| 896 | 28.66 | 10.49 | 0.50 |
| 5| 1154 | 32.34 | 12.88 | 0.56 |
| 10| 1813 | 42.74 | 19.16 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 284 | 5697 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 568 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 30 | 1709 | 6549 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2275 | 6886 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2616 | 7088 | 98.80 | 40.03 | 1.55 |

