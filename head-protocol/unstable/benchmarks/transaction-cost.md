--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-09 16:12:02.279335643 UTC |
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
| 1| 5465 | 10.00 | 3.16 | 0.49 |
| 2| 5560 | 10.83 | 3.41 | 0.51 |
| 3| 5656 | 10.85 | 3.39 | 0.51 |
| 5| 5850 | 12.53 | 3.91 | 0.54 |
| 10| 6327 | 15.54 | 4.80 | 0.59 |
| 50| 10171 | 42.17 | 12.64 | 1.03 |
| 100| 14971 | 76.19 | 22.67 | 1.59 |
| 114| 16315 | 85.16 | 25.31 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 18.47 | 6.43 | 0.42 |
| 2| 1919 | 19.58 | 7.43 | 0.45 |
| 3| 2050 | 20.90 | 8.48 | 0.47 |
| 5| 2316 | 23.02 | 10.45 | 0.51 |
| 10| 2968 | 28.54 | 15.43 | 0.62 |
| 50| 8213 | 75.50 | 55.94 | 1.52 |
| 72| 11092 | 99.52 | 77.75 | 2.00 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 591 | 16.95 | 6.02 | 0.36 |
| 2| 720 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.17 | 8.02 | 0.40 |
| 5| 1116 | 21.43 | 10.02 | 0.44 |
| 10| 1769 | 26.79 | 14.97 | 0.55 |
| 50| 7010 | 73.52 | 55.42 | 1.45 |
| 74| 10155 | 99.75 | 79.21 | 1.97 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 20.62 | 6.87 | 0.39 |
| 2| 766 | 22.09 | 7.96 | 0.42 |
| 3| 898 | 23.64 | 9.08 | 0.45 |
| 5| 1156 | 26.97 | 11.37 | 0.50 |
| 10| 1777 | 38.02 | 17.83 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.14 | 8.14 | 0.44 |
| 2| 760 | 26.91 | 9.32 | 0.47 |
| 3| 891 | 28.71 | 10.50 | 0.50 |
| 5| 1149 | 32.39 | 12.89 | 0.56 |
| 10| 1804 | 42.84 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 10 | 570 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1138 | 6206 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1703 | 6544 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2278 | 6888 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2619 | 7091 | 98.80 | 40.03 | 1.55 |

