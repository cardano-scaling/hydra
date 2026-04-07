--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-07 09:06:13.548656954 UTC |
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
| 1| 5465 | 9.94 | 3.14 | 0.49 |
| 2| 5560 | 10.22 | 3.20 | 0.50 |
| 3| 5654 | 11.27 | 3.54 | 0.52 |
| 5| 5847 | 12.23 | 3.81 | 0.53 |
| 10| 6331 | 15.76 | 4.88 | 0.59 |
| 50| 10172 | 42.21 | 12.66 | 1.03 |
| 100| 14971 | 76.33 | 22.72 | 1.59 |
| 114| 16315 | 85.77 | 25.49 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 18.47 | 6.43 | 0.42 |
| 2| 1920 | 19.61 | 7.43 | 0.45 |
| 3| 2050 | 20.87 | 8.47 | 0.47 |
| 5| 2312 | 23.15 | 10.48 | 0.51 |
| 10| 2968 | 28.49 | 15.42 | 0.62 |
| 50| 8213 | 75.39 | 55.91 | 1.52 |
| 72| 11092 | 99.98 | 77.87 | 2.01 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 851 | 19.15 | 8.01 | 0.40 |
| 5| 1115 | 21.35 | 10.00 | 0.44 |
| 10| 1771 | 26.97 | 15.01 | 0.55 |
| 50| 7011 | 72.74 | 55.21 | 1.44 |
| 73| 10024 | 99.09 | 78.34 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 21.82 | 7.21 | 0.41 |
| 2| 766 | 22.14 | 7.98 | 0.42 |
| 3| 898 | 23.69 | 9.09 | 0.45 |
| 5| 1160 | 26.86 | 11.34 | 0.50 |
| 10| 1820 | 36.02 | 17.28 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.91 | 9.32 | 0.47 |
| 3| 891 | 28.63 | 10.48 | 0.50 |
| 5| 1150 | 32.44 | 12.90 | 0.56 |
| 10| 1813 | 42.61 | 19.12 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 284 | 5697 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 569 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 40 | 2278 | 6889 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2619 | 7091 | 98.80 | 40.03 | 1.55 |

