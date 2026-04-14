--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-14 09:40:14.522190198 UTC |
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
| 1| 5462 | 9.57 | 3.01 | 0.49 |
| 2| 5560 | 10.41 | 3.27 | 0.50 |
| 3| 5661 | 10.86 | 3.39 | 0.51 |
| 5| 5849 | 12.35 | 3.85 | 0.54 |
| 10| 6329 | 15.91 | 4.93 | 0.59 |
| 50| 10172 | 42.43 | 12.72 | 1.03 |
| 100| 14971 | 76.14 | 22.65 | 1.59 |
| 114| 16313 | 85.79 | 25.49 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.56 | 7.42 | 0.45 |
| 3| 2050 | 20.67 | 8.42 | 0.47 |
| 5| 2314 | 23.05 | 10.45 | 0.51 |
| 10| 2968 | 28.59 | 15.44 | 0.62 |
| 50| 8207 | 74.81 | 55.76 | 1.52 |
| 72| 11091 | 99.62 | 77.77 | 2.00 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.95 | 6.02 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 856 | 19.17 | 8.02 | 0.40 |
| 5| 1116 | 21.38 | 10.01 | 0.44 |
| 10| 1770 | 26.84 | 14.98 | 0.55 |
| 50| 7011 | 73.66 | 55.46 | 1.45 |
| 74| 10155 | 99.77 | 79.22 | 1.97 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 602 | 21.82 | 7.21 | 0.41 |
| 2| 766 | 22.14 | 7.98 | 0.42 |
| 3| 902 | 23.67 | 9.08 | 0.45 |
| 5| 1160 | 26.84 | 11.33 | 0.50 |
| 10| 1781 | 37.78 | 17.77 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.16 | 8.15 | 0.44 |
| 2| 756 | 26.91 | 9.32 | 0.47 |
| 3| 896 | 28.71 | 10.50 | 0.50 |
| 5| 1154 | 32.47 | 12.91 | 0.56 |
| 10| 1809 | 42.61 | 19.12 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1141 | 6209 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1708 | 6548 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2277 | 6888 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2618 | 7091 | 98.80 | 40.03 | 1.55 |

