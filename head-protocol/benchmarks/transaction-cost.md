--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 12:55:49.272998363 UTC |
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
| 1| 5464 | 10.00 | 3.16 | 0.49 |
| 2| 5561 | 10.22 | 3.20 | 0.50 |
| 3| 5656 | 10.87 | 3.40 | 0.51 |
| 5| 5850 | 12.69 | 3.96 | 0.54 |
| 10| 6329 | 15.51 | 4.79 | 0.59 |
| 50| 10171 | 42.56 | 12.76 | 1.03 |
| 100| 14976 | 76.42 | 22.75 | 1.59 |
| 114| 16313 | 85.21 | 25.32 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.47 | 6.43 | 0.42 |
| 2| 1919 | 19.61 | 7.43 | 0.45 |
| 3| 2049 | 20.85 | 8.46 | 0.47 |
| 5| 2313 | 22.84 | 10.40 | 0.51 |
| 10| 2968 | 28.59 | 15.44 | 0.62 |
| 50| 8208 | 75.42 | 55.92 | 1.52 |
| 71| 10958 | 99.34 | 77.00 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 720 | 18.06 | 7.02 | 0.38 |
| 3| 852 | 19.15 | 8.01 | 0.40 |
| 5| 1115 | 21.38 | 10.01 | 0.44 |
| 10| 1769 | 26.82 | 14.97 | 0.55 |
| 50| 7011 | 72.83 | 55.24 | 1.45 |
| 74| 10156 | 99.90 | 79.25 | 1.98 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 21.82 | 7.21 | 0.41 |
| 2| 732 | 23.37 | 8.32 | 0.43 |
| 3| 898 | 23.61 | 9.07 | 0.45 |
| 5| 1160 | 26.97 | 11.37 | 0.50 |
| 10| 1782 | 37.96 | 17.82 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.91 | 9.32 | 0.47 |
| 3| 887 | 28.63 | 10.48 | 0.50 |
| 5| 1149 | 32.36 | 12.88 | 0.56 |
| 10| 1808 | 42.68 | 19.14 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5699 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1138 | 6206 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1704 | 6544 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2283 | 6893 | 87.85 | 35.49 | 1.42 |
| 10 | 45 | 2564 | 7060 | 96.98 | 39.28 | 1.53 |

