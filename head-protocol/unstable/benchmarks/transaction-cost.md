--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-05 17:39:27.747155714 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | 408b9fd7444e51bc757889b761dbb3b01b4ddc4e12550884dfe33a7c | 13339 | 
| μHead | 66958cc6e8a9be569bd72cd1d44d8e5c757bd9a0b01da7789eb5a4da* | 4883 | 
| νDeposit | 0430c1afbb704085b936bd7fd530b5c674009c0b85fd18e7a8494ad8 | 1615 | 
| νCRS | 09db7ee6cf7a4b358dd5c8a2f19d2c048336ffc5a01ef35a47ca7072 | 2736 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5403 | 9.04 | 2.98 | 0.48 |
| 2| 5500 | 9.34 | 3.07 | 0.49 |
| 3| 5596 | 10.03 | 3.29 | 0.50 |
| 5| 5792 | 11.03 | 3.61 | 0.52 |
| 10| 6269 | 13.13 | 4.27 | 0.56 |
| 50| 10111 | 34.57 | 10.97 | 0.95 |
| 100| 14910 | 61.96 | 19.57 | 1.45 |
| 115| 16350 | 69.63 | 21.95 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2308 | 12.67 | 4.96 | 0.39 |
| 2| 2436 | 12.96 | 5.73 | 0.40 |
| 3| 2567 | 13.79 | 6.72 | 0.42 |
| 5| 2827 | 15.17 | 8.56 | 0.46 |
| 10| 3484 | 17.74 | 12.85 | 0.54 |
| 50| 8729 | 43.15 | 48.81 | 1.23 |
| 75| 11999 | 59.31 | 71.39 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 10.31 | 4.21 | 0.29 |
| 2| 756 | 10.88 | 5.09 | 0.31 |
| 3| 887 | 11.41 | 5.96 | 0.32 |
| 5| 1149 | 12.55 | 7.72 | 0.36 |
| 10| 1804 | 15.38 | 12.13 | 0.44 |
| 50| 7045 | 39.42 | 47.63 | 1.12 |
| 75| 10321 | 54.84 | 69.95 | 1.55 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 19.88 | 13.34 | 0.44 |
| 2| 784 | 21.13 | 14.43 | 0.46 |
| 3| 911 | 22.42 | 15.52 | 0.48 |
| 5| 1180 | 25.20 | 17.75 | 0.53 |
| 10| 1827 | 33.04 | 23.58 | 0.67 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 713 | 22.66 | 15.77 | 0.48 |
| 2| 839 | 24.04 | 16.89 | 0.50 |
| 3| 976 | 25.45 | 18.01 | 0.53 |
| 5| 1242 | 28.46 | 20.32 | 0.58 |
| 10| 1893 | 36.92 | 26.33 | 0.72 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5467 | 16.22 | 6.40 | 0.57 |
| 10 | 5 | 283 | 5602 | 23.43 | 9.45 | 0.65 |
| 10 | 30 | 1705 | 6450 | 68.83 | 28.60 | 1.20 |
| 10 | 40 | 2280 | 6796 | 87.05 | 36.27 | 1.41 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 568 | 978 | 32.74 | 63.63 | 0.92 |
| 25 | 1365 | 1455 | 69.05 | 99.37 | 1.49 |
| 30 | 1363 | 1454 | 69.05 | 99.37 | 1.49 |
| 40 | 1368 | 1459 | 69.05 | 99.37 | 1.49 |
| 50 | 1366 | 1457 | 69.05 | 99.37 | 1.49 |
| 100 | 1366 | 1457 | 69.05 | 99.37 | 1.49 |
| 150 | 1365 | 1456 | 69.05 | 99.37 | 1.49 |
| 200 | 1368 | 1459 | 69.05 | 99.37 | 1.49 |
| 200 | 1366 | 1457 | 69.05 | 99.37 | 1.49 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1030 | 1506 | 39.83 | 66.15 | 1.02 |
| 25 | 2090 | 2306 | 77.92 | 99.28 | 1.60 |
| 30 | 2134 | 2352 | 77.92 | 99.28 | 1.60 |
| 40 | 2244 | 2467 | 77.92 | 99.33 | 1.60 |
| 50 | 2112 | 2330 | 77.92 | 99.28 | 1.60 |
| 100 | 2552 | 2790 | 77.92 | 99.39 | 1.62 |
| 150 | 2178 | 2399 | 77.90 | 99.28 | 1.60 |
| 200 | 2046 | 2261 | 77.92 | 99.28 | 1.60 |
| 200 | 2046 | 2261 | 77.92 | 99.28 | 1.60 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 103 | 5435 | 22.06 | 43.34 | 0.88 |
| 5 | 475 | 5723 | 35.67 | 54.85 | 1.09 |
| 10 | 970 | 6113 | 53.87 | 69.63 | 1.36 |
| 10 | 1040 | 6184 | 53.87 | 69.66 | 1.36 |

