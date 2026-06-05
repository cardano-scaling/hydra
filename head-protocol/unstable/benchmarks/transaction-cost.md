--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-05 16:01:42.792430829 UTC |
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
| 1| 5403 | 8.92 | 2.94 | 0.48 |
| 2| 5497 | 9.97 | 3.29 | 0.50 |
| 3| 5594 | 10.04 | 3.30 | 0.50 |
| 5| 5788 | 11.03 | 3.61 | 0.52 |
| 10| 6266 | 13.63 | 4.44 | 0.57 |
| 50| 10111 | 34.59 | 10.98 | 0.95 |
| 100| 14911 | 61.72 | 19.49 | 1.45 |
| 115| 16350 | 69.86 | 22.03 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2305 | 12.41 | 4.86 | 0.39 |
| 2| 2437 | 12.98 | 5.74 | 0.40 |
| 3| 2565 | 13.75 | 6.68 | 0.42 |
| 5| 2828 | 15.01 | 8.51 | 0.46 |
| 10| 3484 | 17.50 | 12.78 | 0.54 |
| 50| 8725 | 43.25 | 48.86 | 1.24 |
| 75| 11997 | 58.36 | 71.11 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 10.31 | 4.21 | 0.29 |
| 2| 754 | 10.86 | 5.09 | 0.31 |
| 3| 887 | 11.43 | 5.97 | 0.32 |
| 5| 1149 | 12.55 | 7.72 | 0.36 |
| 10| 1804 | 15.37 | 12.12 | 0.44 |
| 50| 7045 | 38.86 | 47.48 | 1.12 |
| 75| 10321 | 54.16 | 69.76 | 1.54 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 18.94 | 12.53 | 0.42 |
| 2| 813 | 20.14 | 13.60 | 0.45 |
| 3| 949 | 21.38 | 14.67 | 0.47 |
| 5| 1210 | 24.11 | 16.89 | 0.52 |
| 10| 1866 | 31.58 | 22.61 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 709 | 22.66 | 15.77 | 0.48 |
| 2| 844 | 24.03 | 16.88 | 0.50 |
| 3| 976 | 25.50 | 18.03 | 0.53 |
| 5| 1237 | 28.51 | 20.33 | 0.58 |
| 10| 1893 | 36.79 | 26.30 | 0.72 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5434 | 14.42 | 5.63 | 0.55 |
| 10 | 1 | 57 | 5467 | 16.22 | 6.40 | 0.57 |
| 10 | 5 | 284 | 5603 | 23.43 | 9.45 | 0.65 |
| 10 | 10 | 569 | 5772 | 32.44 | 13.26 | 0.76 |
| 10 | 20 | 1138 | 6111 | 50.63 | 20.93 | 0.98 |
| 10 | 40 | 2275 | 6791 | 87.05 | 36.27 | 1.41 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 979 | 32.74 | 63.63 | 0.92 |
| 25 | 1366 | 1452 | 69.05 | 99.37 | 1.49 |
| 30 | 1366 | 1453 | 69.05 | 99.37 | 1.49 |
| 40 | 1366 | 1457 | 69.05 | 99.37 | 1.49 |
| 50 | 1364 | 1455 | 69.05 | 99.37 | 1.49 |
| 100 | 1365 | 1452 | 69.05 | 99.37 | 1.49 |
| 150 | 1366 | 1457 | 69.05 | 99.37 | 1.49 |
| 200 | 1366 | 1457 | 69.05 | 99.37 | 1.49 |
| 200 | 1368 | 1459 | 69.05 | 99.37 | 1.49 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 960 | 1429 | 39.83 | 66.13 | 1.01 |
| 25 | 2442 | 2670 | 77.90 | 99.37 | 1.61 |
| 30 | 2398 | 2624 | 77.92 | 99.33 | 1.61 |
| 40 | 2068 | 2283 | 77.92 | 99.28 | 1.60 |
| 50 | 2134 | 2353 | 77.92 | 99.28 | 1.60 |
| 100 | 2090 | 2303 | 77.92 | 99.28 | 1.60 |
| 150 | 2310 | 2537 | 77.92 | 99.33 | 1.61 |
| 200 | 2376 | 2606 | 77.92 | 99.33 | 1.61 |
| 200 | 2332 | 2560 | 77.92 | 99.33 | 1.61 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 96 | 5429 | 22.06 | 43.34 | 0.88 |
| 5 | 555 | 5803 | 35.67 | 54.88 | 1.09 |
| 10 | 1090 | 6233 | 53.87 | 69.66 | 1.36 |
| 10 | 1030 | 6174 | 53.87 | 69.66 | 1.36 |

