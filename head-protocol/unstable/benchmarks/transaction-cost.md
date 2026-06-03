--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-03 21:11:13.180538561 UTC |
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
| 1| 5404 | 8.87 | 2.91 | 0.48 |
| 2| 5499 | 9.18 | 3.01 | 0.49 |
| 3| 5596 | 9.86 | 3.23 | 0.50 |
| 5| 5788 | 10.62 | 3.47 | 0.52 |
| 10| 6269 | 13.75 | 4.48 | 0.57 |
| 50| 10111 | 34.71 | 11.03 | 0.96 |
| 100| 14911 | 61.23 | 19.32 | 1.44 |
| 115| 16355 | 69.56 | 21.94 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2304 | 12.41 | 4.86 | 0.39 |
| 2| 2435 | 13.30 | 5.87 | 0.41 |
| 3| 2565 | 13.59 | 6.64 | 0.42 |
| 5| 2827 | 14.87 | 8.47 | 0.46 |
| 10| 3483 | 18.00 | 12.96 | 0.54 |
| 50| 8726 | 42.88 | 48.72 | 1.23 |
| 75| 11999 | 58.69 | 71.17 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 10.31 | 4.21 | 0.29 |
| 2| 755 | 10.88 | 5.09 | 0.31 |
| 3| 887 | 11.43 | 5.97 | 0.32 |
| 5| 1149 | 12.59 | 7.73 | 0.36 |
| 10| 1804 | 15.31 | 12.11 | 0.44 |
| 50| 7049 | 39.08 | 47.55 | 1.12 |
| 75| 10320 | 55.02 | 69.99 | 1.55 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 18.92 | 12.52 | 0.42 |
| 2| 783 | 21.13 | 14.43 | 0.46 |
| 3| 944 | 21.38 | 14.67 | 0.47 |
| 5| 1176 | 25.20 | 17.75 | 0.53 |
| 10| 1865 | 31.56 | 22.60 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 710 | 22.64 | 15.76 | 0.48 |
| 2| 844 | 24.04 | 16.89 | 0.50 |
| 3| 975 | 25.48 | 18.02 | 0.53 |
| 5| 1238 | 28.53 | 20.34 | 0.58 |
| 10| 1893 | 36.81 | 26.30 | 0.72 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5433 | 14.42 | 5.63 | 0.55 |
| 10 | 1 | 57 | 5467 | 16.22 | 6.40 | 0.57 |
| 10 | 5 | 286 | 5604 | 23.43 | 9.45 | 0.65 |
| 10 | 10 | 570 | 5773 | 32.44 | 13.26 | 0.76 |
| 10 | 20 | 1138 | 6112 | 50.63 | 20.93 | 0.98 |
| 10 | 40 | 2279 | 6795 | 87.05 | 36.27 | 1.41 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 980 | 32.74 | 63.63 | 0.92 |
| 25 | 1367 | 1457 | 69.05 | 99.37 | 1.49 |
| 30 | 1367 | 1454 | 69.05 | 99.37 | 1.49 |
| 40 | 1368 | 1459 | 69.05 | 99.37 | 1.49 |
| 50 | 1364 | 1455 | 69.05 | 99.37 | 1.49 |
| 100 | 1364 | 1455 | 69.05 | 99.37 | 1.49 |
| 150 | 1368 | 1459 | 69.05 | 99.37 | 1.49 |
| 200 | 1367 | 1458 | 69.05 | 99.37 | 1.49 |
| 200 | 1365 | 1456 | 69.05 | 99.37 | 1.49 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1020 | 1495 | 39.83 | 66.15 | 1.01 |
| 25 | 2684 | 2927 | 77.92 | 99.44 | 1.63 |
| 30 | 2112 | 2325 | 77.92 | 99.28 | 1.60 |
| 40 | 2706 | 2950 | 77.92 | 99.44 | 1.63 |
| 50 | 2200 | 2422 | 77.92 | 99.28 | 1.60 |
| 100 | 2134 | 2353 | 77.92 | 99.28 | 1.60 |
| 150 | 2530 | 2767 | 77.90 | 99.38 | 1.62 |
| 200 | 2090 | 2307 | 77.92 | 99.28 | 1.60 |
| 200 | 2662 | 2905 | 77.92 | 99.44 | 1.62 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 102 | 5434 | 22.06 | 43.34 | 0.88 |
| 5 | 610 | 5859 | 35.67 | 54.89 | 1.09 |
| 10 | 960 | 6103 | 53.87 | 69.63 | 1.36 |
| 10 | 990 | 6133 | 53.87 | 69.64 | 1.36 |

