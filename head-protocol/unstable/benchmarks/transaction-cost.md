--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-04 08:42:56.79643155 UTC |
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
| 1| 5403 | 8.80 | 2.90 | 0.48 |
| 2| 5497 | 9.77 | 3.22 | 0.49 |
| 3| 5596 | 10.08 | 3.31 | 0.50 |
| 5| 5788 | 10.65 | 3.48 | 0.52 |
| 10| 6268 | 13.33 | 4.34 | 0.57 |
| 50| 10111 | 34.92 | 11.10 | 0.96 |
| 100| 14908 | 61.75 | 19.49 | 1.45 |
| 115| 16351 | 69.59 | 21.92 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2304 | 12.41 | 4.86 | 0.39 |
| 2| 2435 | 13.08 | 5.77 | 0.40 |
| 3| 2565 | 13.91 | 6.76 | 0.42 |
| 5| 2829 | 14.99 | 8.50 | 0.46 |
| 10| 3483 | 18.02 | 12.97 | 0.54 |
| 50| 8724 | 43.22 | 48.82 | 1.24 |
| 75| 12000 | 59.65 | 71.45 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 10.29 | 4.21 | 0.29 |
| 2| 755 | 10.88 | 5.09 | 0.31 |
| 3| 889 | 11.40 | 5.96 | 0.32 |
| 5| 1149 | 12.52 | 7.71 | 0.36 |
| 10| 1804 | 15.37 | 12.12 | 0.44 |
| 50| 7044 | 39.36 | 47.62 | 1.12 |
| 75| 10323 | 55.00 | 69.98 | 1.55 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 18.94 | 12.53 | 0.42 |
| 2| 817 | 20.16 | 13.60 | 0.45 |
| 3| 914 | 22.44 | 15.52 | 0.48 |
| 5| 1176 | 25.22 | 17.76 | 0.53 |
| 10| 1865 | 31.56 | 22.60 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 709 | 22.66 | 15.77 | 0.48 |
| 2| 845 | 24.04 | 16.89 | 0.50 |
| 3| 976 | 25.47 | 18.02 | 0.53 |
| 5| 1238 | 28.49 | 20.33 | 0.58 |
| 10| 1892 | 36.90 | 26.32 | 0.72 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5433 | 14.42 | 5.63 | 0.55 |
| 10 | 1 | 57 | 5468 | 16.22 | 6.40 | 0.57 |
| 10 | 20 | 1140 | 6113 | 50.63 | 20.93 | 0.98 |
| 10 | 30 | 1708 | 6453 | 68.83 | 28.60 | 1.20 |
| 10 | 40 | 2278 | 6793 | 87.05 | 36.27 | 1.41 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 980 | 32.74 | 63.63 | 0.92 |
| 25 | 1367 | 1453 | 69.05 | 99.37 | 1.49 |
| 30 | 1369 | 1460 | 69.05 | 99.37 | 1.49 |
| 40 | 1365 | 1456 | 69.05 | 99.37 | 1.49 |
| 50 | 1367 | 1454 | 69.05 | 99.37 | 1.49 |
| 100 | 1365 | 1456 | 69.05 | 99.37 | 1.49 |
| 150 | 1365 | 1456 | 69.05 | 99.37 | 1.49 |
| 200 | 1367 | 1458 | 69.05 | 99.37 | 1.49 |
| 200 | 1367 | 1454 | 69.05 | 99.37 | 1.49 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1230 | 1726 | 39.83 | 66.20 | 1.03 |
| 25 | 2398 | 2624 | 77.92 | 99.33 | 1.61 |
| 30 | 2332 | 2559 | 77.92 | 99.33 | 1.61 |
| 40 | 2156 | 2375 | 77.92 | 99.28 | 1.60 |
| 50 | 2486 | 2717 | 77.92 | 99.39 | 1.62 |
| 100 | 2464 | 2698 | 77.90 | 99.37 | 1.61 |
| 150 | 2684 | 2928 | 77.92 | 99.44 | 1.63 |
| 200 | 2706 | 2951 | 77.90 | 99.43 | 1.63 |
| 200 | 2332 | 2556 | 77.92 | 99.33 | 1.61 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 123 | 5455 | 22.06 | 43.34 | 0.88 |
| 5 | 545 | 5793 | 35.67 | 54.86 | 1.09 |
| 10 | 1060 | 6204 | 53.87 | 69.66 | 1.36 |
| 10 | 1070 | 6213 | 53.87 | 69.66 | 1.36 |

