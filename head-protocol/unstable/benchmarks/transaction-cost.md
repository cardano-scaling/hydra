--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-05 14:53:02.670761886 UTC |
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
| 1| 5404 | 8.63 | 2.83 | 0.48 |
| 2| 5499 | 9.22 | 3.03 | 0.49 |
| 3| 5597 | 9.66 | 3.16 | 0.50 |
| 5| 5788 | 10.85 | 3.55 | 0.52 |
| 10| 6266 | 13.15 | 4.27 | 0.56 |
| 50| 10114 | 34.84 | 11.06 | 0.96 |
| 100| 14910 | 61.55 | 19.42 | 1.44 |
| 115| 16350 | 69.19 | 21.82 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2301 | 12.41 | 4.86 | 0.39 |
| 2| 2436 | 13.34 | 5.88 | 0.41 |
| 3| 2567 | 13.91 | 6.76 | 0.42 |
| 5| 2829 | 15.07 | 8.53 | 0.46 |
| 10| 3482 | 17.77 | 12.89 | 0.54 |
| 50| 8725 | 43.02 | 48.76 | 1.23 |
| 75| 11997 | 59.39 | 71.43 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 10.29 | 4.21 | 0.29 |
| 2| 756 | 10.88 | 5.09 | 0.31 |
| 3| 885 | 11.41 | 5.96 | 0.32 |
| 5| 1149 | 12.55 | 7.72 | 0.36 |
| 10| 1804 | 15.35 | 12.12 | 0.44 |
| 50| 7045 | 39.50 | 47.66 | 1.12 |
| 75| 10320 | 53.78 | 69.64 | 1.54 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 652 | 19.88 | 13.35 | 0.44 |
| 2| 817 | 20.14 | 13.60 | 0.45 |
| 3| 915 | 22.42 | 15.52 | 0.48 |
| 5| 1210 | 24.07 | 16.88 | 0.52 |
| 10| 1866 | 31.63 | 22.63 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 714 | 22.66 | 15.77 | 0.48 |
| 2| 845 | 24.06 | 16.89 | 0.50 |
| 3| 975 | 25.45 | 18.01 | 0.53 |
| 5| 1237 | 28.49 | 20.33 | 0.58 |
| 10| 1893 | 36.97 | 26.35 | 0.72 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5433 | 14.42 | 5.63 | 0.55 |
| 10 | 1 | 57 | 5467 | 16.22 | 6.40 | 0.57 |
| 10 | 10 | 570 | 5774 | 32.44 | 13.26 | 0.76 |
| 10 | 20 | 1139 | 6113 | 50.63 | 20.93 | 0.98 |
| 10 | 30 | 1709 | 6454 | 68.83 | 28.60 | 1.20 |
| 10 | 30 | 1709 | 6454 | 68.83 | 28.60 | 1.20 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 979 | 32.74 | 63.63 | 0.92 |
| 25 | 1365 | 1455 | 69.05 | 99.37 | 1.49 |
| 30 | 1365 | 1456 | 69.05 | 99.37 | 1.49 |
| 40 | 1367 | 1458 | 69.05 | 99.37 | 1.49 |
| 50 | 1365 | 1456 | 69.05 | 99.37 | 1.49 |
| 100 | 1365 | 1456 | 69.05 | 99.37 | 1.49 |
| 150 | 1367 | 1458 | 69.05 | 99.37 | 1.49 |
| 200 | 1365 | 1452 | 69.05 | 99.37 | 1.49 |
| 200 | 1366 | 1457 | 69.05 | 99.37 | 1.49 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1120 | 1605 | 39.83 | 66.17 | 1.02 |
| 25 | 2376 | 2605 | 77.90 | 99.33 | 1.61 |
| 30 | 2398 | 2628 | 77.92 | 99.33 | 1.61 |
| 40 | 2046 | 2260 | 77.90 | 99.27 | 1.59 |
| 50 | 2332 | 2560 | 77.92 | 99.33 | 1.61 |
| 100 | 2464 | 2698 | 77.90 | 99.37 | 1.61 |
| 150 | 2398 | 2629 | 77.92 | 99.33 | 1.61 |
| 200 | 2222 | 2441 | 77.90 | 99.28 | 1.60 |
| 200 | 2684 | 2928 | 77.90 | 99.43 | 1.63 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 112 | 5444 | 22.06 | 43.34 | 0.88 |
| 5 | 540 | 5788 | 35.67 | 54.86 | 1.09 |
| 10 | 1090 | 6234 | 53.87 | 69.66 | 1.36 |
| 10 | 980 | 6123 | 53.87 | 69.63 | 1.36 |

