--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-08 08:01:56.224278537 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | 50208d59904e05820428c613c16e8b98749dcb7b181a7d537cdf8080 | 13279 | 
| μHead | 2d4f9e81be38eec8263bae02a29524d68191a40302fb3de50b459f31* | 4883 | 
| νDeposit | c78e8c9205721eb3ef4410f3db9c6169fa6db497c24641d29c20529c | 1615 | 
| νCRS | 09db7ee6cf7a4b358dd5c8a2f19d2c048336ffc5a01ef35a47ca7072 | 2736 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5404 | 9.42 | 3.12 | 0.49 |
| 2| 5497 | 9.16 | 3.00 | 0.49 |
| 3| 5594 | 10.44 | 3.45 | 0.51 |
| 5| 5786 | 10.65 | 3.48 | 0.52 |
| 10| 6269 | 13.23 | 4.30 | 0.56 |
| 50| 10108 | 34.70 | 11.02 | 0.95 |
| 100| 14910 | 62.03 | 19.60 | 1.45 |
| 115| 16355 | 69.68 | 21.96 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2305 | 12.66 | 4.96 | 0.39 |
| 2| 2433 | 13.23 | 5.84 | 0.41 |
| 3| 2567 | 13.52 | 6.61 | 0.42 |
| 5| 2829 | 15.12 | 8.54 | 0.46 |
| 10| 3482 | 17.59 | 12.81 | 0.54 |
| 50| 8725 | 42.81 | 48.70 | 1.23 |
| 75| 11999 | 59.42 | 71.39 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 10.30 | 4.21 | 0.29 |
| 2| 756 | 10.87 | 5.09 | 0.31 |
| 3| 887 | 11.40 | 5.96 | 0.32 |
| 5| 1148 | 12.58 | 7.73 | 0.36 |
| 10| 1804 | 15.25 | 12.09 | 0.44 |
| 50| 7044 | 39.19 | 47.57 | 1.12 |
| 75| 10321 | 54.21 | 69.77 | 1.55 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 18.93 | 12.53 | 0.42 |
| 2| 817 | 20.15 | 13.60 | 0.45 |
| 3| 911 | 22.45 | 15.52 | 0.48 |
| 5| 1210 | 24.03 | 16.87 | 0.52 |
| 10| 1866 | 31.73 | 22.65 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 713 | 22.65 | 15.76 | 0.48 |
| 2| 840 | 24.02 | 16.88 | 0.50 |
| 3| 975 | 25.49 | 18.02 | 0.53 |
| 5| 1233 | 28.48 | 20.32 | 0.58 |
| 10| 1892 | 36.89 | 26.32 | 0.72 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5434 | 14.40 | 5.63 | 0.55 |
| 10 | 1 | 57 | 5467 | 16.20 | 6.39 | 0.57 |
| 10 | 5 | 285 | 5603 | 23.41 | 9.44 | 0.65 |
| 10 | 10 | 568 | 5771 | 32.42 | 13.26 | 0.76 |
| 10 | 20 | 1139 | 6112 | 50.61 | 20.92 | 0.98 |
| 10 | 30 | 1709 | 6454 | 68.82 | 28.59 | 1.20 |
| 10 | 30 | 1710 | 6456 | 68.82 | 28.59 | 1.20 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 979 | 32.72 | 63.62 | 0.92 |
| 25 | 1367 | 1452 | 69.03 | 99.36 | 1.49 |
| 30 | 1367 | 1457 | 69.03 | 99.36 | 1.49 |
| 40 | 1366 | 1452 | 69.03 | 99.36 | 1.49 |
| 50 | 1366 | 1456 | 69.03 | 99.36 | 1.49 |
| 100 | 1365 | 1455 | 69.03 | 99.36 | 1.49 |
| 150 | 1362 | 1452 | 69.03 | 99.36 | 1.49 |
| 200 | 1363 | 1453 | 69.03 | 99.36 | 1.49 |
| 200 | 1366 | 1456 | 69.03 | 99.36 | 1.49 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1090 | 1571 | 39.81 | 66.14 | 1.02 |
| 25 | 2068 | 2278 | 77.88 | 99.26 | 1.60 |
| 30 | 2178 | 2397 | 77.90 | 99.28 | 1.60 |
| 40 | 2684 | 2922 | 77.90 | 99.43 | 1.63 |
| 50 | 2354 | 2582 | 77.90 | 99.33 | 1.61 |
| 100 | 2420 | 2651 | 77.88 | 99.37 | 1.61 |
| 150 | 2508 | 2743 | 77.88 | 99.37 | 1.62 |
| 200 | 2486 | 2720 | 77.90 | 99.38 | 1.62 |
| 200 | 2222 | 2444 | 77.90 | 99.28 | 1.60 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 109 | 5440 | 22.05 | 43.33 | 0.88 |
| 5 | 470 | 5718 | 35.65 | 54.85 | 1.09 |
| 10 | 990 | 6133 | 53.85 | 69.63 | 1.36 |
| 10 | 1210 | 6353 | 53.86 | 69.70 | 1.37 |

