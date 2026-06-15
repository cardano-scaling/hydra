--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-15 15:00:06.155044458 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | fd75e24c9ea915ce8e48d3ff1d0c54ad09cc01191c24416ad7dba4a3 | 11621 | 
| μHead | 83a964e973c065bbe70588f5e089817f92182ae81743e7a54cf3e29e* | 4856 | 
| νDeposit | c78e8c9205721eb3ef4410f3db9c6169fa6db497c24641d29c20529c | 1615 | 
| νCRS | 09db7ee6cf7a4b358dd5c8a2f19d2c048336ffc5a01ef35a47ca7072 | 2736 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5352 | 9.14 | 3.02 | 0.48 |
| 2| 5447 | 9.60 | 3.16 | 0.49 |
| 3| 5544 | 10.06 | 3.30 | 0.50 |
| 5| 5734 | 10.68 | 3.48 | 0.51 |
| 10| 6216 | 13.64 | 4.45 | 0.57 |
| 50| 10058 | 34.52 | 10.96 | 0.95 |
| 100| 14858 | 61.50 | 19.41 | 1.44 |
| 115| 16296 | 69.40 | 21.87 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2279 | 18.33 | 6.59 | 0.45 |
| 2| 2409 | 18.85 | 7.36 | 0.46 |
| 3| 2537 | 20.10 | 8.39 | 0.48 |
| 5| 2803 | 21.57 | 10.07 | 0.52 |
| 10| 3459 | 26.58 | 14.75 | 0.62 |
| 50| 8699 | 65.59 | 51.53 | 1.43 |
| 75| 11973 | 90.38 | 74.61 | 1.95 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 600 | 16.03 | 5.75 | 0.35 |
| 2| 730 | 16.94 | 6.65 | 0.37 |
| 3| 861 | 17.84 | 7.56 | 0.39 |
| 5| 1124 | 19.64 | 9.35 | 0.43 |
| 10| 1780 | 24.09 | 13.84 | 0.52 |
| 50| 7020 | 62.32 | 50.35 | 1.33 |
| 75| 10299 | 86.34 | 73.20 | 1.83 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 593 | 15.49 | 10.37 | 0.38 |
| 2| 723 | 16.41 | 11.28 | 0.40 |
| 5| 1113 | 19.07 | 13.98 | 0.45 |
| 10| 1772 | 23.71 | 18.53 | 0.55 |
| 50| 7013 | 62.36 | 55.25 | 1.36 |
| 75| 10288 | 86.86 | 78.29 | 1.87 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 619 | 18.79 | 13.48 | 0.43 |
| 2| 750 | 19.89 | 14.44 | 0.45 |
| 3| 886 | 20.97 | 15.40 | 0.47 |
| 5| 1148 | 23.14 | 17.31 | 0.51 |
| 10| 1795 | 28.52 | 22.09 | 0.62 |
| 50| 7045 | 74.16 | 60.88 | 1.50 |
| 72| 9927 | 99.35 | 82.24 | 1.98 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5699 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 568 | 5868 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1140 | 6209 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1140 | 6209 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 982 | 34.31 | 65.19 | 0.94 |
| 25 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 30 | 1309 | 1424 | 67.64 | 98.26 | 1.47 |
| 40 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 50 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 100 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 150 | 1307 | 1426 | 67.64 | 98.26 | 1.47 |
| 200 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 200 | 1305 | 1424 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1230 | 1726 | 41.40 | 67.76 | 1.05 |
| 25 | 2289 | 2544 | 75.97 | 98.03 | 1.58 |
| 30 | 2352 | 2606 | 75.99 | 98.08 | 1.59 |
| 40 | 2352 | 2610 | 75.99 | 98.08 | 1.59 |
| 50 | 1995 | 2237 | 75.99 | 97.98 | 1.57 |
| 100 | 1953 | 2193 | 75.99 | 97.98 | 1.57 |
| 150 | 1995 | 2237 | 75.99 | 97.98 | 1.57 |
| 200 | 2352 | 2611 | 75.99 | 98.08 | 1.59 |
| 200 | 2436 | 2699 | 75.99 | 98.08 | 1.59 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 123 | 5427 | 21.54 | 43.19 | 0.87 |
| 5 | 545 | 5765 | 35.02 | 54.68 | 1.08 |
| 10 | 1120 | 6236 | 53.35 | 69.52 | 1.36 |
| 10 | 1000 | 6115 | 53.35 | 69.48 | 1.35 |

