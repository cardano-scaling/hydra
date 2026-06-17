--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-17 16:05:51.877251658 UTC |
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
| 1| 5351 | 8.87 | 2.91 | 0.48 |
| 2| 5447 | 9.16 | 3.00 | 0.49 |
| 3| 5544 | 9.66 | 3.16 | 0.50 |
| 5| 5736 | 10.63 | 3.47 | 0.51 |
| 10| 6216 | 13.60 | 4.44 | 0.57 |
| 50| 10058 | 34.97 | 11.12 | 0.96 |
| 100| 14858 | 62.03 | 19.59 | 1.45 |
| 115| 16296 | 69.41 | 21.87 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2278 | 17.76 | 6.40 | 0.44 |
| 2| 2408 | 18.65 | 7.30 | 0.46 |
| 3| 2540 | 20.22 | 8.42 | 0.49 |
| 5| 2804 | 21.43 | 10.03 | 0.52 |
| 10| 3459 | 26.64 | 14.77 | 0.62 |
| 50| 8697 | 65.77 | 51.58 | 1.44 |
| 75| 11977 | 90.27 | 74.57 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 599 | 16.03 | 5.75 | 0.35 |
| 2| 730 | 16.92 | 6.65 | 0.37 |
| 3| 861 | 17.79 | 7.54 | 0.39 |
| 5| 1129 | 19.62 | 9.35 | 0.43 |
| 10| 1779 | 24.05 | 13.83 | 0.52 |
| 50| 7020 | 62.10 | 50.29 | 1.32 |
| 75| 10296 | 85.30 | 72.91 | 1.82 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 592 | 15.47 | 10.37 | 0.38 |
| 2| 723 | 16.41 | 11.28 | 0.40 |
| 3| 854 | 17.32 | 12.19 | 0.42 |
| 5| 1117 | 19.11 | 13.99 | 0.45 |
| 10| 1772 | 23.71 | 18.53 | 0.55 |
| 50| 7010 | 61.74 | 55.08 | 1.35 |
| 75| 10288 | 86.12 | 78.09 | 1.86 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 18.78 | 13.47 | 0.43 |
| 2| 755 | 19.85 | 14.43 | 0.45 |
| 3| 886 | 20.97 | 15.40 | 0.47 |
| 5| 1149 | 23.07 | 17.29 | 0.51 |
| 10| 1803 | 28.45 | 22.07 | 0.62 |
| 50| 7044 | 73.62 | 60.73 | 1.49 |
| 73| 10058 | 99.53 | 82.94 | 1.99 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5530 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5564 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 285 | 5700 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 570 | 5870 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1140 | 6209 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1138 | 6208 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 986 | 34.31 | 65.19 | 0.94 |
| 25 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 30 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 40 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 50 | 1310 | 1425 | 67.64 | 98.26 | 1.47 |
| 100 | 1311 | 1430 | 67.64 | 98.26 | 1.47 |
| 150 | 1306 | 1425 | 67.64 | 98.26 | 1.47 |
| 200 | 1306 | 1425 | 67.64 | 98.26 | 1.47 |
| 200 | 1306 | 1425 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1040 | 1521 | 41.40 | 67.71 | 1.04 |
| 25 | 2079 | 2324 | 75.97 | 97.98 | 1.57 |
| 30 | 2562 | 2830 | 75.99 | 98.13 | 1.60 |
| 40 | 2079 | 2324 | 75.99 | 97.98 | 1.57 |
| 50 | 2247 | 2497 | 75.97 | 98.03 | 1.58 |
| 100 | 2079 | 2325 | 75.99 | 97.98 | 1.57 |
| 150 | 2142 | 2391 | 75.97 | 98.02 | 1.58 |
| 200 | 2583 | 2853 | 75.97 | 98.13 | 1.60 |
| 200 | 2226 | 2479 | 75.99 | 98.03 | 1.58 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 104 | 5409 | 21.54 | 43.18 | 0.87 |
| 5 | 495 | 5715 | 35.02 | 54.66 | 1.08 |
| 10 | 1220 | 6335 | 53.35 | 69.55 | 1.36 |
| 10 | 1080 | 6195 | 53.23 | 69.47 | 1.35 |

