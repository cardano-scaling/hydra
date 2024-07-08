---
sidebar_label: "Benchmarks"
sidebar_position: 1
---

# Benchmarks & Limitations

This section provides up-to-date data regarding the known limitations of the Hydra Head on-chain protocol. Cardano transactions and blocks are subject to constraints on transaction size, execution cost, and the number of inputs and outputs. These constraints are determined by network parameters and significantly influence the capabilities of the Head protocol, such as the maximum number of parties that can participate, the amount of UTXOs that can be committed, and the extent to which these UTXOs can be fanned out. As on-chain scripts and transactions are further optimized, and as the underlying Cardano blockchain evolves with expanded parameters and enhanced script execution efficiency, these limitations are expected to evolve.

The data in this section is _generated_ through Hydra's [Continuous Integration](https://github.com/input-output-hk/hydra/actions/workflows/ci-nix.yaml) process, ensuring that it accurately reflects the current state of the code.


```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useDocsSidebar} from '@docusaurus/theme-common/internal';

<DocCardList items={useDocsSidebar().items.filter(({ docId }) => docId != "index").map(item => {if (item.label == "tests") item.label = "Test Results" ; return item})}/>
```