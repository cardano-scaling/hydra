---
sidebar_label: "Benchmarks"
sidebar_position: 1
---

# Benchmarks & Limitations

This section provides up-to-date data about the known limitations Hydra Head on-chain protocol. Cardano transactions (and blocks) have limits on the transaction size, execution cost, number of inputs and outputs which are dependent on the network parameters and which impact the capabilities of the Head protocol: How many parties can take part in a Head, how many UTxO can be committed to the Head, how many can be fan-out... As the on-chain scripts and transactions mature and are optimised, and the underlying Cardano chain evolves with increased parameters and more efficient scripts execution, those limits will change.

The data provided in those pages is _generated_ by Hydra's [Continuous Integration](https://github.com/input-output-hk/hydra/actions/workflows/ci-nix.yaml) process and thus guaranteed to reflect the current state of the code.

```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useDocsSidebar} from '@docusaurus/theme-common/internal';

<DocCardList items={useDocsSidebar().items.filter(({ docId }) => docId != "index").map(item => {if (item.label == "tests") item.label = "Test Results" ; return item})}/>
```
