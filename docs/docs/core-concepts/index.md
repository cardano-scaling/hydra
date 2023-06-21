---
sidebar_position: 50
---

# Core Concepts

Hydra is the layer 2 scalability solution for Cardano, which aims to increase transaction speed through low latency and high throughput and minimize transaction cost.

[Hydra Head](https://eprint.iacr.org/2020/299.pdf) is the first protocol of the Hydra family and embodies the foundation for more advanced deployment scenarios relying on isomorphic, multi-party state-channels. For an introduction to the protocol also check out these two blog posts

- [Hydra – Cardano’s solution for ultimate Layer 2 scalability](https://iohk.io/en/blog/posts/2021/09/17/hydra-cardano-s-solution-for-ultimate-scalability/)
- [Implementing Hydra Heads: the first step towards the full Hydra vision ](https://iohk.io/en/blog/posts/2022/02/03/implementing-hydra-heads-the-first-step-towards-the-full-hydra-vision/)

There are different flavors and extensions of the Hydra Head protocol, but let's start by looking at the full lifecycle of a basic Hydra Head, and how it allows for isomorphic state transfer between layer 1 and layer 2.

![](./hydra-head-lifecycle.svg)

A Hydra Head is formed by a group of online and responsive participants. Participants **init**ialize a Head by announcing several parameters on-chain, including the participants list. Then each of the participants **commit**s unspent transaction outputs (UTXOs) from the Cardano main-chain to it, before all the UTXOs are **collect**ed and made available in a Hydra Head as initial state (`U0`). At any moment before collecting, participants can also **abort** the process and recover their funds.

While open, they can use the Hydra Head via a hydra-node to submit transactions over the Head network. Transactions have the same format and properties as on the main-chain: they are _isomorphic_. When UTXO entries are spent, and new UTXO entries are created in a Hydra Head, all participants are required to acknowledge and agree on the new state in so-called snapshots (`U1..Un`). Snapshots are _not_ posted back onto the layer 1, but are only kept around by the participants.

Any participant can **close** the Head using a snapshot, when for example they wish to cash out on the mainnet, or if another party misbehaves or stalls the Head's evolution. There is a mechanism to **contest** the final state on the main chain. Ultimately, a **fanout** transaction distributes the final agreed state and makes available on the layer 1 what only existed virtually in the Head.

```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useDocsSidebar} from '@docusaurus/theme-common/internal';

<DocCardList items={useDocsSidebar().items.filter(({ docId }) => docId != "index")}/>
```
