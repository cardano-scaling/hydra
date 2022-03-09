---
sidebar_position: 0
---

# Overview

Hydra is the layer-two scalability solution for Cardano, which aims to increase the speed of transactions (low latency, high throughput) and minimize transaction cost. 

The [Hydra Head](https://eprint.iacr.org/2020/299.pdf) protocol is the first protocol of the Hydra family and embodies the foundation for more advanced deployment scenarios relying on isomorphic, multi-party state-channels. There exist various flavors and extensions of the Hydra Head protocol, but let's start by a look at a full life cycle of a basic Hydra Head and how it allows for isomorphic state transfer between layer 1 and layer 2.

![](./hydra-head-lifecycle.svg)

A Hydra Head is formed by a group of online and responsive participants. Participants **init**ialize a Head by announcing several parameters on-chain including the participants list. Then each of the participants **commit**s unspent transaction outputs (UTXO) from the Cardano main-chain to it, before all the UTXO are **collect**ed and made available in a Hydra Head as initial state (`U0`). At any moment before collecting, participants may also **abort** the process and recover their fund. 

While open, they can use the Hydra Head via a hydra-node to submit transactions over the Head network. Transactions have the same format and properties as on the main-chain: they are said _isomorphic_. When UTXO entries are spent and new UTXO entries are created in a Hydra Head, all participants are required to acknowledge and agree on the new state in so-called snapshots (`U1..Un`).

Any participant can **close** the Head using an agreed state, when for example they wish to cash-out on the mainnet or if another party misbehaves or stalls the Head evolution. There is a mechanism to **contest** the final state on the main chain. Ultimately, a **fanout** transaction distributes the final agreed state and makes available on the layer 1, what was only virtually existing in the head.
