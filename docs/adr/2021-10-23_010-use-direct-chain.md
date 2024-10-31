---
slug: 10
title: |
  10. Use direct connection to `cardano-node`
authors: []
tags: [Accepted]
---

## Status

Accepted

## Context

* On-chain validation is a critical part of the Hydra protocol. It requires both the ability to _submit_ transactions to the _layer 1_ chain, advancing the state of a head, and _observing_ those transactions as the [Plutus](https://github.com/input-output-hk/plutus) contracts are validated.
* The [Plutus Application Framework](https://github.com/input-output-hk/plutus-apps) is expected to provide the necessary machinery to allow 'smart contract' applications to interact with the chain, but it's still under active development and not ready for deployment on-chain.
* We want to gather feedback as early as possible and deliver a fully functional Hydra Head node for early adopters to test on a 'real' chain (testnet).
* Our experiment connecting directly to a Cardano node has been conclusive. We can:
  * Connect to a node using local protocols
  * Build and submit Head transactions triggering smart contracts validation
  * Observe transactions using chain-sync protocol.

## Decision

_Therefore_

For the time being, until _Plutus Application Framework_ is released, we will implement on-chain interaction of Hydra nodes _directly_, connecting to a Cardano node through a _local socket_.

## Consequences

* Limit Hydra dependencies to the [Plutus](https://github.com/input-output-hk/plutus) repository
* Remove all PAB-related code as well as off-chain `contract`s code and related dependencies.
