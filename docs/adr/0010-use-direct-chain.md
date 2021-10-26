# 10. Use direct connection to cardano-node

Date: 2021-10-23

## Status

:hammer_and_wrench:

## Context

* On-Chain Validation is a critical part of the Hydra protocol, it requires both the ability to _submit_ transactions to the _Layer 1_ chain advancing the state of a Head, and _observing_ those transactions as the [Plutus](https://github.com/input-output-hk/plutus) contracts are validated
* The [Plutus Application Framework](https://github.com/input-output-hk/plutus-apps) is expected to provide the necessary machinery to allow "Smart Contracts" applications to interact with the chain but it's still under active development and not ready for deployment on-chain
* We want to gather feedback as early as possible and deliver a fully functional Hydra Head node for early adopters to test on a "real" chain (testnet)
* Our experiment connecting directly to a Cardano node has been conclusive. We can:
  * Connect to a node using local protocols,
  * Build and submit Head transactions triggering smart contracts validation, and
  * Observe transactions using chain-sync protocol.

## Decision

_Therefore_

For the time being, until _Plutus Application Framework_ is released, we will implement on-chain interaction of Hydra nodes _directly_, connecting to a Cardano node through a _local socket_.

## Consequences

* Limit Hydra dependencies to [plutus](https://github.com/input-output-hk/plutus) repository
* Remove all PAB-related code as well as off-chain `Contract`s code and related dependencies
