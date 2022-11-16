---
sidebar_label: 'Delegated Head'
sidebar_position: 4
---

# Delegated Head Network

This document details the behaviour of so-called _Delegated Hydra Head Network_.

## Summary

A _Delegated Hydra Head Network_ comprises the following type of actors:

* _Operator_ nodes with low expected downtime, probably operated by some company or organisation with enough resources to host this service,
* _Client_ nodes, either DApp instances, or mobile/personal wallets, which might not be always online and possibly can come and go.

<p align="center">
  <img
    src={require('./custodial-head.png').default}
    alt="Delegated Hydra Head"
    height={400}
  />
</p>

Client nodes want to be able to interact with each other efficiently, at a low cost, using standard Cardano (L1) transactions. They are willing to trust *at least one* _Operator_ to run a full Hydra and Cardano node on their behalf, in effect trading some trust for efficiency. They interact with other _clients_ in a single head using the Hydra [API](https://hydra.family/head-protocol/api-reference) and retain ownership of signing keys for transactions submitted to the Head.

Client nodes, may come and go offline without hampering progress of the Hydra Head. The safety of their funds rely on having at least one honest _Operator_ node with whom they can interact.

Operator nodes hold the _Hydra keys_ used by the protocol to sign snapshots, and the _Cardano keys_ used to progress the Head State Machine on L1. Each of them can sport 100s of client connections through (possibly short lived) _WebSocket_ connections.

## Use Cases

This deployment model has the undesirable property of requiring trust from clients to operators and custodianship of funds committed to the Head. In effect, it's a simple way to create a _Side-chain-a-la-carte_ with the _Operators_ being responsible for its safety and liveness.

This deployment model could be interesting in scenarios where:

1. The Hydra nodes can be fully trusted, either because of their identity, reputation, or lack of personal interest in the purpose of the Head,
2. There's a need to scale the involved parties to the 100s or 1000s, something which is not possible on a normal head.
