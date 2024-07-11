---
sidebar_label: 'Delegated Head'
sidebar_position: 4
---

# Delegated Head network

This document outlines the behavior and structure of the so-called _Delegated Hydra Head Network_.

## Summary

A _Delegated Hydra Head Network_ involves two primary types of actors:

* **Operator nodes**. These nodes are expected to have low downtime and are likely operated by companies or organizations with sufficient resources to host such services reliably.
* **Client nodes**. These can be DApp instances or mobile/personal wallets that might not always be online and can intermittently connect or disconnect.


<p align="center">
  <img
    src={require('./delegated-head.png').default}
    alt="Delegated Hydra Head"
    height={400}
  />
</p>

Client nodes aim for efficient and low-cost interaction using standard Cardano (L1) transactions. They are prepared to place trust in *at least one* _Operator_ to manage a full Hydra and Cardano node on their behalf, essentially trading some degree of trust for increased efficiency. Clients interact with other clients within a single head using the Hydra [API](/api-reference) and maintain control over their signing keys for transactions submitted to the Head.

Client nodes can go offline without affecting the progress of the Hydra Head. The security of their funds depends on the integrity of at least one honest _Operator_ node. Importantly, clients do not relinquish control over the keys used for spending funds within the Head.

Operator nodes maintain the _Hydra keys_ necessary for signing snapshots and the _Cardano keys_ required for advancing the Head State Machine on L1. Each operator can manage connections with hundreds of clients through potentially short-lived _WebSocket_ connections.

## Use cases

This deployment model, while requiring some level of trust from clients towards operators and involving the custodianship of funds committed to the Head, offers a straightforward method to create a _Side-chain-a-la-carte_. In this model, Operators are accountable for the safety and liveness of the network. These alternative chains can be created and deployed ad-hoc without the complexities associated with ‘classic’ side-chains.

Potential scenarios for this deployment model include:

1. Environments where the Hydra nodes are fully trusted, either due to their established reputation, identity, or a disinterest in the specific purposes of the Head.
2. Situations that require scaling to hundreds or thousands of parties, which is unfeasible with a standard Hydra Head.