---
sidebar_position: 2
---

# Layer 2 Solutions

> What does layer 2 mean? What types of layer 2 solutions exist? 

In the blockchain industry, we talk about **layer 2** solution whenever we refer to a solution that overlays an existing protocol (the layer 1) to provide either additional functionality or performance benefits over the underlying protocol. In essence, they are generic purpose solutions built on top of a protocol to enable other applications. They provide a framework for developing decentralized applications with generally different trade-offs than the underlying layer 1 protocol.

## State Channels

The Hydra head protocol is a layer 2 solution which belongs to the family of **state channels**, which is itself a descendant of **payment channels**. A payment channel allows for two or more parties to exchange funds according to a given off-chain protocol without having to commit all transactions to the underlying blockchain. They are historically one of the first kind of layer 2 solution to arise as an answer to scalability issues of permission-less ledgers (and consequently, they're also the most studied and known kind of solutions).

State-channels extend the traditional concept of payment channels to support smart-contracts over off-chain channels. In such a setup, one or more parties are no longer limited to pure transactional payments, but they can execute full-blown scripts validations to handle complex logic, off-chain, only to later commit the result back to the layer 1. 

#### Examples

- Lightning (Bitcoin);
- Perun (Ethereum, Polkadot, Cosmos);
- Sprites (Ethereum);
- And of course, our favorite: **Hydra: Head** (Cardano)!

## Side Chains

**Side chains** allow for transferring assets from a layer 1 protocol to a new chain with its own set of consensus rules. Usually, a side-chain provides either a simpler or more efficient consensus mechanism which allows for more scalability or which ease the implementation of new functionality harder to get adopted on the layer 1. Often, this comes at the price of decentralization or security, since side-chains are typically involving only few actors or committees at their root. 

Side chains are however "proper chains", with blocks produced by validator and usually, smart-contract capabilities. Therefore, unlike state-channels, they provide data-availability and often ways to participate into the validation and observation of the chain (in a state-channel, only participants of the channel really have a reliable view of what is going on in the channel). Entering a side-chain is usually done by burning or locking assets on the layer 1, to receive an equivalent counterpart on the side-chain network. 

#### Examples

- Liquid Network (Bitcoin);
- RSK (Bitcoin);
- Polygon (Ethereum);
- Milkomeda (Cardano).

## Rollups

Another major type of layer 2 solution is **Rollups**. They provide a way for moving transaction execution off-chain only to keep a much more compact representation of the execution on the layer 1. Rollups are typically driven by a central actor offering high availability and high computational capability off-chain while regularly leaving verifiable breadcrumbs on-chain (the rollups).

In general, rollups come in two flavour: **optimistic** or **zero-knowledge**. In the former, rollups are posted on-chain optimistically and verification are done _a posteriori_ by independent validators. In case of disagreement, the dispute is resolved on-chain and the rollup publisher endure financial consequences. In zero-knowledge approaches, a succinct proof of execution is calculated off-chain, published alongside the rollup and controlled by on-chain validators (which thus enforce the rightful execution of the rollup). 

#### Examples

- Arbitrum (Ethereum);
- Optimism (Ethereum);
- Hermez (Ethereum);
- ZKSync (Ethereum).
