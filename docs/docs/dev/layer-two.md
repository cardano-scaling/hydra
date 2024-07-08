# Layer 2 solutions

This section provides an overview of various types of layer 2 solutions, along with several examples.

In the blockchain industry, a layer 2 solution refers to any protocol that overlays an existing protocol (layer 1) to provide additional functionality or performance benefits. Essentially, these solutions are built on top of the base protocol to enable other applications. They offer a framework for developing decentralized applications with different trade-offs compared to the underlying layer 1 protocol.

## State channels

The Hydra Head protocol is a layer 2 solution within the family of **state channels**, which evolved from **payment channels**. A payment channel allows two or more parties to exchange funds using an off-chain protocol without committing all transactions to the underlying blockchain. Historically, they were among the first types of layer 2 solutions developed to address the scalability issues of permissionless ledgers, making them the most studied and well-known.

State channels extend the concept of payment channels to support smart contracts over off-chain channels. In this setup, parties can execute complex logic and validate full-blown scripts off-chain, not just transactional payments. The final result is then committed back to the layer 1 blockchain.

#### Examples

- Lightning (Bitcoin)
- Perun (Ethereum, Polkadot, Cosmos)
- Sprites (Ethereum)
- And of course, our favorite: **Hydra: Head** (Cardano).

## Sidechains

Sidechains allow for transferring assets from a layer 1 protocol to a new chain with its own set of consensus rules. Typically, a sidechain provides a simpler or more efficient consensus mechanism that allows for greater scalability or facilitates the implementation of new functionality that is harder to adopt on layer 1. Often, this comes at the price of decentralization or security, as sidechains typically involve only a few actors or committees at their core.

Sidechains are, however, 'proper chains', with blocks produced by validators and usually smart contract capabilities. Therefore, unlike state channels, they provide data availability and offer ways to participate in the validation and observation of the chain. In a state channel, only participants of the channel have a reliable view of what is happening within the channel. Entering a sidechain is usually done by burning or locking assets on layer 1 to receive an equivalent counterpart on the sidechain network.

#### Examples

- Liquid Network (Bitcoin)
- RSK (Bitcoin)
- Polygon (Ethereum)
- Milkomeda (Cardano).

## Rollups

Another major type of layer 2 solution is rollups. Rollups move transaction execution off-chain, keeping a much more compact representation of the execution on layer 1. They are typically driven by a central actor, often called a *sequencer*, which has high availability and computational resource demands off-chain, while regularly leaving verifiable ‘breadcrumbs’ on-chain (the rollups).

In general, rollups come in two popular flavors: *optimistic* rollups and *validity* rollups.

- **Optimistic rollups** involve interactive games for fraud disputes. Transactions are posted on-chain optimistically, and verification is performed afterward by independent validators. In case of disagreement, the dispute is resolved on-chain, and the rollup batch publisher incurs financial consequences.
- **Validity rollups** involve zero-knowledge proofs for validity checks. A succinct proof of execution is calculated off-chain, published alongside the rollup batch commitment, and controlled by on-chain validators, enforcing the rightful execution of the rollup.

#### Examples

- Arbitrum (Ethereum)
- Optimism (Ethereum)
- Hermez (Ethereum)
- ZKSync (Ethereum).
