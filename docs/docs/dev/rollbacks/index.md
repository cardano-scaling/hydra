# Handling rollbacks

Rollbacks are fundamental to the operation of the Cardano chain. Any application built on Cardano, including Hydra, must anticipate occasional rollbacks, which are reversals of confirmed transactions due to chain reorganization or other consensus adjustments.

This document provides an overview of rollbacks, their origins, and details how Hydra heads manage them.

## Understanding rollbacks

Rollbacks occur on the Cardano chain, and other decentralized blockchains, due to its _asynchronous_ nature. Each node maintains its own view of the chain's state, updating it through communication with other nodes. This process involves exchanging messages about known blocks, which can lead to new blocks being produced that may be valid or invalid. As a result, the chain's state is _eventually consistent_, with all nodes agreeing on its state after processing a certain number of blocks.

In reality, 'rollbacks' are a misnomer; it's more accurate to refer to these events as 'forks'. Let's delve into what this means from the perspective of three nodes running a Hydra head. The following diagram illustrates each node's view of the layer 1 chain.

![](rollbacks-1.jpg)

The _immutable part_ is guaranteed to be identical on all nodes, extending `k` blocks in the past from the current _tip_ (on the mainnet, `k` is 2160). Here's an example scenario: node 2 receives a new block identical to node 1's view, but node 3 receives a different one. Eventually, because node 3's chain is shorter than the others, it will be superseded by a longer chain, resulting in a rollback.

The impact on the node's _direct chain_ observer is detailed in the following diagram:

![](rollbacks-2.jpg)

When new blocks become available, the `ChainSync` client receives a `RollForward` message for each new block. In the event of a fork, it first receives a `RollBackward` message indicating a _point_ that identifies the slot and block hash where the rollback occurred (represented as a single number in the figure). After this rollback point, the client resumes receiving new blocks through `RollForward` messages.

## How do rollbacks impact the Hydra node?

Rollbacks pose challenges because when a transaction is observed on-chain, it can alter the state of the head in several stages: _initializing_ it, collecting _commits_, opening the head via the `CollectCom` transaction, and ultimately _closing_ it and _fanning out_ the head's final UTXO.

The following diagram illustrates the issue where a rollback can lead to potentially conflicting `Commit` transactions:

![](rollbacks-3.jpg)

If the head does not properly handle the rollback, it risks becoming inconsistent with other nodes participating in the head. Therefore, any rollback observed at the `Direct` chain component level must be promptly communicated to the `HeadLogic`. This ensures that the `HeadLogic` can reset its state to maintain consistency with the changes on layer 1.

The consequences of a rollback on the head's state vary depending on when the rollback occurs:

1. If the rollback occurs before or after the head is opened – for example, before the `CollectCom` transaction or after the `Close` – the resolution is relatively straightforward: the head's state can be reset to the point it was at before the rolled-back transaction was observed.

2. If the rollback occurs while the head is open – for instance, if the `CollectCom` transaction is rolled back – it poses greater challenges. At this point, the node has already begun exchanging messages with its peers, and its state no longer depends solely on the blockchain.

## How do we handle them?

:::warning

🛠 Hydra currently handles rollbacks gracefully in simpler cases, such as scenario 1 above. However, in cases where a `CollectCom` transaction rollback occurs, which can easily lead to a head becoming stale due to desynchronization among nodes (where one node resets its state before the rollback and loses track of subsequent events during the head's open phase), the head will need to be closed.
:::

Rollback handling has been partially deactivated in Hydra per [ADR-23](https://github.com/cardano-scaling/hydra/blob/master/docs/adr/2023-04-26_023-single-state.md). This section will be updated with a more comprehensive and refined rollback handling approach with issue [#185](https://github.com/cardano-scaling/hydra/issues/185).
