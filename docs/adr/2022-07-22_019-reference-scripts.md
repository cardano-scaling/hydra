---
slug: 19
title: | 
  19. Use of reference scripts
authors: []
tags: [Proposed]
---

## Status

Proposed

## Context

* In the desire to make Hydra transactions smaller and cheaper (at the time of writing any abort tx was too big), we want to use the **reference script** and **reference input** features of the upcoming `Babbage` ledger era. See the [babbage ledger spec](https://hydra.iohk.io/build/16861604/download/1/babbage-changes.pdf), [CIP-31](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0031) and [CIP-33](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) for details.

* With these features we do not need to (re-)include scripts in each transaction.

* The CIPs do not specify how reference scripts are to be managed and we can see at least two options:
  1. Add them as outputs to the `init` transaction or prior that as part of each Hydra Head instance
  2. Post them out-of-band, separate to individual Head instances

* Ownership of the outputs holding the scripts is to be considered. If these "reference outputs" are spent, they cannot be referred to anymore. This would mean all heads referring to them can be denied of service (DoS).

* Each head will need to refer to the correct version of the hydra scripts. That is, consistent with the script hashes known to the `hydra-node`.
  + This is also related to the problem of managing script versions & updates.
  + Right now, the `hydra-node` is compiled against `hydra-plutus` to access compiled script content and hashes.

* The general trade-off is: instead of paying ADA fees for scripts adding to the transaction size in _each_ transaction, ADA deposits will need to be put down to have scripts be part of the UTxO set in the ledger _once_.

## Decision

* Publish outputs holding Hydra scripts out-of-band (option 2), because
  + All scripts would not fit into the `init` transaction directly, we would need to post multiple.
  + Costs (deposits) would need to be payed for each head instance.

* The scripts are stored at outputs addressed to some **unspendable** `v_publish` validator.
  + This is to avoid DoS risk and unnecessariy centralization
  + We have considered "garbage collection" by allowing spending these outputs into re-publishing new versions of the script.
    - This would make things even more complicated and we decided to not bother about "littering the chain" right now.

* We will publish scripts on release of the `hydra-node`, or more specifically of the `hydra-plutus` package.

## Consequences

* We need a process and/or tool to publish `hydra-plutus` scripts and need to pay the deposits.
  + Any other party could to the same, this does not lead to centralization.

* The `hydra-node` would be need to know the `TxIn`s of the "right" published scripts.
  + In the simplest case we would just make this configurable and provide configurations for the various networks after publishing scripts.

* If we combine the `v_publish` validator with a "tag", this allows nodes to "discover" scripts of a known version 
  + For example, we could define `HydraHeadV1`, `HydraInitialV1` and `HydraCommitV1` as such tags
  + We could parameterize the validator by the tag, yielding unique addresses per tag.
  + Alternatively, the "tag" could be stored in a canonical form as datum on the script outputs. 
  + In any case, this allows for some checking consistency or easier configuration (not needing to enumerate which `TxIn` is which script)

* By also knowing the script hashes the `hydra-node` can verify the integrity of "found" reference scripts
  + This would be possible right now, as they are compiled into the node
  + Might be undesirable later for easier system configuration

* By making `v_publish` unspendable, we "litter" the chain. However, any garbage collection scheme would mean potential to DoS again.

* Extended diagram for the [abort](img/on-chain-abort-reference-scripts.jpg) on-chain life-cycles of a Hydra Head to include reference scripts.
