---
title: August 2023
slug: 2023-08
authors: [pgrange, v0d1ch, ffakenz, ch1bo]
tags: [monthly]
---

This report summarizes the work on Hydra since July 2023. It serves as
preparation for the monthly review meeting (see [slides][slides] and
[recording][recording]), where the team updates project stakeholders on recent
developments to gather their feedback on proposed plans.

## Roadmap

This month, the team released version 0.12.0, and the project
[roadmap](https://github.com/orgs/input-output-hk/projects/21/views/7) has been
slightly updated to focus 0.13.0 on network resiliency and bump incremental
commit and decommit in priority:

![The roadmap with features and ideas](./img/2023-08-roadmap.jpg) <small><center>The latest roadmap with features and ideas</center></small>

#### Release 0.12.0

- Support cardano-node 8.1.2
  - Updated client and Plutus versions
- Layer 2 protocol changes
  - Authenticated messages
  - Removed redundancy
- Event-sourced persistence
- New API endpoints
- Removal of _internal commit_ endpoint
- Improved off-chain transaction processing performance
- Security fixes

- See [full release notes](https://github.com/input-output-hk/hydra/releases/tag/0.12.0) and a list of [delivered features](https://github.com/input-output-hk/hydra/milestone/12?closed=1)

## Development

[Issues and pull requests closed since the last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-07-28..2023-08-29)

This month, the team worked on the following:

#### Update the tutorial and include Mithril [#997](https://github.com/input-output-hk/hydra/issues/997)

To prepare for the Hydra master class at RareEvo, the team has taken the
originally written tutorial for Hydra version 0.8.0 and updated it to work with the latest
versions.

The challenge was to write content that is both easily comprehensible and functional across a wide
range of user platforms, including operating system and processor architectures.

Besides writing the tutorial, it is essential to ensure that it is kept
up-to-date (eg, using continuous integration).

#### Support cardano-node 8.1.2 [#1007](https://github.com/input-output-hk/hydra/issues/1007)

To be able to use the latest Mithril snapshots to bootstrap a
`cardano-node` for the `hydra-node`, we needed to make some updates.

The `hydra-node` uses the `cardano-api` to establish a connection with the node using the node-to-client protocols. The format there has slightly changed (although versioned), necessitating an update to the version of `cardano-api` used within the `hydra-node`.

The way Haskell dependencies are managed required an adjustment of the versions for both `cardano-ledger` and `plutus`. These versions are used to construct our off-chain ledger and on-chain Hydra Head protocol scripts, respectively.

As a result, this process has proven to be more intricate than it might initially sound. However, it has ultimately resulted in enhancements to the efficiency of our on-chain scripts.

TODO: how much?

#### Event-sourced persistence [#913](https://github.com/input-output-hk/hydra/issues/913)

We want the hydra-node to be efficient in processing events to yield high
throughput on processing transactions off-chain.

Work done as part of [#186](https://github.com/input-output-hk/hydra/issues/186)
has demonstrated that the primary bottleneck to faster transaction processing
inside the node was the state persistence logic, which simply overwrites the
full state with whatever new state has been produced.

For that reason, we changed the persistent state to a sequence of events
according to [ADR24](/adr/24). Persistence is now done incrementally by saving
only the `StateChanged` deltas.

As a consequence, the first spike confirmed the following performance
improvements: master ~300ms → spike ~6ms.

TODO: what are the numbers on master before/after?

Finally, this also opens up interesting possibilities for state observation in
clients.

#### New API endpoints

This release also includes several additions to the Hydra API. We added the
[/cardano-transaction
endpoint](https://github.com/input-output-hk/hydra/pull/1001) to submit a
transaction to the layer 1 network. This feature improves developer experience as
Hydra clients do not need direct chain access (eg, connect to `cardano-node`)
to be able to submit transactions.

The other new [/protocol-parameters
endpoint](https://github.com/input-output-hk/hydra/pull/989) serves the
currently configured protocol parameters used in `hydra-node`. This provides
more flexibility when creating transactions for the head on the client side and
avoids configuration or hard-coded values.

On top of this, we also included the hydra-node
[version](https://github.com/input-output-hk/hydra/pull/985) inside of the
`Greetings` message. This is very useful for debugging purposes and detecting
possible version mismatches.

#### Removal of 'internal commit' endpoint [#1018](https://github.com/input-output-hk/hydra/pull/1018)

In the previous release, we made an announcement regarding the deprecation of committing to the head through the websocket command. Subsequently, we have taken steps to eliminate this client command, which in turn resulted in the removal of the _fuel_ markers that were previously used to mark the UTXO of the internal Hydra wallet meant for use in the Head.

This simplifies the setup needed to run the Head protocol and improves security
since users can directly commit funds from their wallets without sending them to
the Head operator beforehand.

## Community

#### Hydra master class at RareEvo

We were happy to run a Hydra master class session at RareEvo 2023. The session
attracted 30+ attendees for the introductory parts including a presentation on
Hydra and Mithril. About 10-15 participants remained for the practical part and
following discussion.

We also streamed the event live on Discord, but were not able to interact much
there. The responses were positive though and we should be doing more things in
the public on this #hydra-live channel.

Several participants managed to use Mithril and synchronize a pre-production cardano-node; two teams effectively initiated Hydra heads, conducted fund transactions within them, and subsequently closed them. The major challenges, as expected, concerned installation and networking. In the future, we intend to ensure the availability of prebuilt binaries catering to a wider range of platforms.

#### Catalyst Fund10

The team screened all the proposals mentioning Hydra and
[Mithril](https://mithril.network/doc/). We submitted 11 community reviews and
noticed, in particular, the following proposals:

- [Sundae Labs Hydra Ledger-only Mode](https://cardano.ideascale.com/c/idea/102138)
- [Sundae Labs Hydra Transaction Stream Plugin](https://cardano.ideascale.com/c/idea/102200)
- [Hydra as a B2B layer for DeFi - a white paper and an MVP](https://cardano.ideascale.com/c/idea/101626)
- [Decentralized Demeter.run - Federated Frontend Hosting - New revenue stream for SPOs](https://cardano.ideascale.com/c/idea/104411)
- [Mithril - Open-source contributor](https://cardano.ideascale.com/c/idea/105113)

## Conclusion

The monthly review meeting for August 2023 was held on 2023-08-23 via Google
Meet with these [slides][slides] and the [recording][recording], 'broadcasting live from warm and sunny Colorado'!

It has been an interesting and unusual month. Some of the team had been in
Longmont, CO to prepare for the RareEvo event and we used the chance to have the
meeting in a hybrid setting with some IO stakeholders attending live on-site and
about 20 community members online.

This time, the demo was about the updated tutorial and demonstrating the full
setup of the cardano-node, opening a Hydra head on the pre-production network, and submitting
transactions off-chain in 15 minutes!

The feedback we received included inquiries about the timing, method, and extent of the audit for the Head protocol. While we will have an internal audit, which is already
helping us improve the protocol, there are no plans for a significant external audit with funding. We also had the chance to look into and learn about some Catalyst Fund10
proposals involving Hydra. Hopefully, some or all of them get funded and we are
looking forward to testing the Hydrazoa concept, implementing the ledger-mode operation, enabling federated Heads, and achieving other objectives.

At the RareEvo event, we had the chance to meet and communicate with various people
from the community. This ranges from known Hydra collaborators to tech-savvy
stake pool operators, to representatives of successful applications running on
Cardano for scaling purposes like [book.io](https://book.io/).

Also with a new full-time contributor on board, we are keen to add more
functionality while the first applications prepare to utilize Hydra in production
on mainnet.

[slides]: https://docs.google.com/presentation/d/1MrCeUsYb3FQk7aCwMZdQs8mc5BfLOIjkK9gcWzgDdDc/edit#slide=id.g1f87a7454a5_0_1392
[recording]: https://drive.google.com/file/d/14pDsf0hDyh9HK8sCSMmkmT8gY8YxgOQ8/view
