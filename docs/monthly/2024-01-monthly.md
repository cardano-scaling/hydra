---
title: January 2024
slug: 2024-01
authors: [v0d1ch, ffakenz, locallycompact, ch1bo]
tags: [monthly]
---

This report summarizes the work on Hydra since November 2023. It serves as
preparation for the monthly review meeting (see [slides][slides] and
[recording][recording]), where the team updates project stakeholders on recent
developments to gather their feedback on proposed plans.

## Roadmap

The Hydra team restructured several items on the project [roadmap](https://github.com/orgs/input-output-hk/projects/21/views/7) this month.

![The roadmap with features and ideas](./img/2024-01-roadmap.jpg) <small><center>The latest roadmap with features and ideas</center></small>

### Notable updates

* Released version `0.15.0`, delivering offline mode and Conway support.

* @SundaeLabs contributed a new feature, [offline mode #1254](https://github.com/input-output-hk/hydra/issues/1254), which is related to [this Catalyst project](https://milestones.projectcatalyst.io/projects/1000179).

* Delivered [Conway support #1177](https://github.com/input-output-hk/hydra/issues/1177) preparing `hydra-node` for the upcoming Conway hard fork.

* Implemented a new feature, indicating preparation for [running the latest version of Cardano nodes in P2P #1256](https://github.com/input-output-hk/hydra/issues/1256). This is necessary as IOG's non-P2P relay nodes are to be shut down in January 2024.

* Worked on implementing the new feature, [running smoke tests in SanchoNet #1257](https://github.com/input-output-hk/hydra/issues/1257), which will provide additional evidence of the capability to open and close heads in the Conway era.

* Groomed and started work on [building and deploying a Hydra heads explorer #696](https://github.com/input-output-hk/hydra/issues/696), see details below.

* Renamed 'Drop Babbage support #1178' to [Switch layer 2 ledger to Conway #1178](https://github.com/input-output-hk/hydra/issues/1178) to better capture its content.

## Hydra development

[Issues and pull requests closed since the last report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-11-30..2024-01-31).

This month, the team worked on the following:

### API changes: dropping JSON Tx

The Hydra API now utilizes a TextEnvelope containing the upstream CBOR encoding from `cardano-api` for the `Tx` type and removes the JSON representation from the API [#1240](https://github.com/input-output-hk/hydra/pull/1240). This ensures that transaction types have a fixed canonical encoding that will not change between versions. As a result, consumers of the Hydra API will need to deserialize CBOR to inspect transactions.

This representation mirrors the TextEnvelope produced by the Cardano CLI, with the addition of an `txId` field, which is guaranteed to match the transaction ID within the CBOR object.

:::warning Breaking change

We do not plan to go through a deprecation cycle on this breaking change to the API. If you are affected and see a problem updating your Hydra client to use CBOR, please [open an issue](https://github.com/input-output-hk/hydra/issues/new) on GitHub.

:::

### Hydra Chess

The team began developing a game on Hydra, leveraging dogfooding to uncover suboptimal elements or stumble upon bugs requiring resolution. Hydra Chess exemplified this process, offering valuable lessons during DApp development.

![](https://ipfs.io/ipfs/bafybeicxcm4yuedetm45kn6xrzqsc4mn2aocmhqtt6wrwxz5lzfry722ra/hydra-chess.png)

While refining the details, the team aims to improve the workflow of running `hydra-node` within a full peer-to-peer DApp framework. The objective is to ensure user-friendly accessibility while also providing an example for Hydra developers. You can access the source code [here](https://github.com/abailly-iohk/hydra-chess).

### Hydra Explorer

Measuring the progress and success of Hydra requires tools that provide
insights into its usage. For this, the team initiated work on
[#696](https://github.com/input-output-hk/hydra/issues/696), enabling anyone to track Hydra heads throughout their entire life cycle and observe the growth of the Hydra ecosystem. 

In this initial phase, the team has implemented a basic backend service
[#1235](https://github.com/input-output-hk/hydra/pull/1235) that can track all
heads on-chain within a devnet network. This service establishes a baseline and
utilizes the `hydra-chain-observer` package, exposing a REST API for querying
and retrieving information about all observed heads and their current states.

## Community update

TapTools recently published a summary of the project's progress over the last two months, which complements the updates provided above. Check it out [here](https://medium.com/tap-in-with-taptools/input-output-releases-hydra-update-97b6139d1c59)!

Besides the contributed offline mode mentioned above, Sundae Labs is also
implementing a streaming API as designed in
[ADR29](https://hydra.family/head-protocol/adr/29/) in the course of [this Catalyst
project](https://milestones.projectcatalyst.io/projects/1000180).

Also [funded through
Catalyst](https://milestones.projectcatalyst.io/projects/1000092) is a
continuation of the [Hydra
auction](https://github.com/mlabs-haskell/hydra-auction) project by Ikigai and
MLabs. The Hydra team has not yet re-engaged in a regular exchange, but they expect to support the project with features as needed as they kick off this work. 

As also pointed out in the review session, some of the team members have been involved in
organizing the first [Cardano Buidler Fest](https://buidl.2024.cardano.org/) -
happening on April 23-24 in Toulouse, France. This event will be a prime chance to
connect with the people building (on) Cardano and inspire new ways of
collaboration. At the time of publishing, tickets are already sold out, but we
hope to see many Cardano builders there!

## Conclusion

The team held the monthly review meeting for January 2024 on 2024-01-19 via Google Meet, presenting these [slides][slides] and this [recording][recording].

Throughout the month, the teams demonstrated various Hydra and Mithril features. Additionally, Jose and his team from Modulo-Pi showcased a compelling demo on utilizing zero-knowledge cryptography in a Hydra head for a mastermind game.

Besides providing the usual status and roadmap updates on both Hydra and Mithril, several interested community members, including Tudor, joined the discussion to share their use case ideas. Tudor presented his proposal for supply chain tracking of Moldavian wine, which is currently running as a Catalyst proposal in Fund11. The proposal aims to leverage Hydra for CIP-68 NFT updates in a head, requiring wallet/app support. The team also discussed a potential CIP to standardize asset access if held on layer 2 ledgers.

Charles provided feedback on considering Cardano block extensions as a potential core protocol change, enabling various proof systems and new consensus algorithms to support partner chains. The team emphasized collaborative efforts with the community to explore and develop the user space, determining which aspects could evolve into marketplaces and what should be integrated into core protocols. Processes like CIPs play a vital role in standardizing and facilitating technology transfer from the user space to the kernel space.

These discussions will inform the team's roadmapping sessions for the year, focusing not only on building essential features for the Hydra Head protocol but also on experimenting with new approaches to enhance Cardano's efficiency and accessibility.

[slides]: https://docs.google.com/presentation/d/113okna4iyhgC7ERDLVHxqQkvhqTUSWJUWjXfkpwIpEY
[recording]: https://drive.google.com/file/d/1XnM4RMKSiJNKLs2GBEg32ZHymg-fGBFt
