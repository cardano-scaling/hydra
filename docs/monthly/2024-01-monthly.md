---
title: January 2024
slug: 2024-01
authors: [v0d1ch, ffakenz, ch1bo]
tags: [monthly]
---

This report summarizes the work on Hydra since November 2023. It serves as
preparation for the monthly review meeting (see [slides][slides] and
[recording][recording]), where the team updates project stakeholders on recent
developments to gather their feedback on proposed plans.

## Roadmap

This month, several items were restructured on the project
[roadmap](https://github.com/orgs/input-output-hk/projects/21/views/7):

![The roadmap with features and ideas](./img/2024-01-roadmap.jpg) <small><center>The latest roadmap with features and ideas</center></small>

#### Notable updates

* Released version `0.15.0` which delivers offline mod and conway support.

* [Offline mode #1254](https://github.com/input-output-hk/hydra/issues/1254) is
  a new feature contributed by @SundaeLabs and is related to [this Catalyst
  project](https://milestones.projectcatalyst.io/projects/1000179)
  
* Delivered [Conway support
  #1177](https://github.com/input-output-hk/hydra/issues/1177) which will
  prepare `hydra-node` for the upcoming hard-fork into the Conway era.
  
* New feature to indicate preparation of [running latest version cardano-nodes in P2P
#1256](https://github.com/input-output-hk/hydra/issues/1256).
  This is necessary as the non-P2P relay nodes of IOG are to be shut down in January 2024.

* Similarly, new feature to [run smoke tests in Sanchonet
  #1257](https://github.com/input-output-hk/hydra/issues/1257) which will serve
  as another proof point of being able to open/close heads in the new Conway
  era.

* Groomed and started work on [building and deploying a Hydra heads explorer
  #696](https://github.com/input-output-hk/hydra/issues/696), details below.
  
* Renamed "Drop Babbage support #1178" to [Switch L2 ledger to Conway
  #1178](https://github.com/input-output-hk/hydra/issues/1178) to better capture
  it's content.

## Hydra development

[Issues and pull requests closed since the last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-11-30..2024-01-31)

This month, the team worked on the following:

### API changes: dropping JSON tx

TODO: DF

### Hydra Chess
We started building a game on Hydra as dogfooding allows us to find things which are suboptimal or stumble accross a bug that needs fixing. Hydra Chess proved to be no different and we learned in the process of making this dApp.

![](https://ipfs.io/ipfs/bafybeicxcm4yuedetm45kn6xrzqsc4mn2aocmhqtt6wrwxz5lzfry722ra/hydra-chess.png)

As we iron out things we hope to improve the workflow of running `hydra-node` as part of a full peer-to-peer dApp. The goal is that it is easy enough to be run by non-tech savvy users, but also provide an example for people trying to build on Hydra. You can find the source code [here](https://github.com/abailly-iohk/hydra-chess).

### Hydra Explorer
To measure the progress and success of Hydra, we require tools that provide insights into its usage. For this purpose, we have initiated work on [#696](https://github.com/input-output-hk/hydra/issues/696) to enable anyone tracking Hydra heads across the whole life-cycle and observe the growth of the Hydra ecosystem.

In this initial phase, we have implemented a basic backend service [#1235](https://github.com/input-output-hk/hydra/pull/1235) that can track all heads on-chain within a devnet network. This service establishes a baseline and utilizes the `hydra-chain-observer` package, exposing a REST API for querying and retrieving information about all observed heads and their current states.

## Community update

TapTools [published a
summary](https://medium.com/tap-in-with-taptools/input-output-releases-hydra-update-97b6139d1c59)
of progress on the project over the last 2 months, which serves as a great
addition to our updates above - go read it!

Besides the contributed offline mode mentioned above, Sundealabs is also
implementing a streaming API as designed in
[ADR29](https://hydra.family/head-protocol/adr/29/) in course of [this Catalyst
project](https://milestones.projectcatalyst.io/projects/1000180).

Also [funded through
Catalyst](https://milestones.projectcatalyst.io/projects/1000092) is a
continuation of the [Hydra
auction](https://github.com/mlabs-haskell/hydra-auction) project by Ikigai and
MLabs. We have not yet re-engaged in a regular exchange, but we expect to
support them with features as needed as they kick of this work.

As we also pointed out in the review session, some of us have been involved in
organizing the first [Cardano Buidler Fest](https://buidl.2024.cardano.org/) -
happening April 23-24 in Toulouse, France. This event will be a prime chance to
connect with the people building (on) Cardano and inspire new ways of
collaboration. At the time of publishing, tickets are already sold out, but we
hope to see many Cardano builders there!

## Conclusion

The monthly review meeting for January 2024 was held on 2024-01-19 via Google
Meet with these [slides][slides] and the [recording][recording].

TODO

* Zk demonstration
* Charles' feedback
* Feedback from Tudor and his project on catalyst
* ...

[slides]: https://docs.google.com/presentation/d/113okna4iyhgC7ERDLVHxqQkvhqTUSWJUWjXfkpwIpEY
[recording]: https://drive.google.com/file/d/1XnM4RMKSiJNKLs2GBEg32ZHymg-fGBFt
