---
title: July 2023
slug: 2023-07
authors: [ch1bo]
tags: [monthly]
---

This report summarizes the work on Hydra since June 2023. It serves as
preparation for the monthly review meeting (see [slides][slides] and
[recording][recording]), where the team updates project stakeholders on recent
developments to gather their feedback on proposed plans.

## Roadmap

This month the team released version 0.11.0 which shifts the way Hydra clients
need to interact with the hydra-node when commiting funds. The project
[roadmap](https://github.com/orgs/input-output-hk/projects/21) was also updated
to reflect latest ideas from an even higher level project plan that is updated
quarterly:

TODO create snapshot
![The roadmap without idea items](./img/2023-07-roadmap-ex-ideas.png) <small><center>The roadmap without idea items</center></small>

#### Release 0.11.0

- Completes the L2 ledger isomorphism with timed transaction support

- Adds commit from external wallets API, which allows to

  - Commit multiple UTxO
  - Commit from scripts
  - Commit directly from any key

- Deprecated: Internal commits via the websocket API

- [Full release notes](https://github.com/input-output-hk/hydra/releases/tag/0.11.0) and a list of [delivered features](https://github.com/input-output-hk/hydra/milestone/11?closed=1)

#### Notable updates

TODO summarize differences

TODO write about high-level project plan (starmap)?

TODO create snapshot
![The latest roadmap with features and ideas](./img/2023-07-roadmap.png) <small><center>The latest roadmap with featuresand ideas</center></small>

## Development

[Issues and pull requests closed since last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-05-24..2023-06-22)

This month, the team worked on the following:

#### Authenticate network messages [#727](https://github.com/input-output-hk/hydra/issues/727)

TODO

- quickly write about re-using hydra keys and EdDSA message signing
- why? why in the application and not on the transport level?

#### ReqSn only sends Transaction IDs [#728](https://github.com/input-output-hk/hydra/issues/728)

TODO

- Initial idea
- scope expanded (crept?)
- specification changes / alignment

#### Github security advisories

TODO

- Found a first bug, potentially impacting off-chain security of Hydra Head
- Published advisory in security section
- Scored with CVSS, but no CVE issued
- Problems with running CI on private forks to fix security advisory

#### Moving to GHC 9.2.7

TODO

- necessary code changes (important?)
- leads to a breaking change via plutus-tx plugin
- plutus script sizes & compile time improvements (mac support?)

## Community

TODO Master-class preparation for Rare Evo
TODO Catalyst fund10 proposals and our stance to them?

## Conclusion

The monthly review meeting for May was held on 2023-07-26 via Google Meet with
these [slides][slides] and here is the [recording][recording].

TODO

[slides]: https://docs.google.com/presentation/d/1CQYAFztRcqofN6sbowg37QuXQ-DQU4NcDmoMghS36B8
[recording]: https://TODO
