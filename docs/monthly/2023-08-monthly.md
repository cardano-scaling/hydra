---
title: August 2023
slug: 2023-08
authors: [ch1bo, abailly, v0d1ch, ffakenz, locallycompact, pgrange]
tags: [monthly]
---

This report summarizes the work on Hydra since August 2023. It serves as
preparation for the monthly review meeting (see [slides][slides] and
[recording][recording]), where the team updates project stakeholders on recent
developments to gather their feedback on proposed plans.

### Starmap update

Every couple of months, the team reflects on the themes identified for 2023 (as
presented in [January](/monthly/2023-01#themes-for-2023)), and creates a high
level list of possible deliverables,
eg using [impact maps](./img/2022-11-impact.png). This time, the team has come up
with the following items:

<!-- TODO: add missing img: ./img/2023-08-starmap.png -->
![](./img/2023-08-starmap.png)

- Sustainable open-source :green_heart:

  - **TODO**: TODO

- Mainnet mature application :purple_heart:

  - **TODO**: TODO

- Drive adoption :blue_heart:

  - **TODO**: TODO

Work packages related to these items appear on the technical
roadmap (especially [in this view](https://github.com/orgs/input-output-hk/projects/21/views/26)) and in our community activities.

## Roadmap

This month, the team released version 0.12.0, which TODO. The project
[roadmap](https://github.com/orgs/input-output-hk/projects/21) was also updated
to reflect the latest developments and ideas from the starmap:

<!-- TODO: add missing img: ./img/2023-08-roadmap-ex-ideas.png -->
![The roadmap without idea items](./img/2023-08-roadmap-ex-ideas.png) <small><center>The roadmap without idea items</center></small>

#### Release 0.12.0

- TODO

- Support cardano-node 8.1.2 [#TODO](https://github.com/input-output-hk/hydra/issues/TODO)

- Authenticated messages [#TODO](https://github.com/input-output-hk/hydra/issues/TODO)

- See [full release notes](https://github.com/input-output-hk/hydra/releases/tag/0.12.0) and a list of [delivered features](https://github.com/input-output-hk/hydra/milestone/12?closed=1)

#### Notable updates

- TODO

- Support cardano-node 8.1.2 [#TODO](https://github.com/input-output-hk/hydra/issues/TODO)
  + Updated client and plutus versions

- Layer 2 protocol changes
  + Removed redundancy

- Event-sourced persistence [#TODO](https://github.com/input-output-hk/hydra/issues/TODO)
  + Improved off-chain tx processing performance

- Update tutorial and include Mithril #997
  + Make tutorial work with latest version of hydra-node
  + Support a wide range of platforms
  + Ensure it’s kept up-to-date (using continuous integration)
  + Preparation for Hydra master-class at RareEvo


<!-- TODO: add missing img: ./img/2023-08-roadmap.png -->
![The latest roadmap with features and ideas](./img/2023-08-roadmap.png) <small><center>The latest roadmap with features and ideas</center></small>

## Development

[Issues and pull requests closed since last report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-07-28..2023-08-24)

This month, the team worked on the following:

#### Authenticated messages [#TODO](https://github.com/input-output-hk/hydra/issues/TODO)
TODO

#### Event-sourced persistence [#913](https://github.com/input-output-hk/hydra/issues/TODO)
First spike confirmed performance improvements  (master ~300ms → spike ~6ms)
Also opens up interesting possibilities for state observation in clients
Needed to refactor in between
Currently carefully porting protocol logic to be event sourced


#### New API endpoints [#TODO](https://github.com/input-output-hk/hydra/issues/TODO)
- submitTx
  + Submit L1 transactions through hydra-node #966
  + API improvement to not need a tx submission server / cardano-node on client side

- getProtocolParameters
  + Ability to read protocol parameters via API #735
  + Also small story to increase developer experience
  + Should cover everything but current time + conversion (projection possible)

TODO

#### Removal of “internal commit” endpoint [#TODO](https://github.com/input-output-hk/hydra/issues/TODO)
Remove commit from internal wallet
- No “fuel marking” anymore
- Simplifies setup

TODO

#### Security fixes [#TODO](https://github.com/input-output-hk/hydra/issues/TODO)
TODO @abailly to write this section

## Community

### Hydra master-class
- Saturday, August 26
9:30-12:30 MDT / 15:30-18:30 UTC

- About 40 registered for on-site event

- Planning to stream it on #hydra-live + support people on #ask-hydra -> on the IOG Discord: https://discord.gg/Qq5vNTg9PT

- Preparation still on-going and maybe some network connectivity challenges..

TODO

### Catalyst Fund10
TODO

## Conclusion

The monthly review meeting for Aug was held on 2023-08-23 via Google Meet with
these [slides][slides] and here is the [recording][recording].

TODO

[slides]: https://docs.google.com/presentation/d/1MrCeUsYb3FQk7aCwMZdQs8mc5BfLOIjkK9gcWzgDdDc/edit#slide=id.g1f87a7454a5_0_1392
[recording]: https://drive.google.com/file/d/14pDsf0hDyh9HK8sCSMmkmT8gY8YxgOQ8/view
