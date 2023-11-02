---
title: October 2023
slug: 2023-10
authors: [abailly, ch1bo]
tags: [monthly]
---

This report summarizes the work on Hydra since September 2023. It serves as
preparation for the monthly review meeting (see [slides][slides] and
[recording][recording]), where the team updates project stakeholders on recent
developments to gather their feedback on proposed plans.

## Roadmap

This month, the team released version 0.13.0, and the project
[roadmap](https://github.com/orgs/input-output-hk/projects/21/views/7) has been
updated with the next planned version 0.14.0:

![The roadmap with features and ideas](./img/2023-10-roadmap.jpg) <small><center>The latest roadmap with features and ideas</center></small>

#### Release 0.13.0

- Security fixes to Hydra on-chain scripts; all of which are only exploitable by other Head participants (not any attacker)

- Add support for (externally) committing inline datums

- Improved stability by querying the `cardano-node` at the tip

- Improved state persistence

- See [full release notes](https://github.com/input-output-hk/hydra/releases/tag/0.13.0) and a list of [delivered features](https://github.com/input-output-hk/hydra/milestone/13?closed=1)

#### Notable updates

- Moved [network resilience
  #188](https://github.com/input-output-hk/hydra/issues/188) out of 0.13.0 to
  allow for a quick release of the security fixes. This feature has been split
  in two, is completed now, and will soon be released in 0.14.0.

- New user submitted feature idea about supporting extended keys [#1141](https://github.com/input-output-hk/hydra/issues/1141) - at first this was
  urgent, but now not anymore; needs clarification.
  
- Not really part of hydra version 0.14.0, but also completed is the [hydra
  support in kupo #1078](https://github.com/input-output-hk/hydra/issues/1078)
  which can already be used with kupo version
  [2.7.0](https://github.com/CardanoSolutions/kupo/releases/tag/v2.7)
  
- New [Hydra Poll dApp](https://github.com/input-output-hk/hydra/issues/1110)
  feature, which is an example to be demonstrated at the Cardano summit 2023.
  
- Realized [stateless chain observation
  #1096](https://github.com/input-output-hk/hydra/issues/1096) as a dedicated
  feature, which will add a `hydra-chain-observer` tool and simplify upcoming
  feature implementation related to L1 transactions.
  
- Created a feature idea item to [migrate all validators to aiken
  #1092](https://github.com/input-output-hk/hydra/issues/1092). This was moved
  to > 1.0.0 though, as it would require a complete re-audit of the scripts and
  there is no pressing reason for the performance improvements at this stage.

## Hydra development

[Issues and pull requests closed since the last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-09-30..2023-10-31)

This month, the team worked on the following:

#### Network resilience to node crash

TODO ??

#### Hydra Poll dApp

TODO ??

#### Upgrade to GHC 9.6 & Brick 1.1.0

TODO dan

## Mithril development

TODO arnaud

## Community Update

This month, the monthly review meeting was almost completely covered by
demonstrations from the various teams working on and with Hydra. Don't miss out
on the demonstrations of the various community contributions and make sure to
also view the [recording][].

### Hypix

Hypix is a continuation of the "hydraw" demonstration the team used throughout
development. The project is spear-headed by Trym Bruset and will integrate CIP68
NFTs with instant transaction processing in a Hydra head to realize
collaboratively created art pieces. The project is progressing great with an
open beta available soon.

![Hypix user interface](./img/2023-10-hypix.png) <small><center>The Hypix user interface</center></small>

### Kupo x Hydra

Indexers of chain data, like [kupo](https://github.com/CardanoSolutions/kupo),
are useful to enable lightweight decentralized applications like Hypix. As the
application state of Hypix, for example, is not only on the Cardano layer 1, but
in the Hydra head, kupo was enhanced to run "in front of" a `hydra-node` and
provide the same API for UTxO indexed from a Hydra head. This was a [joint
effort](https://github.com/CardanoSolutions/kupo/pull/117) between
[@KtorZ](https://github.com/KtorZ), [@v0d1ch](https://github.com/v0d1ch) and
[@ch1bo](https://github.com/ch1bo) and has been [released in version 2.7 of
kupo](https://github.com/CardanoSolutions/kupo/releases/tag/v2.7).

![Kupo indexing data](./img/2023-10-kupo.gif) <small><center>Kupo indexing a Hydra head</center></small>

### Offline-mode Hydra node

TODO ??

### Voting on Hydra

TODO ??

### zkSNARKs on Hydra

TODO ??

### Mithril Signers in Operators' Guild

TODO ??

## Conclusion

TODO sebastian?

[slides]: https://docs.google.com/presentation/d/1pJMRp0YsszJenUvDmknm3wq9yyUE1CDRSYijjILrkHo
[recording]: https://drive.google.com/file/d/1U4yZhliGykxF3BddAAXb4RD417UvsQWB
