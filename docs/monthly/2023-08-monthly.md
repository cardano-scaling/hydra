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

## Roadmap

This month, the team released version 0.12.0, which changes the way a client
can commit funds to a hydra head by leveraging the external commit feature.

The project
[roadmap](https://github.com/orgs/input-output-hk/projects/21/views/7) has been
slightly updated to focus 0.13 on network resiliency and bump incremental
commit and decommit in priority:

![The roadmap](./img/2023-08-roadmap.jpg) <small><center>The roadmap</center></small>

### Release 0.12.0

- Support cardano-node 8.1.2
  - Updated client and plutus versions
- Layer 2 protocol changes
  - Authenticated messages
  - Removed redundancy
- Event-sourced persistence
- New API endpoints
- Removal of _internal commit_ endpoint
- Improved off-chain tx processing performance
- Security fixes

- See [full release notes](https://github.com/input-output-hk/hydra/releases/tag/0.12.0) and a list of [delivered features](https://github.com/input-output-hk/hydra/milestone/12?closed=1)

## Development

TODO

[Issues and pull requests closed since last report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-07-28..2023-08-23)

This month, the team worked on the following:

#### Update tutorial and include Mithril [#997](https://github.com/input-output-hk/hydra/issues/997)

- Make tutorial work with latest version of hydra-node
- Support a wide range of platforms
- Ensure it’s kept up-to-date (using continuous integration)
- Preparation for Hydra master-class at RareEvo

#### Support cardano-node 8.1.2 [#1007](https://github.com/input-output-hk/hydra/issues/1007)

We were still using cardano node 1.35.7 so we took some time to upgrade to the
last release 8.1.2.

#### Authenticated messages [#727](https://github.com/input-output-hk/hydra/pull/727)

The Hydra Head protocol had to be immune from adversaries trying to
impersonate protocol actors via the L2 network. Otherwise an attacker
could, for example, forge ReqSn messages to make the Head stuck (because they
likely will not be consistent with other messages delivered).

Note, however, that an attacker would not be able to sign snapshots nor create
valid layer 2 transactions to spend funds even without any authenticated messages
because both require the respective signing (private) keys.

The HydraV1 specification expects messages to be authenticated. We implemented that
by inserting a new authentication component in our network stack. All messages have
now to be signed with the hydra signing key of the peer sending it.

#### Event-sourced persistence [#913](https://github.com/input-output-hk/hydra/issues/TODO)

First spike confirmed performance improvements  (master ~300ms → spike ~6ms)
Also opens up interesting possibilities for state observation in clients
Needed to refactor in between
Currently carefully porting protocol logic to be event sourced

#### New API endpoints [#TODO](https://github.com/input-output-hk/hydra/issues/TODO)

- submitTx
  - Submit L1 transactions through hydra-node #966
  - API improvement to not need a tx submission server / cardano-node on client side

- getProtocolParameters
  - Ability to read protocol parameters via API #735
  - Also small story to increase developer experience
  - Should cover everything but current time + conversion (projection possible)

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

We already mentioned our intention to have an educational session at the upcoming
Rare Evo event. The Preparation is still in progress and we fear we might have
some network connectivity challenges but here are some practical information:

- Saturday, August 26, 9:30-12:30 MDT / 15:30-18:30 UTC
- About 40 people registered for on-site event
- Planning to stream it on #hydra-live
- Planning to support people on #ask-hydra on the [IOG Discord](https://discord.gg/Qq5vNTg9PT)

### Catalyst Fund10

The team screened all the proposals mentioning Hydra and
[Mithril](https://mithril.network/doc/). We submitted eleven community reviews and
noticed, in particular, the following proposals:

- [Sundae Labs Hydra Ledger-only Mode](https://cardano.ideascale.com/c/idea/102138)
- [Sundae Labs Hydra Transaction Stream Plugin](https://cardano.ideascale.com/c/idea/102200)
- [Hydra as a B2B layer for DeFi- a white paper and a MVP](https://cardano.ideascale.com/c/idea/101626)
- [Decentralized Demeter.run - Federated Frontend Hosting - New revenue stream for SPOs](https://cardano.ideascale.com/c/idea/104411)
- [Mithril - Open-source contributor](https://cardano.ideascale.com/c/idea/105113)

[Sundae Labs Hydra Ledger-only Mode](https://cardano.ideascale.com/c/idea/102138)
proposes a Hydra node which would only receive and validate transactions, and
maintain a ledger state but not run a head per se. It should allow many useful scenarios:

- Run the ledger validation as a component in a larger layer-2 protocol
- Scenario testing frameworks
- Speculative execution

An ADR is under review on our github project:
<https://github.com/input-output-hk/hydra/pull/1012>

[Sundae Labs Hydra Transaction Stream Plugin](https://cardano.ideascale.com/c/idea/102200)
will expose internal hydra event stream to external consumers. This should allow:

- Custom Persistence formats
- Chain-explorers
- Chain indexing integration with existing ecosystem tools

## Conclusion

The monthly review meeting for Aug was held on 2023-08-23 via Google Meet with
these [slides][slides] and here is the [recording][recording].

TODO

[slides]: https://docs.google.com/presentation/d/1MrCeUsYb3FQk7aCwMZdQs8mc5BfLOIjkK9gcWzgDdDc/edit#slide=id.g1f87a7454a5_0_1392
[recording]: https://drive.google.com/file/d/14pDsf0hDyh9HK8sCSMmkmT8gY8YxgOQ8/view
