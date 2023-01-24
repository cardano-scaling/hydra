---
title: January 2023
slug: 2023-01
authors: ch1bo
tags: [monthly]
---

This report summarizes the work on Hydra since December 2022 and also serves as
a preparation & write-up of the monthly review meeting, in which we update major
stakeholders of the project on recent developments and gather their feedback on
our proposed plan forward each month.

## Roadmap

Looking at our [roadmap](https://github.com/orgs/input-output-hk/projects/21)
just a few things to report this month:

- Regroomed [Create updated specification for coordinated head protocol
  #448](https://github.com/input-output-hk/hydra/issues/448) to :yellow_heart:,
  as we made good progress on the specification

- Marked [Align implementation with HeadV1 protocol specification
  #452](https://github.com/input-output-hk/hydra/issues/452) as :red_circle:,
  because we experienced scope creep and have not progressing as fast as we
  expected on this.
  + We plan on breaking this up into smaller chunks of work which are addressing
    and explaining a short-coming in the implementation each.
  
![](./img/2023-01-roadmap.png)
<small><center>
Latest roadmap with only minor changes. Need to reflect latest objectives into it.
</center></small>

## Development

[Issues closed since last report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2022-12-23..2023-01-24)

This month we have been working on:
  
- **First write-up of Hydra spec online**: As already mentioned last time, we
  have been aggregating the latest definitions of on- and off-chain semantics
  into a new technical specification. The latest version can be found [still on
  overleaf](https://www.overleaf.com/project/6389ba5edbcf7a51fda1328f), while we
  are looking at integrating it into the core hydra repository. Should you have
  feedback, please send it our way through one of the [communication
  channels](https://github.com/input-output-hk/hydra/blob/master/SUPPORT.md).

- **Closed more gaps in Head Contract**: We addressed two more gaps in our
  on-chain scripts based on discussions on the specification.

  + All validators are authenticated now and ensure contract continuity by checking
    for the state tokens in the output (or getting burned).

  + Abort now fully reimburses all committed UTxO, even if two parties would
    have committed the exact same output (quite theorethical attack vector).

  This work is captured by
  [#452](https://github.com/input-output-hk/hydra/issues/452), but this work
  item is taking quite a bit longer than we expected. Also, the fact it's a
  single feature item and it lacks a bit rationale on why some things are
  required, so we intend to break this down into smaller pieces over the next
  days & weeks.

- **Mutation test framework improvements**: While closing gaps in our contracts,
  we realized that some of our [mutation
  tests](https://hydra.family/head-protocol/haddock/hydra-node/tests/Hydra-Chain-Direct-Contract-Mutation.html)
  were correctly tripping validators and making transactions invalid, but not
  always for the right reason. In fact, some of our mutations were "too harsh"
  and making the transaction even not even pass phase-1 validation. We addressed
  this by introducing a first (naiive) way to assert the right cause of failure
  by checking strings in the validator log.
  [#679](https://github.com/input-output-hk/hydra/pulls/679)

- **Add HeadId in API & TUI** to make it easier of identifying heads. We added
  not only the unique `headId` to approriate server outputs and the TUI
  [#678](https://github.com/input-output-hk/hydra/pull/678), but also added
  `seq`uence numbers and `timestamp` to produced outputs
  [#618](https://github.com/input-output-hk/hydra/pull/618). This was wished by
  users and makes integrating with the `hydra-node` easier / possible.

- **Hydra explorer experiment**: Some of us used the holiday season to conduct
  some experiments on summarizing Hydra Heads observed on a network. This is
  only a quick hack, but already demonstrates the value of a hydra explorer as a
  tool to measure adoption by open heads on a given network. Naturally it would
  reuse code from the `hydra-node` and challenge the architecture used in it for
  tracking multiple heads. Also, it will drive discussion about versioning of
  `hydra-plutus` and how we could track multiple versions of the Hydra protocol
  on the chain (hashes/addresses change in each version).
  
![](./img/2023-01-explorer.png)
<small><center>
Hydra explorer first experiment UI
</center></small>

## Community

- **Hydra for Voting project**, which got kicked off in December is picking up
  steam with deep dives on Catalyst voting and Hydra as technology. Discussions
  are currently had in various settings and we're continuing to develop a
  picture of what is achievable.

- **Hydra for Auctions litepaper**: We are partnering with MLabs on
  exploring how Hydra can improve auction use cases. At this stage, we discussed
  multiple approaches how to structure the problem space with various Hydra
  topologies and their trade-offs. The full case study was published [on
  Essential
  Cardano](https://www.essentialcardano.io/article/implementing-auction-projects-using-hydra),
  while we plan on upstreaming the findings back into the [use case
  section](https://hydra.family/head-protocol/use-cases/nft-auction/) on the
  Hydra website from which this project originated.
  
## Themes for 2023

After kicking off planning in a
[workshop](./2022-11-monthly.md#cardano-summit--workshop) and reflecting on
[what we achieved in
2023](https://cardanofoundation.org/en/news/hydra-head-protocol-an-open-source-solution-for-scalability/),
we have progressed in fleshing out relevant themes and objectives for this year
to position Hydra as a sustainable open-source project in the age of Voltaire:

- **A mainnet mature application**
  + **Why?** We (the core contributors) should be able to use the protocol on the mainnet ourselves, it should be maintainable and mature enough to lock some ADA in a Hydra Head using a reference DApp (e.g. hydraw). By creating & growing this DApp ourselves we will [dogfood](https://en.wikipedia.org/wiki/Eating_your_own_dog_food) features and hence improve usability as well.
  + **How to measure?** Number of heads on mainnet `> 0` and core contributors feel confident to lock at least `100₳` in a Hydra Head on mainnet.
  + **Next steps:**
    - Publish Hydra Head V1 specification
    - Close gaps in implementation and release 0.9.0
    - Create a request for proposals (RFP) to audit spec & implementation

- **Increase adoption**
  + **Why?** We want to showcase what is possible with Hydra through benchmarks
    and lighthouse projects, enable use cases and reducing friction to use
    Hydra. Also, ensuring interoperability through open standards and reference
    implementations.
  + **How to measure?** Number of third-party created heads on any network `> 0`
  + **Next steps:**
    - Build a basic Head explorer to measure adoption
    - Benchmark off-chain performance (of a selected scenario)
    - Demonstrate Hydra for Payments to potential users
    - Support Hydra for Voting project
    - Hydra for Auctions project exploring a delegated voucher auction

- **Sustainable open-source-development**
  + **Why?** We want to make Cardano grow & be scalable also in the long run.
    Hence this project should not be owned by a single entity, but by the
    community and the Cardano network itself. We need to make contributions to
    it possible and ultimately it should be easy to become a contributor.
  + **How to measure?** Number of contributors on github
  + **Next steps:**
    - Open up our monthly reviews
    - Add tutorials to the website
    - Publish & maintain a use case centric roadmap and feature map

## Conclusion

The monthly review meeting accompanying this month was the first public one
where we invited not only stakeholders from IOG and the CF, but also partners of
our latest projects. This was the first step to a more open and transparent
development process.

We did show some classic demos, like our hydraw application - which we have been
running on the same Hydra Head for 4 weeks now - and some new developments on
the hydra explorer and minor improvements to hydra-node API and TUI.

It was very interesting to bring the various teams together as we exchanged
ideas and thoughts on our individual roadmaps for the new year. We also shared
the relevant themes & objectives of the Hydra project for 2023 (as also listed
above) and will now make sure to take reflect these steps into our concrete
[roadmap](https://github.com/orgs/input-output-hk/projects/21) such that it will
contain [marketable features](https://www.agilealliance.org/glossary/mmf/)
bringing us closer to these goals.
