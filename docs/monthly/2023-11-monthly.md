---
title: November 2023
slug: 2023-11
authors: [ch1bo, abailly]
tags: [monthly]
---

This report summarizes the work on Hydra since October 2023. It serves as
preparation for the monthly review meeting (see [slides][slides] and
[recording][recording]), where the team updates project stakeholders on recent
developments to gather their feedback on proposed plans.

## Roadmap

This month, several items were restructured on the project
[roadmap](https://github.com/orgs/input-output-hk/projects/21/views/7):

![The roadmap with features and ideas](./img/2023-11-roadmap.jpg) <small><center>The latest roadmap with features and ideas</center></small>

#### Notable updates

- Release of version `0.14.0` is imminent as most of the [stateless chain
  observation #1096](https://github.com/input-output-hk/hydra/issues/1096)
  feature is done and only some internal refactoring missing.

- The user-submitted feature idea on supporting extended keys,
  [#1141](https://github.com/input-output-hk/hydra/issues/1141), has
  been refined. The purpose of this item is now clear, and although
  it's not currently a priority, work on it can be started.

- Two new items were created to improve the developer experience of the [Hydra build
  toolchain #1146](https://github.com/input-output-hk/hydra/issues/1146). Additionally, a
  follow-up on things learned by `hydra-poll` aims to create a full end-to-end
  example of a [two-player game in a Hydra head
  #1098](https://github.com/input-output-hk/hydra/issues/1098).

- There was already an item about reacting to main chain parameter changes on the
  roadmap, which included not only protocol parameter updates, but also
  hard-fork events. With the upcoming hard fork into `Conway`, the team split off the
  changes needed to navigate the hard fork and clarified them:

  + [Conway support #1177](https://github.com/input-output-hk/hydra/issues/1177) will make the `hydra-node` support Conway and allow users to keep heads open across the hard fork.
  + [Drop Babbage support #1178](https://github.com/input-output-hk/hydra/issues/1178) will eventually drop support for the Babbage era and retain maintainability.
  + The original item is only about [reacting to protocol parameter changes #195](https://github.com/input-output-hk/hydra/issues/195) now.

- Substantial updates were made to the design of the next major feature [Incremental decommit #1057](https://github.com/input-output-hk/hydra/issues/1057), and work on it can be started.

- In general, the team prepared several of these 💭 idea items and many of them will turn into 💬 features soon.

## Hydra development

[Issues and pull requests closed since the last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-10-31..2023-11-30)

This month, the team worked on the following:

### MeshJS + Hydra

At the beginning of month, the team attended the Cardano Summit in Dubai where they held
the _Hydra MasterClass_ workshop. There they also presented the Hydra-Poll dApp
which is using MeshJS to build Hydra transactions on the frontend.

The Hydra team collaborated closely with the MeshJS team and as the outcome of this work
The MeshJS team added some Hydra specific features to the MeshJS SDK. There is a nice low
level [API](https://meshjs.dev/apis/transaction/builderExample) for building
custom transactions which is pretty similar to what `cardano-cli` does.

Now you can use `isHydra` field which you can pass to this API which then uses
another set of protocol paramters suitable for Hydra (eg. usually in Hydra we
want to have zero transaction fees, reduce script execution units etc.).

Big thanks to the MeshJS team on giving much needed support. They were
presenting these changes in the monthly review meeting so make sure to watch the
[recording](https://drive.google.com/file/d/1-iv8IveUzA2KrJV_Kqrgx4ts05Ow0zjM).

### Rendering the Hydra specification

Besides working on the design of incremental commits itself, the team also
tidied up the way the [Hydra protocol
specification](https://hydra.family/head-protocol/core-concepts/specification)
is written.

So far, the specification was written in LaTex and rendered to a PDF. LaTeX was
a good choice because of its expressiveness on math expressions, but not so
great a choice for pull requests and the entry barrier is quite
high. The majority of documentation around Hydra and Mithril is written using
Markdown and rendered into a webpage using a static site generator, currently
[Docusaurus](https://docusaurus.io/). Most modern Markdown renderers do support
LaTeX-style math nowadays, even the normal [GitHub file
preview](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/writing-mathematical-expressions).

Hence, the team experimented with using the more standard Markdown format to write the
specification, but still being able to use LaTeX math **and** LaTeX macros
(which are _still_ heavily used in the spec). The aim is to have both, a
PDF and HTML rendering.

![](https://ipfs.io/ipfs/QmPUTYaSViLEyZcGmsTPL4jHdGSevH3yc3RD7TRhU3ivwH?filename=Peek%202023-11-28%2018-42.gif)

While not yet finished, the GIF above shows the progress being made using
[pandoc](https://pandoc.org/) to resolve macros, producing a PDF via `xelatex`
and Docusaurus compatible markdown. Using powerful extensions of pandoc one can
even convert TikZ, GraphViz and Mermaid into both target formats.

## Community update

Beside the great work done by the MeshJS team towards supporting Hydra in the browser, here are some news and updates on community-led work streams related to Hydra:

* SundaeLabs has made good progress on their [offline mode
  PR](https://github.com/input-output-hk/hydra/pull/1118) which should
  hit the main branch soon,
* TxPipe has been collaborating with IOG's Creative Engineering team
  to build _Cardaminal_, a command-line wallet, and demonstrated it
  during the monthly review meeting. Cardaminal is built in Rust,
  leveraging [Pallas](https://github.com/txpipe/pallas) libraries, and
  is aimed at being a fully-fledged scriptable wallet providing users
  with powerful stateful transaction edition capabilities, native
  assets management, chain synchronisation, etc.

  The plan is to make Cardaminal compatible with Hydra - to be able to
  use it to interact with a head - and Mithril - using snapshots to
  synchronise the wallet.

* The team behind [ZK Snarks in
  Hydra](https://projectcatalyst.io/funds/10/f10-development-and-infrastructure/a-zero-knowledge-proof-framework-for-cardano-based-on-hydra-and-zk-snarks)
  Catalyst proposal is making good progress and should be able to give
  a demo at the next monthly review meeting

## Conclusion

The monthly review meeting for November 2023 was held on 2023-11-27 via Google
Meet with these [slides][slides] and the [recording][recording].

The Hydra team is thankful to have again had so many great demos from the numerous projects
in and around Hydra and Mithril projects - we should really schedule it to be one
hour planned duration. Also, if you have not watched it already, make sure to
check out the recording, the written report is merely expanding on a couple of
topics but cannot render the cool demonstrations.

At first look, the demonstrated projects may appear not related - we even had a
minimal command-line wallet on the show after all. But as we went on, these
impulses spark discussions and it becomes clear that all of these projects are
connected.

For example, the terminal-based wallet 'Cardaminal' as it was presented the
first time in public this month, could itself become a Mithril client for faster
synchronizing or even light-mode using the recently published Rust client
library for Mithril. Furthermore, such a minimal wallet would be a great testing
ground for a Hydra-aware wallet. The MeshJS integration also has further
potential of Hydra specific `fetcher` and `submitter` providers.

On the other hand, we also stumbled over not-so-optimistic sentiment even in
much more mature scaling solutions like the Bitcoin Lightning Network. [This
tweet](https://twitter.com/udiWertheimer/status/1719122153155473492) summarizes
that creating non-custodial [scalability/LN] solutions 'feel like the most
thankless thing in the world'.

It is an exciting ride still, but events like this monthly review meeting with
its colorful demonstrations of small steps to improve the usability
and scalability of Cardano piece by piece with often unknown synergies is also
empowering. This month again made us feel not 'only a tribe', but more a working
group across the Cardano community .. the Cardano Scaling group!?

[slides]: https://docs.google.com/presentation/d/1JA_frlOXVrrBeaBGUnIq3U9cclrfU1A2cZR9B2AeVJg
[recording]: https://drive.google.com/file/d/1-iv8IveUzA2KrJV_Kqrgx4ts05Ow0zjM
