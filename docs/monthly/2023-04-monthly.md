---
title: April 2023
slug: 2023-04
authors: [ch1bo, abailly, v0d1ch, pgrange]
tags: [monthly]
---

This report summarizes the work on Hydra since March 2023. It serves as
preparation for the monthly review meeting
([slides](https://docs.google.com/presentation/d/10wZJy0tyGMbvMihbHnHk0QByA_TAZrtbcRbf5Gd-SHg/) &
[recording](https://drive.google.com/file/d/1X4yPerLTatPPMrX3RYS7XH9lfT_LYaaX/view?usp=sharing)),
where the team updates major project stakeholders on recent developments to
gather their feedback on proposed plans.

## Roadmap

The project [roadmap](https://github.com/orgs/input-output-hk/projects/21) was
only slightly updated.

![](./img/2023-04-roadmap.png) <small><center>The latest roadmap containing features and ideas.</center></small>

#### Notable roadmap updates

- As you can see, there are still many 💭 **idea** items on the roadmap.
  However, not on the current and next planned release columns. The process is
  to clarify & groom each idea item into a 💬 **feature** before starting work
  on it, while equally considering new user feature 💭 **idea**s and requests.

- Moved [Hydra heads explorer
  #696](https://github.com/input-output-hk/hydra/issues/696) item further out
  for now until concretized. There are no blockers really and this just needs
  more detailing as we are in talks with existing Cardano explorer platforms.

- Converted the [aggregated multi-signature
  #193](https://github.com/input-output-hk/hydra/issues/193) from the concrete
  roadmap into an [idea discussion
  #787](https://github.com/input-output-hk/hydra/discussions/787) as we got more
  input from the community and it's better to discuss there.

- Main focus for 0.10.0 is mainnet compatibility, which is mostly done and only
  some docs and disclaimers are missing now.

- Meanwhile the [configurable API
  #380](https://github.com/input-output-hk/hydra/issues/380) was completed. Work
  on this gave rise to new ideas and follow-up feature requests from users. One
  of them ([Add HeadState/Snapshot to Greetings
  #823](https://github.com/input-output-hk/hydra/issues/823)) was fairly
  straight-forward and necessary to deliver a consistent usable increment on the
  API with the upcoming release.

- About to release 0.10.0, which will be the first mainnet compatible version

- Prioritized [Support timed transactions
  #196](https://github.com/input-output-hk/hydra/issues/196) higher as yet
  another use case would benefit from this.

![](./img/2023-04-roadmap-ex-ideas.png) <small><center>The roadmap without idea items.</center></small>

## Development

[Issues and pull requests closed since last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-03-29..2023-04-26)

This month, the team worked on the following:

- **Configurable API.** Our API evolved a bit driven by the issues our users
  reported [#823](https://github.com/input-output-hk/hydra/issues/823)
  [#813](https://github.com/input-output-hk/hydra/issues/813)
  [#800](https://github.com/input-output-hk/hydra/issues/800)
  [#789](https://github.com/input-output-hk/hydra/issues/789).
  Related changes were added to our API server so now our clients can:
  - Control the historical messages output. History messages can be displayed
  uppon re/connection or not depending on client needs.
  - Snapshot UTxO's can optionally be disabled.
  - Transactions can be displayed in cbor of json format.

  Our clients can also have a nice insight into current Hydra node state and Head utxos
  that are now displayed as part of a `Greetings` message.

  Next steps on the API level are to further fulfill user needs by grooming and
  implementing needed changes related to filtering, pagination etc.

- **Versioned docs and specification.** Over the [last couple
  months](./2023-02#development) the Hydra specification became an important
  artifact to use in discussion, review and potential audit of the Head protocol
  implementation. The document was now moved from overleaf into the Hydra
  repository, where it is properly versioned and built on each CI run. Changes
  can be proposed using our normal pull request worfklow and the final PDF is
  built and [published to the
  website](https://hydra.family/head-protocol/unstable/core-concepts/specification)
  automatically.

  Note that the above link points to the new `/unstable` version of the
  documentation, which holds the bleeding edge user manual, specification and
  api reference (which got a new sidebar) built directly from `master`. The
  normal, non-unstable version of the website is always referring to the [last
  released version](https://github.com/input-output-hk/hydra/releases).

![](./img/2023-04-specification.png) <small><center>Specification on Hydra website</center></small>

- **Fixed scripts, plutonomy and custom script contexts.** As we made the
  specification use a more direct way to represent transactions (instead of the
  constraint emitting machine formalism), we realised that our scripts are not
  correctly ensuring _script continuity_. We identified these "gaps" as red
  sections (see above) in the specification and worked on fixing them.

  While the [actual fix #777](https://github.com/input-output-hk/hydra/pull/777)
  was fairly straight forward and could easily be covered by our mutation-based
  contract tests, the script size increased and we could not publish all three
  Hydra scripts in a single publish transaction (which allows for a single
  `--hydra-scripts-tx-id` parameter on the `hydra-node`).

  To mitigate, we looked into the UPLC optimizer
  [plutonomy](https://github.com/well-typed/plutonomy/tree/master/src/Plutonomy).
  Applying it was fairly simple, our tests did also pass, script sizes _and
  costs_ also became lower. But, script size does not matter so much as we are
  using reference scripts and using a (not really maintained?) optimizer which
  introduces yet another question mark after compilation from `plutus-tx` to
  `uplc` was not our cup of tea.. right now at least (and we might pull this out
  of the drawer later).

  There is an alternative: decoding `ScriptContext` involves quite some code,
  but we don't need it everything in all validators. So we introduced a custom
  script context which only decodes the fields we need.

  | scripts  | @0.9.0 | fixes | fixes + plutonomy | fixes + custom ScriptContext |
  | -------- | ------ | ----- | ----------------- | ---------------------------- |
  | νInitial | 4621   | 4727  | 3672              | 4300                         |
  | νCommit  | 2422   | 2513  | 1816              | 2068                         |
  | νHead    | 8954   | 9492  | 7579              | 9456 (no custom SC)          |
  | μHead    | 4458   | 4537  | 3468              | 4104                         |

  In the process of this, we also [updated dependencies
  #826](https://github.com/input-output-hk/hydra/pull/826) to latest
  `cardano-node` master. It did not help on the script sizes, but is a great
  preparation for upcoming hard-forks.

- **Rollback bug hunt.** ... @pgrange

## Community

- **Hydra for Voting.** The project is advancing and a basic vote tallying
  scenario in the Catalyst use case was demonstrated in the review meeting. The
  project is driving the API discussions as it is not using any Haskell tooling,
  but an application in Java with Aiken as the validator scripting language.
  Besides the catalyst use case, other scenarios like the ballot voting for the
  summit are also explored now.

- **Hydra for Auctions.** A new demo was recorded in the wake of an upcoming
  twitter space discussing auctions and NFT marketplaces with the community. The
  feature set currently includes starting the auction on L1, bidding on L1 or
  (and this is the novel thing!) transferring the auction from L1 to L2, such
  that it can be bid on L2.

  <div style={{position: "relative", paddingBottom: "56.25%", height: 0}}>
    <iframe src="https://www.loom.com/embed/7ed84e37d65748d994d8a0be147f7ecb"
    frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen
    style={{position: "absolute", top: 0, left: 0, width: "100%", height:
    "100%"}}></iframe>
  </div>

- **Kupo x Hydra.** In a good old pairing session between IOG and CF engineers,
  the integration of Kupo with Hydra was explored. This seems to be promising
  and work started [here
  kupo#117](https://github.com/CardanoSolutions/kupo/pull/117). This will make
  it possible to run `kupo` natively connected to a `hydra-node`, very much it
  would run with `cardano-node` or `ogmios`. Kupo is a light-weight indexer of
  chain data like unspent transaction outputs and allows its clients to query
  information on-demand. 🐹

- **CBIA meetings.** ... @abailly

- **Twitter space on Scaling Cardano.** This month we took part in a Twitter
  space about scaling Cardano and how Hydra can contribute to this. Thanks for
  conducting this [@thepizzaknight\_](https://twitter.com/thepizzaknight_) 🙏

  <a href="https://twitter.com/i/spaces/1vOxwMVDaXLGB">

  ![](./img/2023-04-twitter-space.png)

  </a>

## Conclusion

The monthly review meeting for April was held on 2023-04-26 via Google Meet with
these
[slides](https://docs.google.com/presentation/d/10wZJy0tyGMbvMihbHnHk0QByA_TAZrtbcRbf5Gd-SHg/)
and here is the
[recording](https://drive.google.com/file/d/1X4yPerLTatPPMrX3RYS7XH9lfT_LYaaX/view?usp=sharing).

Although it has been a busy month we could not cut a release unfortunately.

Multiple set backs on the commits vs. rollbacks bug and too big script sizes
were slowing us down. The back and forth on the API with sometimes very "dirt
road" solutions and follow-up requests is of course time intensive as well, but
this is **very** valuable feedback and will make `hydra-node` easier to use and
more capable step-by-step.

Associated projects in the greater Hydra community are moving ahead nicely
because of the collaborative approach and tight loops of interaction between the
individual teams.

All things considered, the project can be considered on-track. We are very close
to cut our first mainnet compatible release and the rising number of user
requests and interested developers are a good indicator that adoption of Hydra
is increasing.
