---
title: April 2023
slug: 2023-04
authors: [ch1bo, abailly, v0d1ch, pgrange]
tags: [monthly]
---

This report summarizes the work on Hydra since March 2023. It serves as
preparation for the monthly review meeting
([slides](https://docs.google.com/presentation/d/10wZJy0tyGMbvMihbHnHk0QByA_TAZrtbcRbf5Gd-SHg/)/recording
(TODO)) , where the team updates major project stakeholders on recent
developments to gather their feedback on proposed plans.

## Roadmap

The project [roadmap](https://github.com/orgs/input-output-hk/projects/21) was
only slightly updated.

![](./img/2023-04-roadmap.png) <small><center>The latest roadmap containing features and ideas.</center></small>

#### Notable roadmap updates

- Still many idea items on it. However not on the current and next planned release columns

  - Want to clarify each idea item into a feature before starting work on it

- Moved hydra heads explorer further out until concretized (for now, no blockers)

- “Dropped” aggregated multi-signature from board into discussion - Got more input from the community

- Still focus on mainnet compatibility - mostly docs and disclaimers missing now

- Completed configurable API

  - Gave rise to new ideas, which we started work on right away to validate /
    deliver a usable increment on the API side

- About to release 0.10.0, which will be the first mainnet compatible version

![](./img/2023-04-roadmap-ex-ideas.png) <small><center>The roadmap without idea items.</center></small>

## Development

[Issues and pull requests closed since last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-03-29..2023-04-26)

This month, the team worked on the following:

- **Making Hydra mainnet compatible.** ...

  - smoke tests on mainnet
  - dedicated github action runner
  - managed cardano signing key funded with real money

- **Configurable API.** ... @v0d1ch

  - history, snapshot-utxo, tx-format, status/utxo in greeting
  - dirt road, workarounds
  - filter by address and paginated responses requested?
  - `GetUTxO` and `GetUTxOResponse` became a crutch and not suitable

- **Fixed scripts + updating dependencies.** ... @ch1bo

  - explain the oddysey
  - plutonomy results and why we didn't go for it (yet)
  - fix scripts https://github.com/input-output-hk/hydra/pull/777
  - update dependencies https://github.com/input-output-hk/hydra/pull/826

- **Rollback bug hunt.** ... @pgrange

- **Versioned docs.** ...

- **Specification** in repository and on website ...

- **Miscellaneous.** ...

  - Hydra node crashed after fork?
  - Production grade web server?
  - Red bin item: upload failing cluster logs? fixed port handling in tests?

## Community

- **Hydra for Voting.** ...

  - Demo in the review meeting
  - Summarize situation on catalyst and allude to summit voting
  - maybe quickly mention tool stack, scenario and their challenges

- **Hydra for Auctions.** ...

  - New demo recording https://www.loom.com/share/7ed84e37d65748d994d8a0be147f7ecb
  - bidding in L1 and L2 and transfer of auction L1 -> L2 works

- **Kupo x Hydra.** ... @ch1bo

- **CBIA meetings.** ... @abailly

- **Twitter spaces.** ...

## Conclusion

The monthly review meeting for March was conducted on 2023-04-26 via Google
Meet -
[slides](https://docs.google.com/presentation/d/10wZJy0tyGMbvMihbHnHk0QByA_TAZrtbcRbf5Gd-SHg/)/recording
TODO.
