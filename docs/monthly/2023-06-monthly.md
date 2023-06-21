---
title: June 2023
slug: 2023-06
authors: [ch1bo]
tags: [monthly]
---

This report summarizes the work on Hydra since May 2023. It serves as
preparation for the monthly review meeting (see [slides][slides] and
[recording][recording]), where the team updates project stakeholders on recent
developments to gather their feedback on proposed plans.

## Roadmap

While there was not a release this month, we implemented some notable features,
which will be released soon as version 0.11.0:

![The roadmap without idea items](./img/2023-06-roadmap-ex-ideas.png) <small><center>The roadmap without idea items</center></small>

#### Notable roadmap updates

TODO @ch1bo

- Factored out commits of multiple UTxO #774

- Revisited off-chain performance by doing benchmarks #186

- Identified a bottleneck, groomed and planned in performance improvement "Event sourced persistence" #913

- Will release 0.11.0 without "Authenticate network messages #727"

- API improvements and exploring batched transactions on L2 showed that "ReqSN
  only sends transaction IDs #728" is in demand, front-loads further API changes
  and is estimated to be a low-hanging fruit.

- Reprioritized items in `<= 1.0.0` column to do items with on-chain protocol
  impact earlier and we have not had much feedback on snapshotting items. In
  fact, if we do "Support larger # of UTxO via split-fanout #190" slightly
  differently, we should avoid some of the problems of impossible to finalize
  snapshots.

![The latest roadmap with features and ideas](./img/2023-06-roadmap.png) <small><center>The latest roadmap with features and ideas</center></small>

## Development

[Issues and pull requests closed since last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-05-24..2023-06-22)

This month, the team worked on the following:

#### Commits with multiple UTxO [#774](https://github.com/input-output-hk/hydra/issues/774)

TODO @ch1bo

#### Commits from external wallet [#215](https://github.com/input-output-hk/hydra/issues/215)

TODO @v0d1ch/@ffakenz?

- Break this down into it's constituents
- Option A/B?
- Explain what is already done and what not

#### Benchmark performance of Hydra Head [#186](https://github.com/input-output-hk/hydra/issues/215)

TODO @abailly / @pgrange?

- What did we do here? What's the result?
- Including new "Event source persistence" outcome

#### Operating hydra nodes

TODO @abailly / @pgrange?

## Community

TODO @ch1bo

- Hydra / Mithril talk proposal for Cardano Summit 2023?
- Hydra hackathon on Rare Evo?

#### Hydra for Auctions contributions and closing of project

TODO @ch1bo/@nebojsa?

- Include demo video
- Yielded multiple github issues for ideas
- Demonstrated how to move scripts from L1 -> L2
- Nice example of establishing crucial state on L1 and achieving scalability through L2

## Conclusion

The monthly review meeting for May was held on 2023-06-21 via Google Meet with
these [slides][slides] and here is the [recording][recording].

TODO @ch1bo

[slides]: https://docs.google.com/presentation/d/1TVzjaFKXBi9DAugSd2L8MSUSZGIU9EjTmwf6yccckPI
[recording]: https://hydra.family
