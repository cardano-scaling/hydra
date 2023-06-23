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

The team started to mark _fuel_ some time ago as it was an easy workaround to
distinguish UTxOs that can be committed into a head apart from regular outputs
holding ADA to pay for fees - the socalled _fuel_. However, this required users
to "send funds" they want to commit first to the `hydra-node`s internal wallet
and involved additional steps in tagging such outputs with a special datum hash

To commit from external wallets, a new API endpoint was introduced for the
purpose of _drafting_ a commit transaction. The clients would request such draft
transaction by sending a POST request to `/commit` and the `hydra-node` would
respond with a transaction already authorized by the internal wallet. If the
commit involved user funds (empty commits are still possible), then the client
applicatin would need to sign the transaction using the corresponding signing
key. Also, submitting this transaction has shifted from `hydra-node` to the
client.

This removes direct custody of `hydra-node` over user funds since clients can
now use whatever key they own, not known to the `hydra-node`, to do a commit
step and no single `hydra-node` has access to user funds used in the Head
protocol.

Within this work package, _marking fuel_ became deprecated and all UTxOs owned
by the internal wallet are considered fuel. Fuel marking wil be completely
removed in the future. Furthermore, we started using a good old HTTP-based API
for the new query (so far it was only WebSocket-based), which prompts a
potential shift to using OpenAPI as API specification since AsyncAPI does not
describe synchronous requests well.

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
[recording]: https://drive.google.com/file/d/1_N6b4RDe579TgLawiJzbE0NLofD3ljE6/view
