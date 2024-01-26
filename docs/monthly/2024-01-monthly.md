---
title: January 2024
slug: 2024-01
authors: [ch1bo, abailly]
tags: [monthly]
---

This report summarizes the work on Hydra since January 2024. It serves as
preparation for the monthly review meeting (see [slides][slides] and
[recording][recording]), where the team updates project stakeholders on recent
developments to gather their feedback on proposed plans.

## Roadmap

This month, several items were restructured on the project
[roadmap](https://github.com/orgs/input-output-hk/projects/21/views/7):

![The roadmap with features and ideas](./img/2024-01-roadmap.jpg) <small><center>The latest roadmap with features and ideas</center></small>

#### Notable updates

## Hydra development

### Hydra Chess

- Hydra team wanted to build a game on Hydra since by dogfooding their product
  they always find more things which are suboptimal or stumble accross a bug
  that needs fixing. Hydra Chess proved to be no different and they learned in
  the process of making this dApp.

  They also tried to iron out all details and difficulties present when trying
  to run hydra-node and have dApp that is easy to use by the end users which
  might not be tech savvy but also provide an example for people trying to
  build on Hydra. You can find the source code
  [here](https://github.com/abailly-iohk/hydra-chess).

[Issues and pull requests closed since the last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-11-30..2024-01-31)

This month, the team worked on the following:

### TODO

### Hydra Explorer
To measure the progress and success of Hydra, we require tools that provide insights into its usage. For this purpose, we have initiated work on [#696](https://github.com/input-output-hk/hydra/issues/696) to enable the tracking of the lifecycles of Heads and observe the growth of the Hydra ecosystem.

In this initial phase, we have developed a fundamental backend service [#1235](https://github.com/input-output-hk/hydra/pull/1235) that can track all Babbage heads on-chain within a devnet network. This service establishes a baseline and utilizes the hydra-chain-observer package, exposing a REST API for querying and retrieving information about all observed heads and their current states.

## Community update

Beside the great work done by the MeshJS team towards supporting Hydra in the browser, here are some news and updates on community-led work streams related to Hydra:

* TODO

## Conclusion

The monthly review meeting for January 2024 was held on 2024-01-19 via Google
Meet with these [slides][slides] and the [recording][recording].

TODO

[slides]: https://docs.google.com/presentation/d/1JA_frlOXVrrBeaBGUnIq3U9cclrfU1A2cZR9B2AeVJg
[recording]: https://drive.google.com/file/d/1-iv8IveUzA2KrJV_Kqrgx4ts05Ow0zjM
