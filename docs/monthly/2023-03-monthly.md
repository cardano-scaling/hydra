---
title: March 2023
slug: 2023-03
authors: [Franco, v0d1ch]
tags: [monthly]
---

This report summarizes the work on Hydra since February 2023. It serves as
preparation for the monthly review meeting, where the teams update major project
stakeholders on recent developments to gather their feedback on proposed plans.

## Roadmap

Lets start with our roadmap as usual [roadmap](https://github.com/orgs/input-output-hk/projects/21).

- Released the new version [0.9.0](https://github.com/input-output-hk/hydra/releases/tag/0.9.0) :tada:

- Our current roadmap changes are directed towards creating something that users actually need.
  That is why we converted some issues to ideas and/or discussions to gauge the community interest in them
  and allow for a nice back and forth on features users need.

- This release brought in on-chain and off-chain changes that are fully reflected in our specification.

- We managed to decrease costs of our plutus scripts by using new error code
  framework.

- Our mutation test suite is now complete and we made sure it checks our tests
  are failing for the right reason.

- It is worthwhile mentioning we had some external contributions.

- Next version **0.10.0** is almost ready. This will be the first mainnet
  compatible release and we are close to publishing it.

- We opened a head on mainnet already! Community welcomed this nicely and a lot of people were drawing
on the Hydraw mainnet instance and managed to take it down in the process!
<blockquote class="twitter-tweet"><p lang="en" dir="ltr">JUST IN: We have a Hydra Head live on the <a href="https://twitter.com/hashtag/Cardano?src=hash&amp;ref_src=twsrc%5Etfw">#Cardano</a> Mainnet 🚀 <a href="https://t.co/6kDKq7T7no">pic.twitter.com/6kDKq7T7no</a></p>&mdash; Emmanuel 𓂀 🍕 𓅓🇬🇭🦄🟣⚡️ (@thepizzaknight_) <a href="https://twitter.com/thepizzaknight_/status/1638572527789252608?ref_src=twsrc%5Etfw">March 22, 2023</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

- Gathered the team this time in Feldkirch Austria for some nice sessions and
  discussions on future plans for Hydra.

- Monthly review meeting
  [slides](https://docs.google.com/presentation/d/1yZ4AqUQ8OBMG9ARMYvj3IOjaIAqglf7kZei4vsLMrbs/edit#slide=id.g1f87a7454a5_0_1392)/[recording](https://drive.google.com/file/d/1krzM8VN-gpKTtpVdC2JQx-lGti-4gKQS/view?ts=641b3c1c)

![](./img/2023-03-roadmap.png) <small><center> The latest roadmap with multiple
new items and starting to reflect Hydra objectives of 2023.</center></small>

## Development

[Issues and pull requests closed since last
report](https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-02-24..2023-03-29)

This month, the team worked on the following:

- **The Hydra had a team workshop** and conducted the monthly review meeting for
  March. They demonstrated a Hydra Head on mainnet, which was running the hydraw
  demo application. While this marks an important milestone, they also
  emphasized that more features are still to be added as needed for applications
  to run on Hydra. Besides this, the roadmap is getting cleaned up to encourage
  discussions and provide more space for user requests.

- **Making Hydra Mainnet compatible**. In the process we decided we want to safe
  guard our users and prevent them from shooting themselves in the foot with a
  mainnet gun. That is why we
  [limited](https://github.com/input-output-hk/hydra/issues/762) the amount of
  ADA you can commit to a head on mainnet. Our smoke-tests should be running on
  mainnet also so we made sure to
  [return](https://github.com/input-output-hk/hydra/pull/770) the leftover funds
  back to out faucet. There was also
  [work](https://github.com/input-output-hk/hydra/pull/775) on our CI that
  accomodates actually running the tests on mainnet. We also realized we will need a
  custom github runner for this.

- **Improving the Hydra UX**. We noticed a possible pitfall for the users where
  you could restart your node using different parameters which would conflict
  with the existing state. Now, hydra-node would
  [prevent](https://github.com/input-output-hk/hydra/issues/764) this kind of
  misconfiguration. Since we also want to make sure Hydra is safe to use in a
  sense that you users can always fanout what they commited we
  [prevent](https://github.com/input-output-hk/hydra/issues/698) committing the
  output with reference scripts inside since that can easily lead to a stuck
  Hydra Head.

- **Optimize the on-chain scripts** Reduced the cost of opening/closing a Head
  (error codes [#748](https://github.com/input-output-hk/hydra/pull/748) + head
  reference script [#701](https://github.com/input-output-hk/hydra/pull/701)).
  Related to handling our plutus scripts we
  [added](https://github.com/input-output-hk/hydra/pull/772) them to our golden
  tests so we can detect accidental changes.

## Community

=== Mention our community members wanting to help out with the website here ===
=== Mention CBIA meeting here ===

## Conclusion

The monthly review meeting for March was conducted on 2023-03-27 via Google
Meet -

[slides](https://docs.google.com/presentation/d/1yZ4AqUQ8OBMG9ARMYvj3IOjaIAqglf7kZei4vsLMrbs/edit#slide=id.g1f87a7454a5_0_1392)/[recording](https://drive.google.com/file/d/1krzM8VN-gpKTtpVdC2JQx-lGti-4gKQS/view?ts=641b3c1c)
