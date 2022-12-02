---
title: November 2022
slug: 2022-11
authors: ch1bo
tags: [monthly]
---

## Introduction

This report summarizes the activities of the Hydra team since October 2022 and also serves as a preparation & write-up of the monthly review meeting, in which we update major stakeholders of the project on recent developments and gather their feedback on our proposed plan forward each month.

## Starmap update

This month we had a look again on the high-level plan which includes not only development, but also research and product efforts. We started structuring the project like this in the original project plan a year ago and had a first review on this 6 months ago.

![](./img/2022-11-starmap.png)
<small><center>Latest version of the starmap with some annotations</center></small>

<br/>

Reflecting on the last half year, we can say that our release cadence was not as regular as we anticipated. While we continued cutting releases, the rate and size of the releases decreased. Also, we missed schedule for the milestone of a feature-complete, but not-yet-audited version of the hydra-node by October 2022. Both issues can be explained by team fluctuation: MB moving to the CF and AB becoming Head of Architecture resulted in both picking up new responsibilities and obviously spending less time on the project (although they are still very much part of it). Although we could welcome three more full-time engineers on the project, onboarding them has now been mostly done by SN, resulting in not much productivity left. We still hope to deliver this first **Alpha scope milestone** soon and aim for a mainnet release in the next 3-6 months.

The Audit work stream is also delayed. We started preparation for this in June, but could not line-up an internal audit until September - and this picks up only slowly now. Initially the internal audit was a pre-requisite for progressing on a proper, external audit. This has been lifted and we are now pursuing both simultaneously.

Over the last couple of months, we became less involved in the research track. We have been mostly specifying & reviewing the actually implemented “flavor” of the protocol (Coordinated Hydra Head). There will be more need for R&D though with incremental de/-commits further down the road.

The **scaling smart contracts** product track saw not as much progress as we would have hoped, but at least SundaeSwap (SS) has been continuing their exploration & early adoption, even showcasing their DEX running in a Head lately. They want to continue this track by also looking at creating an add-on protocol. The Catalyst challenge for Hydra projects was not accepted for Fund10. However, Catalyst itself might become a use case though - we are currently drafting a joint project with the IOG Catalyst team, the CF and maybe SS to explore Catalyst voting on Hydra. Besides this, we have started a lighthouse project with MLabs on NFT auctions and also the CF itself is starting a project about an NFT marketplace using Hydra.

In the **scaling payments track**, the first lighthouse project is coming to an end now. We have been creating **Hydra for Payments** ([HydraPay](https://github.com/obsidiansystems/hydra-pay)) open-source tooling for Managed Hydra Heads and easy access to payment channels with Obsidian Systems. This has been stirring up some dust and we see Obsidian Systems is keen on continuing this track. We are currently considering to also help integrating HydraPay with a concrete B2B payments use case: pay-per-use APIs.

## Roadmap update

On the more short-term focused Roadmap we can report the following releases and updates:

#### Release `0.8.0`

- Long overdue feature release adding persistence to the hydra-node
- Backup & restore the state of a Hydra Head [#187](https://github.com/input-output-hk/hydra/issues/187)
- Improve user experience following hydraw experiment [#518](https://github.com/input-output-hk/hydra/issues/518) from being only a discussion to a (to-be-groomed & planned) feature
- [Full release notes](https://github.com/input-output-hk/hydra/releases/tag/0.8.0)

#### Release `0.8.1`

- Follow-up release addressing user feedback on persistence from `0.8.0`
- Allow clients to see latest state after restart [#580](https://github.com/input-output-hk/hydra/issues/580)
- Bug fixes of following chain state [#599](https://github.com/input-output-hk/hydra/issues/599)
- [Full release notes](https://github.com/input-output-hk/hydra/releases/tag/0.8.1)

#### Notable changes

- Impact mapping from workshop (see below) and reviewing the starmap above had us discuss whether the scope for `1.0.0` is correct or not
- Need to be conscious of scope creep vs. getting something audited
- Focus on getting the spec done, `0.9.0` tagged and an audit under way now → minor re-ordering in priorities
- Iterative releases afterwards still - even while getting audited, non-contract changes first
- Promoted [https://github.com/input-output-hk/hydra/issues/635](https://github.com/input-output-hk/hydra/issues/635) from being only a discussion to a (to-be-groomed & planned) feature

![](./img/2022-11-roadmap.png)
<small><center>
Latest roadmap with 0.8.0 and 0.8.1 already released and slight re-ordering on 0.9.0

</center></small>

## Development

Issues closed since last report: [Issues - input-output-hk/hydra](https://github.com/input-output-hk/hydra/issues?page=1&q=is%3Aclosed+sort%3Aupdated-desc+closed%3A%3E%3D2022-10-19)

Besides the things in the releases above (see roadmap update), we have
been working on:

- **Flaky TUI CI tests:** We have sunk quite some time again in
  investigating cryptic CI failures in our TUI end-to-end tests. They
  have been crashing abruptly without any information and thus
  hard-to-debug. This investigation was unsuccessful and we realized
  at some point that the TUI tests are not that important anyways - we
  have API-level end-to-end tests. So we disabled these tests for now.

- **Published** `hydra-cardano-api` **to CHaP:** With the Cardano
  Haskell Packages (CHaP) becoming available now and wanting to be a
  good citizen, we pushed for getting our flavor of the `cardano-api`
  also published there. This is non-trivial though, as we are at an
  integration point even "further up" than the `cardano-node`
  [#504](https://github.com/input-output-hk/hydra/issues/504)

- **Implemented ADR18:** While the first stints on persistence already
  make it possible to restart `hydra-node` without needing to close a
  Head, we have implemented this in a nicer way now only keeping a
  single state (ground truth) for both L2 and L1 information
  [#541](https://github.com/input-output-hk/hydra/issues/541) of the Head protocol and the specification, we kept
  discovering bigger and bigger issues and hence realized the need for
  a discussion on transaction validity in context of closing /
  contesting Heads. [#615](#615)

## Formal verification & specification

An important part of the project right now is the formalization and
audit preparation:

- **Internal audit** has started with involvement of two persons from
  Charles Morgan's team, each one addressing different part of Hydra:

  - One auditor is working on the project as a whole, targeting
    potential vulnerabilities with the off-chain code, the
    infrastructure, dependencies, etc. This has not lead to any
    significant issue nor action plan so far.
  - Another auditor is specifically targeting the formal
    specification and the on-chain code to identify vulnerabilities
    in the protocol itself.

- Discussions with researchers have lead to the development of a joint
  [Coordinated Hydra Head
  V1](https://docs.google.com/document/d/1XQ0C7Ko3Ifo5a4TOcW1fDT8gMYryB54PCEgOiFaAwGE/edit#heading=h.8t0g5xu875ms) specification defining formally the protocol as it is actually implemented.

  - Work on this document has already allowed us to identify gaps
  - It is the basis on which BCryptic's analyst is working to audit Hydra protocol
  - It allows us to make explicit a lot of implicit assumptions that are in the code but not in the original paper and "Shape" the language used to describe the protocol

- We have drafted an RFP for submission to **external** auditors
  [https://github.com/input-output-hk/hydra/blob/audit/rfp/security/RFP.md](https://github.com/input-output-hk/hydra/blob/audit/rfp/security/RFP.md) defining the scope and targets of the audit

- We are still unsure about whether our **approach** is the right one as having a "formal specification" in a manually checked document spanning a dozen pages seems quite brittle.

  - Some progress has been made on "formalizing" properties to be automatically "QuickChecked" using an executable model of the expected behavior of the system but this approach seems more suited for "team-internal consumption", e.g. building confidence within the core committers and contributors

  - Ideally, we would want a proper formalization of the protocol, using an existing theorem proving/model checking framework, through which properties could be asserted. Quite a lot of work has already been done in blockchain space, including some work on Lightning and TLA+ or some other state-machine/temporal logic based language appear promising:

    - [https://www.youtube.com/watch?v=wecVT_4QDcU](https://www.youtube.com/watch?v=wecVT_4QDcU)
    - [https://github.com/rberenguel/tla_lightning](https://github.com/rberenguel/tla_lightning)
    - [https://www.amazon.com/Practical-TLA-Planning-Driven-Development/dp/1484238281](https://www.amazon.com/Practical-TLA-Planning-Driven-Development/dp/1484238281)

  - This effort should be coordinated with Simon Thompson and the smart contracts team as part of the DApps certification process

## Product

Most updates on the product side of things have been addressed in the
starmap update above.

- **Hydra for Payments lighthouse project**: Latest demonstration
  shown by Obsidian Systems on how to use payment channels in a "light
  way" from a web frontend. The API evolved and we were able to use
  the `preview` testnet now. The project is progressing very nicely
  and we are optimistic to close it successfully soon with improved
  documentation and usability.

- **NFT auctions lighthouse project:** After doing surveys and
  interviews, MLabs is currently concluding the discovery phase with a
  light / white paper on how NFT auctions could be implemented using
  Hydra (today or in the future with more features).

- **Drafting Voting on Hydra project**: Work has started on building a
  Proof-of-concept for voting on Hydra targeting Catalyst’s use case but with an eye towards building a generic solution suitable for large scale voting systems based on Hydra Head

  This development should be undertaken jointly with the Cardano Foundation and SundaeSwap who are also interested in building such a system and recently demonstrated their capacity of doing voting via the Cardano Summit voting system.

- **TxPipe demonstrated integration with demeter.run:** Without our
  help nor even knowing of this effort, TxPipe has recently shown an
  early prototype of `hydra-node`s instrumented via their
  [demeter.run](https://demeter.run) platform. This has come a bit as a surprise and is exactly why we love open source :heart: → [Video](https://www.loom.com/share/c811360e60084f18ab9e9f16cc941432)

## Team & open source

Some notable developments this month has been:

- **Renamed the repository:** from **hydra-poc** to
  [**hydra**](https://github.com/input-output-hk/hydra)! This was revealed in the summit presentation (see below) and should indicate that the project is not only a Proof of Concept (POC) anymore, but has become more - as also demonstrated by the various early adopters and demos lately. :dragon_face:

- **Inner source (IOG) contribution:** The education team has been working on a Hydra Tutorial and we have been involved in reviewing and trying it out. This is a great effort and will help people get started with Hydra. Thanks folks :green_heart:

## Summit & workshop

This month there was also the Cardano summit, this time organized by the
Cardano Foundation and the content was even voted on by the community!
We have been both, invited and nominated as panelist and speakers and
this makes us extremely proud! In general, the reception of Hydra seems
to be very positive in the community from what we could experience first
hand on the summit.

Agenda items we participated in:

- [Best of blockchain, best of open source - Open Source panel ](https://summit.cardano.org/agenda-day-1/best-of-blockchain-best-of-open-source/) with IOG, CF & TxPipe

- [Cardano Ballot Speaker Winner: Dev Team](https://summit.cardano.org/agenda-day-2/cardano-ballot-speaker-winner-presentation-6/) Introduction to Hydra and
  reveal of the repository rename 🎉

After attending the summit, we also used the fact that the whole team is
in one location for a team workshop. We booked a coworking space and
spent 3 days together. Not much coding & hacking this time, but we are
happy to have produced these results:

- **Timeline / year in review:** As we had new team members with us,
  the request was to give a recap of how the Hydra project evolved
  over the last two years. The whole timeline can bee seen below, and
  this also sparked the idea of creating a "Year in review" blog post,
  which is currently in preparation.

- **Retrospective**: In-person workshops are the perfect place for
  doing retrospectives to reflect on what was good, bad and collecting
  ideas & actions in how to improve our work environment and
  processes.

  ![](./img/2022-11-retro.png)
  <small><center>
  Retrospective board from 2022-11-22
  </center></small>

- **Impact map:** Conscious about the fact that the project got defined about one year ago in a first project plan, and inspired by timeline and (short-term) ideas, we also set off to reflect on the **Why**, **How** and **What** of this project.

  After reviewing the project vision (TODO: link) , we reached for the tool of our choice to come up with tangible objective / deliverables - impact maps! Also, we felt the need to use a different goal this time. One that is reflecting more closely (or, in fact, broadly) what our current mission is. So instead of the **99% of Cardano transactions are done through Hydra**, we set a new goal: being the **number one DApp on Cardano** (by all known metrics: TVL, traffic, volume etc..) Fundamentally, both goals illustrate the same idea, but the latter better supports the narrative that Hydra is also just a DApp (not a network upgrade) and needs usage & adoption to reach our vision.

  Below you see the result of our session. We checked back to the old impact map after creating this one, and many things are still relevant / similar on the new sample (it's never complete!) - some of the deliverables we even achieved.

  ![](./img/2022-11-impact.png)
  <small><center>
  Impact map with new goal
  </center></small>

# Conclusion

Reflecting on the starmap has been a bit depressing as we are definitely behind our anticipated schedule. However, recent developments on the lighthouse projects and random people contributing & using Hydra are convincing us that we are on the right track. The summit paints a similar picture and we feel reinvigorated by the great people we met, discussions we had and relationships we created & solidified on the Summit and during our Hydra workshop.

We see & want to stress the importance of this project for the scalability story of Cardano, but will also make sure to work on clarifying the project vision for Hydra. This will include challenges & risks, outline opportunities for products and explain benefits for Cardano-the-network-itself when we continue our journey with Hydra.
