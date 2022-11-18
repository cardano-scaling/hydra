---
slug: 16
title: |
  16. Keep Rejected ADRs
authors: []
tags: [Accepted]
---

## Status

Accepted

## Context

We have started using _Architecture Decision Records_ as our primary way to document the most important design decisions we take while developing Hydra Node, and this has proved effective in fostering fruitful discussions about major architecture changes.

During the course of this project, we have sometimes had debates on various topics leading to rejection of [some ADRs](https://github.com/input-output-hk/hydra/pull/230). It could be the case that  a previously rejected proposal turns out to be interesting, either because the context and situation have changed enough to reevaluate a proposal, or as background for some new proposal.

## Decision

_therefore_

* We will keep rejected _Architecture Decision Records_ alongside accepted and draft ones, in the same location and format
* Rejected ADRs _must_ have tag `[Rejected]` set

## Consequences

Once attributed a _serial number_ an ADR keeps it "forever", whether it's rejected or accepted
