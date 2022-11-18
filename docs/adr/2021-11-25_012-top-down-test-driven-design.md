---
slug: 12
title: | 
  12. Top-down Test-driven Design
authors: []
tags: [Accepted]
---

## Status

Accepted

## Context

* [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development) or _Test-Driven Design_ is a technique that helps team promotes simple and loosely coupled design, reduces the amount of code written, increases confidence in delivered software by providing a high level of code coverage by regression tests, and improves development speed through shorter feedback loop
* While initially focused on _unit tests_, TDD has evolved over time to include higher-level tests like [Behaviour Driven Development](https://en.wikipedia.org/wiki/Behavior-driven_development) or [Specification by Example](https://en.wikipedia.org/wiki/Specification_by_example), leading to comprehensive strategies like the [Outside-In Diamond TDD](http://tpierrain.blogspot.com/2021/03/outside-in-diamond-tdd-1-style-made.html)
* Being a foundational part of scalable applications based on Cardano blockchain, Hydra Head needs to be released early, often, and with high assurance in order to benefit from early adopters' feedback

## Decision

_Therefore_

We start as early as possible with _End-to-End_ tests, gradually making them more complex as we develop the various components but starting with something simple (like a system-level but dummy chain and hydra network).

We flesh out other integration tests as needed, when we refine the technological stack used for the various bits and pieces.

We do most of our work in the _Executable Specifications_ layer while we are developing the core domain functions, eg. the Head protocol. The rationale being this is the level at which we can test the most complex behaviours in the fastest and safest possible way as we everything runs without external dependencies or can even run as pure code using io-sim.

We tactically drop to _Unit tests_ level when dealing with the protocol's "fine prints".

## Consequences

* Development of each "feature", whether new or change to existing one, should start with a test defined at the highest level possible, but no higher
* A detailed presentation of the various testing layers is available in the [wiki](https://github.com/input-output-hk/hydra/wiki/Testing-Strategy)
