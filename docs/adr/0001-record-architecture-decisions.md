# 1. Record architecture decisions

Date: 2021-06-07

## Status

Accepted

## Context

We are in search for a means to describe our technical architecture.

We are a small team working in a very lean and agile way (XP), so we naturally
prefer also light-weight documentation methods which also accomodate change
easily.

## Decision

* We will use _Architecture Decision Records_, as described by Michael Nygard in
  this
  [article](http://thinkrelevance.com/blog/2011/11/15/documenting-architecture-decisions).
* We will follow the convention of storing those ADRs as Markdown formatted
  documents stored under `docs/adr` directory, as exemplified in Nat Pryce's
  [adr-tools](https://github.com/npryce/adr-tools). This does not imply we will
  be using `adr-tools` itself.

## Consequences

See Michael Nygard's article, linked above.
