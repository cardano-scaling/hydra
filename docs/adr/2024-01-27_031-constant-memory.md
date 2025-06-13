---
slug: 31
title: |
  31. Achieve constant memory in hydra-node
authors: [v0d1ch]
tags: [Accepted]
---

## Status

Accepted

## Context

When testing out hydra-node operation under heavy or increased load we are
noticing that memory consumption is far from ideal. So far we didn't bother
thinking about the performance so much but time has come to try and reduce
memory footprint of a running hydra-node.

There are some quick points to be scored here since our projections that are
used to serve in-memory data are using a common haskell list as a data
structure. We should stream the data keeping the memory bounded as the first
optimisation.

It is also not necessary to output the whole history of messages by default and
only do that if clients request to see the whole history. Internally our
`ServerOutput` type could be remapped to `StateChanged` since the two are
almost identical. Any new information must be streamed to the clients
automatically.

## Decision

- Re-map `ServerOutput` to `StateChanged` by adding any missing constructor to `StateChanged` (eg. `PeerConnected`).
- Output new client messages on `newState` changes instead of using `ClientEffect`.
- Use `StateChanged` in all projections we server from the API (re-use `eventId` as sequence number).
- Make hydra-node output history of messages only on demand (breaking change is to be communicated in the changelog).
- Use `conduit` library to achieve constant memory by streaming the data in our projections.

## Consequences

This should lead to much better performance of hydra-node in terms of used
memory for the running process. This should be also confirmed by running the
relevant [benchmarks](https://github.com/cardano-scaling/hydra/issues/1724) and
do a test (even manual or a script) to assert that the memory consumption is
actually reduced.
