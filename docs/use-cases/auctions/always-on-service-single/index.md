---
sidebar_label: 'Auctions-as-a-service: single'
sidebar_position: 4
---

# Always-on delegated auction service (single head)

The always-on delegated auction is a persistent service for DApps that runs multiple auctions on layer 2, using a single Hydra head. This enables an as-a-service business model.

:::note Application design

This page describes a proposed service, not a release-tested integration.
Deposits and decommits allow a head to remain open across auctions, but the
auction contracts and service still need their own implementation and audit.

:::

## Design requirements

- Define how auctions isolate their assets and state within one head.
- Wait for deposit finalization before accepting a bid backed by those funds.
- Settle completed auctions through decommits, with a close/fanout fallback.
- Retain signed results and explain what bidders can do if operators censor
  requests or stop signing.
- Keep every output compatible with L1 and stay within the head's
  [size limits](/docs/known-issues).

A signed snapshot proves the operators agreed to a state. The auction
validator and recovery protocol must establish that this state satisfies the
auction's rules; signatures alone do not provide a remedy for every failure.
